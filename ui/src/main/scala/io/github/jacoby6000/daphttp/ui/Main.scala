package io.github.jacoby6000.daphttp.ui

import io.circe.Decoder
import io.circe.Json
import io.circe.parser.parse
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.MouseEvent

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.Thenable.Implicits._
import scala.util.Failure
import scala.util.Success

final case class RouteTreeNode(
    path: String,
    kind: String,
    fetchable: Boolean,
    member: Option[String],
    index: Option[Int],
    arrayLength: Option[Int],
    children: List[RouteTreeNode]
)

final case class RoutesResponse(
    routes: List[String],
    tree: List[RouteTreeNode],
    errors: List[String]
)

object RouteTreeNode {
  implicit val decoder: Decoder[RouteTreeNode] = Decoder.instance { c =>
    for {
      path <- c.get[String]("path")
      kind <- c.get[String]("kind")
      fetchable <- c.get[Boolean]("fetchable")
      member <- c.get[Option[String]]("member")
      index <- c.get[Option[Int]]("index")
      arrayLength <- c.get[Option[Int]]("arrayLength")
      children <- c.get[List[RouteTreeNode]]("children")
    } yield RouteTreeNode(path, kind, fetchable, member, index, arrayLength, children)
  }
}

object RoutesResponse {
  implicit val decoder: Decoder[RoutesResponse] = Decoder.instance { c =>
    for {
      routes <- c.get[List[String]]("routes")
      tree <- c.get[List[RouteTreeNode]]("tree")
      errors <- c.get[List[String]]("errors")
    } yield RoutesResponse(routes, tree, errors)
  }
}

object Main {
  private var tree: List[RouteTreeNode] = Nil
  private val expanded = scala.collection.mutable.Set.empty[String]
  private val payloads = scala.collection.mutable.Map.empty[String, Json]
  private val loading = scala.collection.mutable.Set.empty[String]
  private val loadErrors = scala.collection.mutable.Map.empty[String, String]
  private var selected: Option[String] = None

  def main(@unused args: Array[String]): Unit = {
    byId("reload-tree").onclick = (_: MouseEvent) => loadTree()
    byId("expand-all").onclick = (_: MouseEvent) => {
      collectPaths(tree).foreach(expanded.add)
      renderTree()
    }
    byId("collapse-all").onclick = (_: MouseEvent) => {
      expanded.clear()
      renderTree()
    }
    byId("refresh-visible").onclick = (_: MouseEvent) => refreshExpanded()
    loadTree()
  }

  private def loadTree(): Unit = {
    setBanner(None)
    fetchJson("/routes").onComplete {
      case Success(json) =>
        json.as[RoutesResponse] match {
          case Right(response) =>
            tree = response.tree
            if (expanded.isEmpty) tree.foreach(n => expanded.add(n.path))
            byId("route-count").textContent = s"${response.routes.size} routes"
            if (response.errors.nonEmpty)
              setBanner(Some(s"IR warnings/errors: ${response.errors.mkString("; ")}"))
            renderTree()
          case Left(err) =>
            setBanner(Some(s"Failed to decode /routes: ${err.getMessage}"))
        }
      case Failure(err) =>
        setBanner(Some(s"Failed to load /routes: ${err.getMessage}"))
    }
  }

  private def refreshExpanded(): Unit = {
    val paths = collectNodes(tree).collect {
      case n if n.fetchable && (expanded.contains(n.path) || n.children.isEmpty) =>
        n.path
    }.distinct
    paths.foreach(refreshPath)
  }

  private def refreshPath(path: String): Unit = {
    loading.add(path)
    loadErrors.remove(path)
    renderTree()
    fetchJson(path).onComplete {
      case Success(json) =>
        loading.remove(path)
        payloads.update(path, json)
        selected = Some(path)
        renderTree()
        showDetail(path, json)
      case Failure(err) =>
        loading.remove(path)
        loadErrors.update(path, err.getMessage)
        selected = Some(path)
        renderTree()
        showErrorDetail(path, err.getMessage)
    }
  }

  private def refreshNode(node: RouteTreeNode): Unit =
    if (node.fetchable) refreshPath(node.path)
    else node.children.foreach(refreshNode)

  private def renderTree(): Unit = {
    val root = byId("tree-root")
    root.innerHTML = ""
    root.className = ""
    val ul = el("ul")
    ul.className = "tree"
    tree.foreach(n => ul.appendChild(renderNode(n)))
    root.appendChild(ul)
  }

  private def renderNode(node: RouteTreeNode): HTMLElement = {
    val li = el("li")
    val row = el("div")
    val selectedClass = if (selected.contains(node.path)) " selected" else ""
    row.className = "node" + selectedClass

    val twist = el("span")
    twist.className = if (node.children.isEmpty) "twist empty" else "twist"
    val isOpen = expanded.contains(node.path)
    twist.textContent = if (node.children.isEmpty) "·" else if (isOpen) "▼" else "▶"
    if (node.children.nonEmpty) {
      twist.onclick = { (e: MouseEvent) =>
        e.stopPropagation()
        if (isOpen) expanded.remove(node.path) else expanded.add(node.path)
        renderTree()
      }
    }

    val label = el("span")
    label.className = "label"
    label.textContent = shortenPath(node.path)
    val kind = el("span")
    kind.className = "kind"
    kind.textContent = node.kind
    label.appendChild(kind)
    statusBadge(node.path).foreach(label.appendChild)
    label.onclick = { (_: MouseEvent) =>
      selected = Some(node.path)
      payloads.get(node.path) match {
        case Some(json) => showDetail(node.path, json)
        case None       =>
          loadErrors.get(node.path) match {
            case Some(msg) => showErrorDetail(node.path, msg)
            case None      => showPlaceholder(node.path)
          }
      }
      renderTree()
    }

    val refresh = el("button")
    refresh.className = "ghost"
    refresh.textContent = "↻"
    refresh.title =
      if (node.fetchable) s"Refresh ${node.path}"
      else "Refresh children"
    refresh.onclick = { (e: MouseEvent) =>
      e.stopPropagation()
      refreshNode(node)
    }
    if (loading.contains(node.path)) refresh.setAttribute("disabled", "true")

    row.appendChild(twist)
    row.appendChild(label)
    row.appendChild(refresh)
    li.appendChild(row)

    if (isOpen && node.children.nonEmpty) {
      val childUl = el("ul")
      node.children.foreach(c => childUl.appendChild(renderNode(c)))
      li.appendChild(childUl)
    }
    li
  }

  private def statusBadge(path: String): Option[HTMLElement] =
    if (loading.contains(path)) {
      val st = el("span")
      st.className = "kind status-loading"
      st.textContent = " loading"
      Some(st)
    } else if (loadErrors.contains(path)) {
      val st = el("span")
      st.className = "kind status-err"
      st.textContent = " error"
      Some(st)
    } else if (payloads.contains(path)) {
      val st = el("span")
      st.className = "kind status-ok"
      st.textContent = " loaded"
      Some(st)
    } else None

  private def showDetail(path: String, json: Json): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    empty.setAttribute("hidden", "true")
    body.removeAttribute("hidden")
    val decoded = json.hcursor
      .downField("decoded")
      .focus
      .orElse(json.hcursor.downField("reads").downArray.downField("decoded").focus)
      .getOrElse(json)
    body.textContent = s"$path\n\n${decoded.spaces2}"
  }

  private def showErrorDetail(path: String, message: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    empty.setAttribute("hidden", "true")
    body.removeAttribute("hidden")
    body.textContent = s"$path\n\nError: $message"
  }

  private def showPlaceholder(path: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    body.setAttribute("hidden", "true")
    empty.removeAttribute("hidden")
    empty.textContent = s"$path — press ↻ to fetch."
  }

  private def setBanner(message: Option[String]): Unit = {
    val banner = byId("error-banner")
    message match {
      case None =>
        banner.className = ""
        banner.textContent = ""
      case Some(text) =>
        banner.className = "visible"
        banner.textContent = text
    }
  }

  private def shortenPath(path: String): String =
    if (path.startsWith("/api/")) path.stripPrefix("/api/") else path

  private def collectPaths(nodes: List[RouteTreeNode]): List[String] =
    nodes.flatMap(n => n.path :: collectPaths(n.children))

  private def collectNodes(nodes: List[RouteTreeNode]): List[RouteTreeNode] =
    nodes.flatMap(n => n :: collectNodes(n.children))

  private def byId(id: String): HTMLElement =
    dom.document.getElementById(id).asInstanceOf[HTMLElement]

  private def el(tag: String): HTMLElement =
    dom.document.createElement(tag).asInstanceOf[HTMLElement]

  private def fetchJson(path: String): Future[Json] =
    dom.fetch(path).toFuture.flatMap { response =>
      response.text().toFuture.map { text =>
        if (!response.ok)
          throw new RuntimeException(s"HTTP ${response.status}: $text")
        parse(text) match {
          case Right(json) => json
          case Left(err)   => throw new RuntimeException(err.message)
        }
      }
    }
}
