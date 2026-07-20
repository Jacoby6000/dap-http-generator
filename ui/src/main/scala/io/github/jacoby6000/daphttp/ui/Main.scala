package io.github.jacoby6000.daphttp.ui

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.RequestInit
import org.scalajs.dom.HttpMethod

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

final case class RouteTreeNode(
    path: String,
    kind: String,
    fetchable: Boolean,
    member: Option[String],
    index: Option[Int],
    arrayLength: Option[Int],
    address: Option[Long],
    children: List[RouteTreeNode]
)

final case class RoutesResponse(
    routes: List[String],
    tree: List[RouteTreeNode],
    errors: List[String]
)

final case class TypeCatalogEntry(
    id: String,
    kind: String,
    members: Option[List[String]],
    fields: Option[List[OverlayMemberUi]]
)

final case class OverlayMemberUi(
    name: String,
    typeId: String,
    isArray: Boolean,
    arrayLength: Option[Int],
    isPointer: Boolean
)

final case class OverlayStructDefUi(members: List[OverlayMemberUi])

final case class OverlayNewStructUi(id: String, members: List[OverlayMemberUi])

final case class TypeOverlayDocumentUi(
    structs: Map[String, OverlayStructDefUi],
    newStructs: List[OverlayNewStructUi]
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
      address <- c
        .get[Option[String]]("address")
        .map(_.flatMap(parseAddress))
      children <- c.get[List[RouteTreeNode]]("children")
    } yield RouteTreeNode(
      path,
      kind,
      fetchable,
      member,
      index,
      arrayLength,
      address,
      children
    )
  }

  private def parseAddress(raw: String): Option[Long] = {
    val trimmed = raw.trim.toLowerCase
    val hex =
      if (trimmed.startsWith("0x")) trimmed.drop(2)
      else trimmed
    Try(java.lang.Long.parseUnsignedLong(hex, 16)).toOption
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

object TypeCatalogEntry {
  implicit val decoder: Decoder[TypeCatalogEntry] = Decoder.instance { c =>
    for {
      id <- c.get[String]("id")
      kind <- c.get[String]("kind")
      members <- c.get[Option[List[String]]]("members")
      fields <- c.get[Option[List[OverlayMemberUi]]]("fields")
    } yield TypeCatalogEntry(id, kind, members, fields)
  }
}

object OverlayMemberUi {
  implicit val decoder: Decoder[OverlayMemberUi] = Decoder.instance { c =>
    for {
      name <- c.get[String]("name")
      typeId <- c.get[String]("typeId")
      isArray <- c.getOrElse[Boolean]("isArray")(false)
      arrayLength <- c.get[Option[Int]]("arrayLength")
      isPointer <- c.getOrElse[Boolean]("isPointer")(false)
    } yield OverlayMemberUi(name, typeId, isArray, arrayLength, isPointer)
  }

  implicit val encoder: Encoder[OverlayMemberUi] = Encoder.instance { m =>
    Json.obj(
      "name" -> Json.fromString(m.name),
      "typeId" -> Json.fromString(m.typeId),
      "isArray" -> Json.fromBoolean(m.isArray),
      "arrayLength" -> m.arrayLength.fold(Json.Null)(Json.fromInt),
      "isPointer" -> Json.fromBoolean(m.isPointer)
    )
  }
}

object OverlayStructDefUi {
  implicit val decoder: Decoder[OverlayStructDefUi] =
    Decoder.instance(_.get[List[OverlayMemberUi]]("members").map(OverlayStructDefUi.apply))
  implicit val encoder: Encoder[OverlayStructDefUi] =
    Encoder.instance(d => Json.obj("members" -> d.members.asJson))
}

object OverlayNewStructUi {
  implicit val decoder: Decoder[OverlayNewStructUi] = Decoder.instance { c =>
    for {
      id <- c.get[String]("id")
      members <- c.get[List[OverlayMemberUi]]("members")
    } yield OverlayNewStructUi(id, members)
  }
  implicit val encoder: Encoder[OverlayNewStructUi] = Encoder.instance { s =>
    Json.obj("id" -> Json.fromString(s.id), "members" -> s.members.asJson)
  }
}

object TypeOverlayDocumentUi {
  val empty: TypeOverlayDocumentUi = TypeOverlayDocumentUi(Map.empty, Nil)

  implicit val decoder: Decoder[TypeOverlayDocumentUi] = Decoder.instance { c =>
    for {
      structs <- c.getOrElse[Map[String, OverlayStructDefUi]]("structs")(Map.empty)
      newStructs <- c.getOrElse[List[OverlayNewStructUi]]("newStructs")(Nil)
    } yield TypeOverlayDocumentUi(structs, newStructs)
  }

  implicit val encoder: Encoder[TypeOverlayDocumentUi] = Encoder.instance { d =>
    Json.obj(
      "structs" -> d.structs.asJson,
      "newStructs" -> d.newStructs.asJson
    )
  }
}

object Main {
  private var catalog: List[RouteTreeNode] = Nil
  private var visible: List[RouteTreeNode] = Nil
  private val expanded = scala.collection.mutable.Set.empty[String]
  private val payloads = scala.collection.mutable.Map.empty[String, Json]
  private val loading = scala.collection.mutable.Set.empty[String]
  private val loadErrors = scala.collection.mutable.Map.empty[String, String]
  private var selected: Option[String] = None
  private var activeQuery: String = ""
  private var typeCatalog: List[TypeCatalogEntry] = Nil
  private var overlays: TypeOverlayDocumentUi = TypeOverlayDocumentUi.empty
  private var editingStructId: Option[String] = None
  private var draftMembers: List[OverlayMemberUi] = Nil
  private var lastDecodeType: Option[String] = None
  private var editorOpen: Boolean = false

  /** Global counter bumped on every successful memory refresh (full or per-field). */
  private var refreshCount: Int = 0

  /** Json-pointer path → `refreshCount` when that subtree was last refreshed. */
  private val fieldFreshAt = scala.collection.mutable.Map.empty[String, Int]

  private val fieldLoading = scala.collection.mutable.Set.empty[String]
  private var detailViewPath: Option[String] = None
  private var cachedFetchablePaths: Set[String] = Set.empty

  /** Expanded JSON composite paths (`stampKey`) — collapsed nodes are not built in the DOM. */
  private val jsonExpanded = scala.collection.mutable.Set.empty[String]

  private val MaxRefreshAge = 10

  /** Template path containing `{index}` slots currently being browsed. */
  private var indexTemplate: Option[String] = None

  /** Current index values for `indexTemplate` (or concrete indexed path). */
  private var indexValues: List[Int] = Nil

  def main(@unused args: Array[String]): Unit = {
    byId("reload-tree").onclick = (_: MouseEvent) => loadCatalog()
    byId("clear-results").onclick = (_: MouseEvent) => {
      activeQuery = ""
      searchInput().value = ""
      visible = Nil
      expanded.clear()
      selected = None
      clearIndexBrowse()
      renderResults()
      showIdleDetail()
    }
    byId("refresh-visible").onclick = (_: MouseEvent) => refreshVisible()
    val input = searchInput()
    input.onkeydown = { (e: KeyboardEvent) =>
      if (e.key == "Enter") {
        e.preventDefault()
        runSearch(input.value)
      }
    }
    byId("search-submit").onclick = (_: MouseEvent) => runSearch(searchInput().value)
    byId("editor-toggle").onclick = (_: MouseEvent) => setEditorOpen(!editorOpen)
    byId("overlay-apply").onclick = (_: MouseEvent) => applyOverlay()
    byId("overlay-reset").onclick = (_: MouseEvent) => resetCurrentStruct()
    byId("overlay-add-field").onclick = (_: MouseEvent) => {
      syncDraftFromDom()
      draftMembers = draftMembers :+ OverlayMemberUi("field", "u8", isArray = false, None, false)
      renderFieldEditor()
    }
    byId("create-struct").onclick = (_: MouseEvent) => createNewStruct()
    byId("edit-struct-load").onclick = (_: MouseEvent) => {
      val id = inputById("edit-struct").value.trim
      if (id.nonEmpty) loadDraftForStruct(id)
    }
    inputById("edit-struct").onchange = (_: dom.Event) => {
      val id = inputById("edit-struct").value.trim
      if (id.nonEmpty) loadDraftForStruct(id)
    }
    byId("index-fetch").onclick = (_: MouseEvent) => fetchIndexed(syncIndexInputsFromDom())
    byId("index-prev").onclick = (_: MouseEvent) => stepIndex(-1)
    byId("index-next").onclick = (_: MouseEvent) => stepIndex(1)
    loadCatalog()
    loadTypesAndOverlays()
  }

  private def loadTypesAndOverlays(): Unit = {
    fetchJson("/types").foreach { json =>
      json.hcursor.downField("types").as[List[TypeCatalogEntry]].foreach { entries =>
        typeCatalog = entries
        renderTypeDatalists()
      }
    }
    fetchJson("/overlays").foreach { json =>
      json.as[TypeOverlayDocumentUi].foreach { doc =>
        overlays = doc
        editingStructId.foreach(id => loadDraftForStruct(id))
      }
    }
  }

  private def loadCatalog(): Unit = {
    setBanner(None)
    byId("tree-root").textContent = "Loading route catalog…"
    fetchJson("/routes").onComplete {
      case Success(json) =>
        json.as[RoutesResponse] match {
          case Right(response) =>
            catalog = response.tree
            cachedFetchablePaths = collectNodes(catalog).collect {
              case n if n.fetchable => n.path
            }.toSet
            byId("route-count").textContent = s"${response.routes.size} routes"
            if (response.errors.nonEmpty)
              setBanner(Some(s"IR warnings/errors: ${response.errors.mkString("; ")}"))
            if (activeQuery.nonEmpty) runSearch(activeQuery)
            else {
              visible = Nil
              renderResults()
              showIdleDetail()
            }
          case Left(err) =>
            setBanner(Some(s"Failed to decode /routes: ${err.getMessage}"))
            byId("tree-root").textContent = "Failed to load routes."
        }
      case Failure(err) =>
        setBanner(Some(s"Failed to load /routes: ${err.getMessage}"))
        byId("tree-root").textContent = "Failed to load routes."
    }
  }

  private def runSearch(rawQuery: String): Unit = {
    val query = rawQuery.trim
    activeQuery = query
    searchInput().value = query
    if (query.isEmpty) {
      visible = Nil
      expanded.clear()
      renderResults()
      showIdleDetail()
    } else {
      visible = filterCatalog(catalog, query)
      expanded.clear()
      visible.foreach(n => expanded.add(n.path))
      renderResults()
      if (visible.isEmpty) {
        byId("detail-empty").removeAttribute("hidden")
        byId("detail-body").setAttribute("hidden", "true")
        byId("detail-empty").textContent = s"No routes matched “$query”."
      }
    }
  }

  private def filterCatalog(nodes: List[RouteTreeNode], query: String): List[RouteTreeNode] = {
    val trimmed = query.trim
    if (trimmed.toLowerCase.startsWith("0x")) {
      parseHexAddress(trimmed) match {
        case None =>
          setBanner(Some(s"Invalid address query: $trimmed"))
          Nil
        case Some(address) =>
          setBanner(None)
          nodes.flatMap(filterByAddress(_, address))
      }
    } else {
      setBanner(None)
      val needle = trimmed.toLowerCase
      nodes.flatMap(filterByName(_, needle))
    }
  }

  private def filterByName(node: RouteTreeNode, needle: String): Option[RouteTreeNode] = {
    val selfMatch = nameMatches(node, needle)
    val childMatches = node.children.flatMap(filterByName(_, needle))
    if (selfMatch) Some(node)
    else if (childMatches.nonEmpty) Some(node.copy(children = childMatches))
    else None
  }

  private def filterByAddress(node: RouteTreeNode, address: Long): Option[RouteTreeNode] = {
    val selfMatch = node.address.contains(address)
    val childMatches = node.children.flatMap(filterByAddress(_, address))
    if (selfMatch && childMatches.isEmpty) Some(node.copy(children = Nil))
    else if (selfMatch) Some(node.copy(children = childMatches))
    else if (childMatches.nonEmpty) Some(node.copy(children = childMatches))
    else None
  }

  private def nameMatches(node: RouteTreeNode, needle: String): Boolean = {
    val path = shortenPath(node.path).toLowerCase
    val member = node.member.map(_.toLowerCase)
    path.contains(needle) ||
    member.exists(_.contains(needle)) ||
    path.split('/').exists(_.contains(needle))
  }

  private def parseHexAddress(raw: String): Option[Long] = {
    val hex = raw.trim.toLowerCase.stripPrefix("0x")
    if (hex.isEmpty || !hex.forall(c => c.isDigit || (c >= 'a' && c <= 'f'))) None
    else Try(java.lang.Long.parseUnsignedLong(hex, 16)).toOption
  }

  private def refreshVisible(): Unit = {
    val paths = collectNodes(visible).collect {
      case n if n.fetchable && (expanded.contains(n.path) || n.children.isEmpty) =>
        n.path
    }.distinct
    paths.foreach(refreshPath)
  }

  private def refreshPath(path: String): Unit = {
    loading.add(path)
    loadErrors.remove(path)
    renderResults()
    setIndexStatus(s"Fetching $path…", ok = true)
    fetchJson(path).onComplete {
      case Success(json) =>
        loading.remove(path)
        if (decodeFailed(json)) {
          loadErrors.update(path, decodeErrorMessage(json))
          selected = Some(path)
          renderResults()
          showErrorDetail(path, decodeErrorMessage(json))
          setIndexStatus(s"End of chain / decode failed at $path", ok = false)
        } else {
          payloads.update(path, json)
          selected = Some(path)
          bumpRefreshFreshness(path, extractDecoded(json), extractOverlayDecoded(json))
          renderResults()
          showDetail(path, json)
          setIndexStatus(s"Loaded $path", ok = true)
        }
      case Failure(err) =>
        loading.remove(path)
        loadErrors.update(path, err.getMessage)
        selected = Some(path)
        renderResults()
        showErrorDetail(path, err.getMessage)
        setIndexStatus(err.getMessage, ok = false)
    }
  }

  private def refreshNode(node: RouteTreeNode): Unit =
    resolveIndexBrowse(node.path) match {
      case Some((template, indices)) =>
        indexTemplate = Some(template)
        indexValues = indices
        renderIndexBar()
        if (node.path.contains("{index}")) {
          // Template: fetch with current (default) indices.
          fetchIndexed(indices)
        } else if (node.fetchable) {
          refreshPath(node.path)
        } else {
          fetchIndexed(indices)
        }
      case None =>
        clearIndexBrowse()
        if (node.fetchable) refreshPath(node.path)
        else node.children.foreach(refreshNode)
    }

  private def renderResults(): Unit = {
    val root = byId("tree-root")
    root.innerHTML = ""
    root.className = ""
    if (catalog.isEmpty) {
      root.className = "empty"
      root.textContent = "Loading route catalog…"
    } else if (activeQuery.isEmpty) {
      root.className = "empty"
      root.textContent =
        "Search for a symbol, struct, or field name. Prefix with 0x to find a symbol by address."
    } else if (visible.isEmpty) {
      root.className = "empty"
      root.textContent = s"No matches for “$activeQuery”."
    } else {
      val hint = el("div")
      hint.className = "results-meta"
      hint.textContent = s"${visible.size} result root(s) for “$activeQuery”"
      root.appendChild(hint)
      val ul = el("ul")
      ul.className = "tree"
      visible.foreach(n => ul.appendChild(renderNode(n)))
      root.appendChild(ul)
    }
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
        renderResults()
      }
    }

    val label = el("span")
    label.className = "label"
    label.textContent = shortenPath(node.path)
    node.address.foreach { addr =>
      val addrEl = el("span")
      addrEl.className = "kind address"
      addrEl.textContent = f" 0x$addr%x"
      label.appendChild(addrEl)
    }
    val kind = el("span")
    kind.className = "kind"
    kind.textContent = node.kind
    label.appendChild(kind)
    statusBadge(node.path).foreach(label.appendChild)
    label.onclick = { (_: MouseEvent) =>
      selected = Some(node.path)
      resolveIndexBrowse(node.path) match {
        case Some((template, indices)) =>
          indexTemplate = Some(template)
          indexValues = indices
          renderIndexBar()
        case None =>
          clearIndexBrowse()
      }
      payloads.get(concretePathForSelection(node.path)) match {
        case Some(json) => showDetail(concretePathForSelection(node.path), json)
        case None       =>
          loadErrors.get(concretePathForSelection(node.path)) match {
            case Some(msg) => showErrorDetail(concretePathForSelection(node.path), msg)
            case None      =>
              if (node.path.contains("{index}")) showIndexPrompt(node.path)
              else showPlaceholder(node.path)
          }
      }
      renderResults()
    }

    val refresh = el("button")
    refresh.className = "ghost"
    refresh.textContent = "↻"
    refresh.title =
      if (node.path.contains("{index}")) s"Fetch ${node.path} with indices"
      else if (node.fetchable) s"Refresh ${node.path}"
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
    byId("editor-panel").removeAttribute("hidden")
    setEditorOpen(editorOpen)

    resolveIndexBrowse(path).foreach { case (template, indices) =>
      indexTemplate = Some(template)
      indexValues = indices
      renderIndexBar()
    }

    val readCursor = json.hcursor
      .downField("reads")
      .downArray
      .success
      .map(_.value.hcursor)
      .getOrElse(json.hcursor)
    val decoded = readCursor
      .downField("decoded")
      .focus
      .orElse(json.hcursor.downField("decoded").focus)
      .getOrElse(json)
    val overlayDecoded = readCursor
      .downField("overlayDecoded")
      .focus
      .orElse(json.hcursor.downField("overlayDecoded").focus)
    lastDecodeType = readCursor
      .get[String]("decodeType")
      .toOption
      .orElse(json.hcursor.get[String]("decodeType").toOption)

    byId("detail-path").textContent = path
    detailViewPath = Some(path)
    // Cached payload with no stamps for this view yet: treat as fresh without bumping.
    if (!fieldFreshAt.contains(stampKey(path, Nil))) {
      fieldFreshAt.update(stampKey(path, Nil), refreshCount)
    }
    setJsonView("detail-decoded", decoded, rootOpen = true, basePath = path)
    overlayDecoded match {
      case Some(overlay) => setJsonView("detail-overlay", overlay, rootOpen = true, basePath = path)
      case None          => setPlainView("detail-overlay", "No overlay applied for this type.")
    }

    lastDecodeType.foreach { id =>
      editingStructId = Some(id)
      inputById("edit-struct").value = id
      loadDraftForStruct(id)
    }
  }

  private def showErrorDetail(path: String, message: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    empty.setAttribute("hidden", "true")
    body.removeAttribute("hidden")
    byId("editor-panel").setAttribute("hidden", "true")
    resolveIndexBrowse(path).foreach { case (template, indices) =>
      indexTemplate = Some(template)
      indexValues = indices
      renderIndexBar()
    }
    byId("detail-path").textContent = path
    detailViewPath = Some(path)
    setPlainView("detail-decoded", s"Error: $message", error = true)
    setPlainView("detail-overlay", "")
  }

  private def showPlaceholder(path: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    body.setAttribute("hidden", "true")
    empty.removeAttribute("hidden")
    empty.textContent = s"$path — press ↻ to fetch."
  }

  private def showIndexPrompt(path: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    empty.setAttribute("hidden", "true")
    body.removeAttribute("hidden")
    byId("editor-panel").setAttribute("hidden", "true")
    byId("detail-path").textContent = path
    detailViewPath = Some(path)
    setPlainView(
      "detail-decoded",
      "Set index values below, then Fetch (or press ↻). Use Prev/Next to walk the chain."
    )
    setPlainView("detail-overlay", "")
    renderIndexBar()
  }

  private def setPlainView(id: String, text: String, error: Boolean = false): Unit = {
    val host = byId(id)
    host.innerHTML = ""
    host.className = if (error) "json-view plain error" else "json-view plain"
    host.textContent = text
  }

  private def setJsonView(id: String, json: Json, rootOpen: Boolean, basePath: String): Unit = {
    val host = byId(id)
    host.innerHTML = ""
    host.className = "json-view"
    host.appendChild(
      renderJsonNode(
        json,
        forceOpen = rootOpen,
        key = None,
        basePath = basePath,
        segments = Nil,
        fetchable = cachedFetchablePaths
      )
    )
  }

  private def repaintJsonViews(basePath: String): Unit =
    payloads.get(basePath).foreach { payload =>
      val decoded = extractDecoded(payload)
      setJsonView("detail-decoded", decoded, rootOpen = true, basePath = basePath)
      extractOverlayDecoded(payload) match {
        case Some(overlay) =>
          setJsonView("detail-overlay", overlay, rootOpen = true, basePath = basePath)
        case None =>
          setPlainView("detail-overlay", "No overlay applied for this type.")
      }
    }

  private def renderJsonNode(
      json: Json,
      forceOpen: Boolean,
      key: Option[HTMLElement],
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): HTMLElement =
    json.arrayOrObject(
      or = {
        val row = el("div")
        row.className = "jv-line"
        row.setAttribute("data-age", refreshAge(basePath, segments).toString)
        val pad = el("span")
        pad.className = "jv-twist empty"
        pad.textContent = "·"
        row.appendChild(pad)
        key.foreach(row.appendChild)
        val leaf = el("span")
        leaf.className = jsonPrimitiveClass(json)
        leaf.textContent = jsonPrimitiveText(json)
        row.appendChild(leaf)
        appendFieldRefresh(row, basePath, segments, fetchable)
        row
      },
      jsonArray = arr =>
        renderJsonComposite(
          forceOpen = forceOpen,
          openPunct = "[",
          closePunct = "]",
          preview = if (arr.isEmpty) "[]" else s"[${arr.size}]",
          key = key,
          basePath = basePath,
          segments = segments,
          fetchable = fetchable,
          children = arr.toList.zipWithIndex.map { case (value, index) =>
            (index.toString, value, segments :+ index.toString)
          }
        ),
      jsonObject = obj =>
        renderJsonComposite(
          forceOpen = forceOpen,
          openPunct = "{",
          closePunct = "}",
          preview = if (obj.isEmpty) "{}" else s"{${obj.size}}",
          key = key,
          basePath = basePath,
          segments = segments,
          fetchable = fetchable,
          children = obj.toList.map { case (name, value) =>
            (name, value, segments :+ name)
          }
        )
    )

  private def renderJsonComposite(
      forceOpen: Boolean,
      openPunct: String,
      closePunct: String,
      preview: String,
      key: Option[HTMLElement],
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      children: List[(String, Json, List[String])]
  ): HTMLElement = {
    val pathKey = stampKey(basePath, segments)
    val isOpen = forceOpen || jsonExpanded.contains(pathKey)
    val root = el("div")
    root.className = if (isOpen) "jv-composite" else "jv-composite collapsed"

    val line = el("div")
    line.className = "jv-line"
    line.setAttribute("data-age", refreshAge(basePath, segments).toString)

    val twist = el("span")
    twist.className = if (children.isEmpty) "jv-twist empty" else "jv-twist"
    twist.textContent =
      if (children.isEmpty) "·" else if (isOpen) "▼" else "▶"

    val openMark = el("span")
    openMark.className = "jv-punct jv-open"
    openMark.textContent = openPunct

    val previewEl = el("span")
    previewEl.className = "jv-preview"
    previewEl.textContent = preview

    line.appendChild(twist)
    key.foreach(line.appendChild)
    line.appendChild(openMark)
    line.appendChild(previewEl)
    appendFieldRefresh(line, basePath, segments, fetchable)
    root.appendChild(line)

    if (children.isEmpty) {
      openMark.className = "jv-punct"
      openMark.textContent = openPunct + closePunct
      previewEl.textContent = ""
    } else {
      var built = false
      def ensureChildren(): Unit =
        if (!built) {
          built = true
          val childUl = el("ul")
          childUl.className = "jv-children"
          children.foreach { case (name, value, childSegments) =>
            val keyEl = el("span")
            keyEl.className = "jv-key"
            keyEl.textContent = name
            val li = el("li")
            li.appendChild(
              renderJsonNode(
                value,
                forceOpen = false,
                key = Some(keyEl),
                basePath = basePath,
                segments = childSegments,
                fetchable = fetchable
              )
            )
            childUl.appendChild(li)
          }
          root.appendChild(childUl)

          val closeLine = el("div")
          closeLine.className = "jv-line jv-close"
          val closePad = el("span")
          closePad.className = "jv-twist empty"
          closePad.textContent = "·"
          val closeMark = el("span")
          closeMark.className = "jv-punct"
          closeMark.textContent = closePunct
          closeLine.appendChild(closePad)
          closeLine.appendChild(closeMark)
          root.appendChild(closeLine)
        }

      if (isOpen) ensureChildren()

      val toggle = (_: MouseEvent) => {
        val collapsed = root.classList.toggle("collapsed")
        if (collapsed) {
          jsonExpanded.remove(pathKey)
          twist.textContent = "▶"
        } else {
          jsonExpanded.add(pathKey)
          ensureChildren()
          twist.textContent = "▼"
        }
      }
      twist.onclick = toggle
      openMark.onclick = toggle
      previewEl.onclick = toggle
      previewEl.style.cursor = "pointer"
      openMark.style.cursor = "pointer"
    }

    root
  }

  private def appendFieldRefresh(
      row: HTMLElement,
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): Unit =
    httpPathForJsonField(basePath, segments, fetchable).foreach { httpPath =>
      val btn = el("button").asInstanceOf[dom.html.Button]
      btn.className = "jv-refresh"
      btn.textContent = "↻"
      btn.title = s"Refresh $httpPath"
      if (fieldLoading.contains(httpPath)) btn.disabled = true
      btn.onclick = { (e: MouseEvent) =>
        e.stopPropagation()
        refreshJsonField(basePath, segments, httpPath)
      }
      row.appendChild(btn)
    }

  private def httpPathForJsonField(
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): Option[String] = {
    if (segments.exists(_.startsWith("_"))) None
    else {
      val path =
        if (segments.isEmpty) basePath
        else segments.foldLeft(basePath)((p, s) => s"$p/$s")
      if (fetchable.contains(path)) Some(path) else None
    }
  }

  private def refreshJsonField(
      basePath: String,
      segments: List[String],
      httpPath: String
  ): Unit = {
    fieldLoading.add(httpPath)
    setFieldRefreshBusy(httpPath, busy = true)
    setIndexStatus(s"Refreshing $httpPath…", ok = true)
    fetchJson(httpPath).onComplete {
      case Success(json) =>
        fieldLoading.remove(httpPath)
        val errorOpt = json.hcursor.get[String]("error").toOption
        if (errorOpt.isDefined || decodeFailed(json)) {
          val err = errorOpt.getOrElse(decodeErrorMessage(json))
          setIndexStatus(s"Field refresh failed: $err", ok = false)
          setFieldRefreshBusy(httpPath, busy = false)
        } else {
          payloads.update(httpPath, json)
          payloads.get(basePath) match {
            case None =>
              setIndexStatus(s"Parent payload missing for $basePath", ok = false)
              setFieldRefreshBusy(httpPath, busy = false)
            case Some(parent) =>
              val fieldDecoded = extractDecoded(json)
              val fieldOverlay = extractOverlayDecoded(json)
              val mergedDecoded =
                replaceAtPath(extractDecoded(parent), segments, fieldDecoded)
              val mergedOverlay =
                (extractOverlayDecoded(parent), fieldOverlay) match {
                  case (Some(rootOverlay), Some(piece)) =>
                    Some(replaceAtPath(rootOverlay, segments, piece))
                  case (rootOverlay, _) => rootOverlay
                }
              payloads.update(basePath, writeDecodedFields(parent, mergedDecoded, mergedOverlay))
              bumpRefreshFreshnessAt(basePath, segments)
              repaintJsonViews(basePath)
              setIndexStatus(s"Refreshed $httpPath", ok = true)
          }
        }
      case Failure(err) =>
        fieldLoading.remove(httpPath)
        setFieldRefreshBusy(httpPath, busy = false)
        setIndexStatus(err.getMessage, ok = false)
    }
  }

  private def setFieldRefreshBusy(httpPath: String, busy: Boolean): Unit = {
    val buttons = dom.document.querySelectorAll("button.jv-refresh")
    var i = 0
    while (i < buttons.length) {
      val btn = buttons.item(i).asInstanceOf[dom.html.Button]
      if (btn.title == s"Refresh $httpPath") btn.disabled = busy
      i += 1
    }
  }

  private def bumpRefreshFreshness(
      basePath: String,
      @unused decoded: Json,
      @unused overlay: Option[Json]
  ): Unit = {
    refreshCount += 1
    clearFreshUnder(basePath, Nil)
    fieldFreshAt.update(stampKey(basePath, Nil), refreshCount)
  }

  private def bumpRefreshFreshnessAt(basePath: String, segments: List[String]): Unit = {
    refreshCount += 1
    clearFreshUnder(basePath, segments)
    fieldFreshAt.update(stampKey(basePath, segments), refreshCount)
  }

  private def clearFreshUnder(basePath: String, segments: List[String]): Unit = {
    val prefix = stampKey(basePath, segments)
    fieldFreshAt.keys.filter(k => k == prefix || k.startsWith(prefix + "/")).toList.foreach { k =>
      fieldFreshAt.remove(k)
    }
  }

  private def jsonPointer(segments: List[String]): String =
    if (segments.isEmpty) "" else segments.mkString("/", "/", "")

  private def stampKey(basePath: String, segments: List[String]): String =
    basePath + jsonPointer(segments)

  private def refreshAge(basePath: String, segments: List[String]): Int = {
    var segs = segments
    while (true) {
      fieldFreshAt.get(stampKey(basePath, segs)) match {
        case Some(stamped) =>
          return math.min(MaxRefreshAge, math.max(0, refreshCount - stamped))
        case None =>
          if (segs.isEmpty) return 0
          segs = segs.init
      }
    }
    0
  }

  private def extractDecoded(json: Json): Json =
    json.hcursor
      .downField("reads")
      .downArray
      .downField("decoded")
      .focus
      .orElse(json.hcursor.downField("decoded").focus)
      .getOrElse(json)

  private def extractOverlayDecoded(json: Json): Option[Json] =
    json.hcursor
      .downField("reads")
      .downArray
      .downField("overlayDecoded")
      .focus
      .orElse(json.hcursor.downField("overlayDecoded").focus)

  private def replaceAtPath(json: Json, segments: List[String], value: Json): Json =
    segments match {
      case Nil          => value
      case head :: tail =>
        json.asObject match {
          case Some(obj) =>
            val child = obj(head).getOrElse(Json.Null)
            Json.fromJsonObject(obj.add(head, replaceAtPath(child, tail, value)))
          case None =>
            json.asArray match {
              case Some(arr) =>
                Try(head.toInt).toOption match {
                  case Some(index) if index >= 0 && index < arr.size =>
                    Json.fromValues(arr.updated(index, replaceAtPath(arr(index), tail, value)))
                  case _ => json
                }
              case None => json
            }
        }
    }

  private def writeDecodedFields(
      payload: Json,
      decoded: Json,
      overlay: Option[Json]
  ): Json =
    payload.hcursor.downField("reads").as[Vector[Json]] match {
      case Right(reads) if reads.nonEmpty =>
        val head = reads.head
        var updatedHead = head.mapObject(_.add("decoded", decoded))
        overlay match {
          case Some(od) =>
            updatedHead = updatedHead.mapObject(_.add("overlayDecoded", od))
          case None =>
            if (head.hcursor.downField("overlayDecoded").succeeded)
              updatedHead = updatedHead.mapObject(_.remove("overlayDecoded"))
        }
        payload.mapObject(_.add("reads", Json.fromValues(updatedHead +: reads.tail)))
      case _ =>
        var obj = payload.mapObject(_.add("decoded", decoded))
        overlay match {
          case Some(od) =>
            obj.mapObject(_.add("overlayDecoded", od))
          case None =>
            if (payload.hcursor.downField("overlayDecoded").succeeded)
              obj.mapObject(_.remove("overlayDecoded"))
            else obj
        }
    }

  private def jsonPrimitiveClass(json: Json): String =
    if (json.isNull) "jv-null"
    else if (json.isBoolean) "jv-bool"
    else if (json.isNumber) "jv-num"
    else if (json.isString) "jv-str"
    else "jv-punct"

  private def jsonPrimitiveText(json: Json): String =
    json.fold(
      jsonNull = "null",
      jsonBoolean = _.toString,
      jsonNumber = _.toString,
      jsonString = s => Json.fromString(s).noSpaces,
      jsonArray = _ => "[]",
      jsonObject = _ => "{}"
    )

  private def showIdleDetail(): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    body.setAttribute("hidden", "true")
    empty.removeAttribute("hidden")
    empty.textContent = "Search for a route, then press ↻ to load memory."
    clearIndexBrowse()
    detailViewPath = None
  }

  private def clearIndexBrowse(): Unit = {
    indexTemplate = None
    indexValues = Nil
    byId("index-bar").setAttribute("hidden", "true")
    setIndexStatus("", ok = true)
  }

  private def renderIndexBar(): Unit = {
    indexTemplate match {
      case None =>
        byId("index-bar").setAttribute("hidden", "true")
      case Some(template) =>
        byId("index-bar").removeAttribute("hidden")
        val host = byId("index-inputs")
        host.innerHTML = ""
        val slotCount = countIndexSlots(template)
        val values =
          if (indexValues.length == slotCount) indexValues
          else List.fill(slotCount)(0)
        indexValues = values
        values.zipWithIndex.foreach { case (value, i) =>
          val lab = el("label")
          lab.textContent = if (slotCount == 1) "index" else s"index$i"
          lab.setAttribute("for", s"index-input-$i")
          val inp = inputEl()
          inp.`type` = "number"
          inp.min = "0"
          inp.step = "1"
          inp.value = value.toString
          inp.id = s"index-input-$i"
          inp.setAttribute("data-index-slot", i.toString)
          inp.onkeydown = { (e: KeyboardEvent) =>
            if (e.key == "Enter") {
              e.preventDefault()
              fetchIndexed(syncIndexInputsFromDom())
            }
          }
          host.appendChild(lab)
          host.appendChild(inp)
        }
        if (values.headOption.getOrElse(0) <= 0) byId("index-prev").setAttribute("disabled", "true")
        else byId("index-prev").removeAttribute("disabled")
    }
  }

  private def syncIndexInputsFromDom(): List[Int] = {
    val inputs = byId("index-inputs").querySelectorAll("input[data-index-slot]")
    val slots = (0 until inputs.length).map { i =>
      val el = inputs.item(i).asInstanceOf[HTMLInputElement]
      val slot = el.getAttribute("data-index-slot").toInt
      val value = Try(el.value.toInt).getOrElse(0).max(0)
      slot -> value
    }
    slots.sortBy(_._1).map(_._2).toList
  }

  private def fetchIndexed(indices: List[Int]): Unit =
    indexTemplate match {
      case None =>
        setIndexStatus("No indexed route selected.", ok = false)
      case Some(template) =>
        val normalized =
          if (indices.length == countIndexSlots(template)) indices
          else List.fill(countIndexSlots(template))(0)
        indexValues = normalized
        renderIndexBar()
        val path = substituteIndices(template, normalized)
        selected = Some(path)
        refreshPath(path)
    }

  private def stepIndex(delta: Int): Unit = {
    val current = syncIndexInputsFromDom()
    if (current.isEmpty) ()
    else if (delta < 0 && current.head <= 0) {
      setIndexStatus("Already at index 0.", ok = true)
    } else {
      val head = (current.head + delta).max(0)
      fetchIndexed(head :: current.tail)
    }
  }

  private def setIndexStatus(message: String, ok: Boolean): Unit = {
    val status = byId("index-status")
    status.textContent = message
    status.className = "index-status" + (if (message.isEmpty) "" else if (ok) " ok" else " err")
  }

  private def countIndexSlots(path: String): Int =
    path.split('/').count(_ == "{index}")

  private def substituteIndices(template: String, indices: List[Int]): String = {
    var remaining = indices
    template
      .split('/')
      .map {
        case "{index}" =>
          val v = remaining.headOption.getOrElse(0)
          remaining = remaining.drop(1)
          v.toString
        case other => other
      }
      .mkString("/")
  }

  private def extractIndices(template: String, concrete: String): Option[List[Int]] = {
    val t = template.split('/')
    val c = concrete.split('/')
    if (t.length != c.length) None
    else {
      val pairs = t.zip(c)
      if (
        pairs.forall {
          case ("{index}", seg) => seg.nonEmpty && seg.forall(_.isDigit)
          case (a, b)           => a == b
        }
      )
        Some(pairs.collect { case ("{index}", seg) => seg.toInt }.toList)
      else None
    }
  }

  private def resolveIndexBrowse(path: String): Option[(String, List[Int])] =
    if (path.contains("{index}")) {
      val slots = countIndexSlots(path)
      val values =
        if (indexTemplate.contains(path) && indexValues.length == slots) indexValues
        else List.fill(slots)(0)
      Some((path, values))
    } else {
      val templates = collectNodes(catalog).map(_.path).filter(_.contains("{index}")).distinct
      templates.view
        .flatMap(t => extractIndices(t, path).map(t -> _))
        .headOption
        .orElse {
          val parts = path.split('/').toList
          val nums = parts.reverse.takeWhile(s => s.nonEmpty && s.forall(_.isDigit)).reverse
          if (nums.isEmpty) None
          else {
            val base = parts.dropRight(nums.length).mkString("/")
            val template =
              (parts.dropRight(nums.length) ++ List.fill(nums.length)("{index}")).mkString("/")
            if (templates.exists(t => t == template || t.startsWith(base + "/")))
              Some((template, nums.map(_.toInt)))
            else None
          }
        }
    }

  private def concretePathForSelection(path: String): String =
    if (!path.contains("{index}")) path
    else
      indexTemplate
        .filter(_ == path)
        .map(t => substituteIndices(t, indexValues))
        .getOrElse(path)

  private def decodeFailed(json: Json): Boolean = {
    val cursor = json.hcursor
    val hasError = cursor.get[String]("error").toOption.exists(_.nonEmpty)
    val decodedFocus = cursor
      .downField("decoded")
      .focus
      .orElse(cursor.downField("reads").downArray.downField("decoded").focus)
    hasError || decodedFocus.exists(_.isNull)
  }

  private def decodeErrorMessage(json: Json): String =
    json.hcursor
      .get[String]("error")
      .toOption
      .filter(_.nonEmpty)
      .getOrElse("Decode returned null / empty result.")

  private def loadDraftForStruct(structId: String): Unit = {
    editingStructId = Some(structId)
    inputById("edit-struct").value = structId
    setEditorStatus(s"Loading fields for $structId…", ok = true)

    def applyMembers(members: List[OverlayMemberUi]): Unit = {
      draftMembers =
        if (members.nonEmpty) members
        else List(OverlayMemberUi("field0", "u8", isArray = false, None, false))
      renderFieldEditor()
      setEditorStatus(s"Loaded ${draftMembers.size} field(s).", ok = true)
    }

    overlays.structs
      .get(structId)
      .map(_.members)
      .orElse(
        overlays.newStructs
          .find(ns => ns.id == structId || s"overlay#${ns.id}" == structId)
          .map(_.members)
      ) match {
      case Some(members) =>
        applyMembers(members)
      case None =>
        fetchJson(s"/types/fields?id=${js.URIUtils.encodeURIComponent(structId)}").onComplete {
          case Success(json) =>
            json.hcursor.downField("fields").as[List[OverlayMemberUi]] match {
              case Right(fields) => applyMembers(fields)
              case Left(err)     =>
                setEditorStatus(s"Failed to decode fields: ${err.getMessage}", ok = false)
                applyMembers(Nil)
            }
          case Failure(err) =>
            setEditorStatus(err.getMessage, ok = false)
            applyMembers(Nil)
        }
    }
  }

  private def setEditorOpen(open: Boolean): Unit = {
    editorOpen = open
    val body = byId("detail-body")
    val editorBody = byId("editor-body")
    val chevron = byId("editor-chevron")
    val toggle = byId("editor-toggle")
    if (open) {
      body.classList.add("editor-open")
      editorBody.removeAttribute("hidden")
      chevron.textContent = "▼"
      toggle.setAttribute("aria-expanded", "true")
    } else {
      body.classList.remove("editor-open")
      editorBody.setAttribute("hidden", "true")
      chevron.textContent = "▶"
      toggle.setAttribute("aria-expanded", "false")
    }
  }

  private def renderTypeDatalists(): Unit = {
    val structList = byId("struct-type-list")
    val allList = byId("all-type-list")
    structList.innerHTML = ""
    allList.innerHTML = ""
    typeCatalog.foreach { entry =>
      val opt = dom.document.createElement("option").asInstanceOf[dom.HTMLOptionElement]
      opt.value = entry.id
      allList.appendChild(opt)
      if (entry.kind == "struct") {
        val structOpt = dom.document.createElement("option").asInstanceOf[dom.HTMLOptionElement]
        structOpt.value = entry.id
        structList.appendChild(structOpt)
      }
    }
    overlays.newStructs.foreach { ns =>
      val id = if (ns.id.contains("#")) ns.id else s"overlay#${ns.id}"
      val opt = dom.document.createElement("option").asInstanceOf[dom.HTMLOptionElement]
      opt.value = id
      structList.appendChild(opt)
      val opt2 = dom.document.createElement("option").asInstanceOf[dom.HTMLOptionElement]
      opt2.value = id
      allList.appendChild(opt2)
    }
  }

  private def renderFieldEditor(): Unit = {
    val list = byId("field-list")
    list.innerHTML = ""
    draftMembers.zipWithIndex.foreach { case (member, index) =>
      val row = el("div")
      row.className = "field-row"
      row.setAttribute("data-field-index", index.toString)

      val nameInput = inputEl()
      nameInput.value = member.name
      nameInput.placeholder = "name"
      nameInput.setAttribute("data-role", "name")

      val typeInput = inputEl()
      typeInput.value = member.typeId
      typeInput.placeholder = "typeId"
      typeInput.setAttribute("list", "all-type-list")
      typeInput.setAttribute("data-role", "typeId")

      val ptr = inputEl()
      ptr.setAttribute("type", "checkbox")
      ptr.asInstanceOf[HTMLInputElement].checked = member.isPointer
      ptr.title = "pointer"
      ptr.setAttribute("data-role", "pointer")

      val arr = inputEl()
      arr.setAttribute("type", "checkbox")
      arr.asInstanceOf[HTMLInputElement].checked = member.isArray
      arr.title = "array"
      arr.setAttribute("data-role", "array")

      val len = inputEl()
      len.className = "len"
      len.placeholder = "len"
      len.value = member.arrayLength.map(_.toString).getOrElse("")
      len.setAttribute("data-role", "len")

      val up = el("button")
      up.className = "ghost"
      up.textContent = "↑"
      up.onclick = (_: MouseEvent) => {
        syncDraftFromDom()
        if (index > 0) {
          val a = draftMembers(index - 1)
          val b = draftMembers(index)
          draftMembers = draftMembers.updated(index - 1, b).updated(index, a)
          renderFieldEditor()
        }
      }

      val down = el("button")
      down.className = "ghost"
      down.textContent = "↓"
      down.onclick = (_: MouseEvent) => {
        syncDraftFromDom()
        if (index < draftMembers.length - 1) {
          val a = draftMembers(index)
          val b = draftMembers(index + 1)
          draftMembers = draftMembers.updated(index, b).updated(index + 1, a)
          renderFieldEditor()
        }
      }

      val del = el("button")
      del.className = "ghost"
      del.textContent = "✕"
      del.onclick = (_: MouseEvent) => {
        syncDraftFromDom()
        draftMembers = draftMembers.zipWithIndex.collect {
          case (m, i) if i != index => m
        }
        renderFieldEditor()
      }

      row.appendChild(nameInput)
      row.appendChild(typeInput)
      row.appendChild(ptr)
      row.appendChild(arr)
      row.appendChild(len)
      row.appendChild(up)
      row.appendChild(down)
      row.appendChild(del)
      list.appendChild(row)
    }
  }

  /** Read editor DOM into draftMembers so Apply sees the latest edits. */
  private def syncDraftFromDom(): Unit = {
    val list = byId("field-list")
    val rows = list.querySelectorAll(".field-row")
    val synced = (0 until rows.length.toInt).flatMap { i =>
      val row = rows(i).asInstanceOf[HTMLElement]
      val name = Option(row.querySelector("""[data-role="name"]"""))
        .map(_.asInstanceOf[HTMLInputElement].value.trim)
        .getOrElse("")
      val typeId = Option(row.querySelector("""[data-role="typeId"]"""))
        .map(_.asInstanceOf[HTMLInputElement].value.trim)
        .getOrElse("u8")
      val isPointer = Option(row.querySelector("""[data-role="pointer"]"""))
        .exists(_.asInstanceOf[HTMLInputElement].checked)
      val isArray = Option(row.querySelector("""[data-role="array"]"""))
        .exists(_.asInstanceOf[HTMLInputElement].checked)
      val arrayLength = Option(row.querySelector("""[data-role="len"]"""))
        .flatMap(el => Try(el.asInstanceOf[HTMLInputElement].value.trim.toInt).toOption)
        .filter(_ > 0)
      if (name.isEmpty && typeId.isEmpty) None
      else Some(OverlayMemberUi(name, typeId, isArray, arrayLength, isPointer))
    }.toList
    if (synced.nonEmpty || rows.length == 0) {
      draftMembers = synced
    }
  }

  private def applyOverlay(): Unit = {
    syncDraftFromDom()
    val structId = inputById("edit-struct").value.trim
    if (structId.isEmpty) {
      setEditorStatus("Select a struct to edit.", ok = false)
    } else if (draftMembers.isEmpty || draftMembers.exists(_.name.trim.isEmpty)) {
      setEditorStatus("Each field needs a non-empty name.", ok = false)
    } else if (draftMembers.exists(_.typeId.trim.isEmpty)) {
      setEditorStatus("Each field needs a typeId.", ok = false)
    } else {
      editingStructId = Some(structId)
      val updatedStructs =
        overlays.structs + (structId -> OverlayStructDefUi(draftMembers))
      val updatedNew =
        overlays.newStructs.map { ns =>
          val fullId = if (ns.id.contains("#")) ns.id else s"overlay#${ns.id}"
          if (fullId == structId || ns.id == structId)
            ns.copy(members = draftMembers)
          else ns
        }
      val next = overlays.copy(structs = updatedStructs, newStructs = updatedNew)
      putOverlays(next)
    }
  }

  private def resetCurrentStruct(): Unit = {
    editingStructId match {
      case None =>
        setEditorStatus("Select a struct to reset.", ok = false)
      case Some(structId) =>
        val next = overlays.copy(structs = overlays.structs - structId)
        putOverlays(next)
    }
  }

  private def createNewStruct(): Unit = {
    val raw = inputById("new-struct-id").value.trim
    if (raw.isEmpty) {
      setEditorStatus("Enter a name for the new struct.", ok = false)
    } else {
      val id = if (raw.contains("#")) raw else s"overlay#$raw"
      if (overlays.newStructs.exists(ns => ns.id == raw || ns.id == id)) {
        setEditorStatus(s"$id already exists.", ok = false)
      } else {
        val members = List(OverlayMemberUi("field0", "u8", isArray = false, None, false))
        val next = overlays.copy(
          newStructs = overlays.newStructs :+ OverlayNewStructUi(id, members)
        )
        overlays = next
        typeCatalog = typeCatalog :+ TypeCatalogEntry(
          id,
          "struct",
          Some(members.map(_.name)),
          Some(members)
        )
        editingStructId = Some(id)
        draftMembers = members
        inputById("new-struct-id").value = ""
        inputById("edit-struct").value = id
        renderTypeDatalists()
        renderFieldEditor()
        setEditorStatus(s"Created $id — edit fields then Apply.", ok = true)
      }
    }
  }

  private def putOverlays(document: TypeOverlayDocumentUi): Unit = {
    setEditorStatus("Saving overlays…", ok = true)
    putJson("/overlays", document.asJson).onComplete {
      case Success(json) =>
        json.as[TypeOverlayDocumentUi] match {
          case Right(saved) =>
            overlays = saved
            setEditorStatus("Overlays applied.", ok = true)
            loadTypesAndOverlays()
            selected.foreach(refreshPath)
          case Left(err) =>
            setEditorStatus(s"Bad overlay response: ${err.getMessage}", ok = false)
        }
      case Failure(err) =>
        setEditorStatus(err.getMessage, ok = false)
    }
  }

  private def setEditorStatus(message: String, ok: Boolean): Unit = {
    val elStatus = byId("editor-status")
    elStatus.textContent = message
    elStatus.className = if (ok) "ok" else "err"
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

  private def collectNodes(nodes: List[RouteTreeNode]): List[RouteTreeNode] =
    nodes.flatMap(n => n :: collectNodes(n.children))

  private def searchInput(): HTMLInputElement =
    dom.document.getElementById("route-search").asInstanceOf[HTMLInputElement]

  private def inputById(id: String): HTMLInputElement =
    dom.document.getElementById(id).asInstanceOf[HTMLInputElement]

  private def inputEl(): HTMLInputElement =
    dom.document.createElement("input").asInstanceOf[HTMLInputElement]

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

  private def putJson(path: String, payload: Json): Future[Json] = {
    val init = new RequestInit {
      method = HttpMethod.PUT
      body = payload.noSpaces
      headers = js.Dictionary("Content-Type" -> "application/json")
    }
    dom.fetch(path, init).toFuture.flatMap { response =>
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
}
