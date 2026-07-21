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

final case class OpenTab(
    path: String,
    decodeType: Option[String] = None,
    editingStructId: Option[String] = None,
    draftMembers: List[OverlayMemberUi] = Nil,
    editorOpen: Boolean = false,
    indexTemplate: Option[String] = None,
    indexValues: List[Int] = Nil
)

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

  /** Open workspace tabs (insertion order). */
  private val openTabs = scala.collection.mutable.LinkedHashMap.empty[String, OpenTab]

  /** Active tab path — drives decode/editor panes and watch repaint targeting. */
  private var activeTabPath: Option[String] = None

  // Working copies for the active tab (synced via saveActiveTabDraft / restoreActiveTabEditor).
  private var editingStructId: Option[String] = None
  private var draftMembers: List[OverlayMemberUi] = Nil
  private var lastDecodeType: Option[String] = None
  private var editorOpen: Boolean = false

  /** Json-pointer path → epoch ms when that subtree was last refreshed. */
  private val fieldFreshAt = scala.collection.mutable.Map.empty[String, Double]

  /** Most recent successful memory update time (epoch ms). */
  private var latestDataTime: Double = js.Date.now()

  private val fieldLoading = scala.collection.mutable.Set.empty[String]
  private var cachedFetchablePaths: Set[String] = Set.empty

  /** Expanded JSON composite paths (`stampKey`) — collapsed nodes are not built in the DOM. */
  private val jsonExpanded = scala.collection.mutable.Set.empty[String]

  /** Dual-view `.jv-line` / value nodes by stampKey — watch updates patch these in place. */
  private val dualLineByStamp = scala.collection.mutable.Map.empty[String, HTMLElement]
  private val dualValueByStamp = scala.collection.mutable.Map.empty[String, HTMLElement]

  /** Coalesce rapid watch patches into one rAF flush (avoids multi-kHz DOM work). Value is a set of
    * focus keys (`""` = whole tree, else `seg/seg` under the base path).
    */
  private val pendingPatchFocus =
    scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
  private var patchRafScheduled = false

  /** Active realtime watches: HTTP path → server watchId. */
  private val activeWatches = scala.collection.mutable.Map.empty[String, Int]

  /** Source watch path → overlay JSON segment paths co-watched via byte-range overlap. */
  private val watchOverlaySegments =
    scala.collection.mutable.Map.empty[String, List[List[String]]]

  private var watchSocket: Option[dom.WebSocket] = None

  /** Soft ceiling: fields older than this (relative to latest data / now) look distinctly muted. */
  private val StaleAfterMs = 60_000.0

  /** Template path containing `{index}` slots currently being browsed. */
  private var indexTemplate: Option[String] = None

  /** Current index values for `indexTemplate` (or concrete indexed path). */
  private var indexValues: List[Int] = Nil

  private def detailViewPath: Option[String] = activeTabPath

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
    byId("expand-tree").onclick = (_: MouseEvent) => expandAllTree()
    byId("collapse-tree").onclick = (_: MouseEvent) => collapseAllTree()
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
      persistActiveTabDraft()
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
    connectWatchSocket()
    js.timers.setInterval(2000)(refreshVisibleAgeStyles())
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
              showToast(
                "IR warnings / errors",
                response.errors.mkString("\n")
              )
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
      // Search results start fully collapsed; Expand all / per-node twists open branches.
      expanded.clear()
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

  private def expandAllTree(): Unit = {
    collectNodes(visible).foreach { n =>
      if (n.children.nonEmpty) expanded.add(n.path)
    }
    renderResults()
  }

  private def collapseAllTree(): Unit = {
    expanded.clear()
    renderResults()
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
          // Errors do not open tabs — keep tree error badge; leave workspace as-is.
          if (openTabs.isEmpty) showErrorDetail(path, decodeErrorMessage(json))
          setIndexStatus(s"End of chain / decode failed at $path", ok = false)
        } else {
          payloads.update(path, json)
          bumpRefreshFreshness(path, extractDecoded(json), extractOverlayDecoded(json))
          val openedNew = !openTabs.contains(path)
          if (openedNew) {
            selected = Some(path)
            openOrFocusTab(path)
            showDetail(path, json)
          } else if (activeTabPath.contains(path)) {
            selected = Some(path)
            showDetail(path, json)
          }
          renderResults()
          setIndexStatus(s"Loaded $path", ok = true)
        }
      case Failure(err) =>
        loading.remove(path)
        loadErrors.update(path, err.getMessage)
        selected = Some(path)
        renderResults()
        if (openTabs.isEmpty) showErrorDetail(path, err.getMessage)
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
    val selectedClass =
      if (activeTabPath.contains(node.path) || selected.contains(node.path)) " selected"
      else ""
    row.className = "node" + selectedClass

    val twist = el("span")
    twist.className = if (node.children.isEmpty) "twist empty" else "twist"
    val isOpen = expanded.contains(node.path)
    // Keep an empty width gutter for leaves; do not insert a · placeholder glyph.
    twist.textContent = if (node.children.isEmpty) "" else if (isOpen) "▼" else "▶"
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
      val concrete = concretePathForSelection(node.path)
      resolveIndexBrowse(node.path) match {
        case Some((template, indices)) =>
          if (openTabs.contains(concrete)) {
            updateActiveTabIndex(concrete, Some(template), indices)
          } else {
            indexTemplate = Some(template)
            indexValues = indices
            renderIndexBar()
          }
        case None =>
          if (!openTabs.contains(concrete)) clearIndexBrowse()
      }
      if (openTabs.contains(concrete)) {
        activateTab(concrete)
        payloads.get(concrete).foreach(showDetail(concrete, _))
      } else {
        payloads.get(concrete) match {
          case Some(_) =>
            // Tab was closed; require ↻ to reopen (successful load opens tabs).
            setIndexStatus(s"$concrete — press ↻ to reopen tab", ok = true)
          case None =>
            loadErrors.get(concrete) match {
              case Some(msg) =>
                if (openTabs.isEmpty) showErrorDetail(concrete, msg)
                else setIndexStatus(msg, ok = false)
              case None =>
                if (openTabs.isEmpty) {
                  if (node.path.contains("{index}")) showIndexPrompt(node.path)
                  else showPlaceholder(node.path)
                } else
                  setIndexStatus(s"$concrete — press ↻ to fetch", ok = true)
            }
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
    ensureWorkspaceVisible()
    byId("editor-panel").removeAttribute("hidden")

    val tab = openTabs.getOrElse(path, OpenTab(path))
    openTabs.update(path, tab)
    if (!activeTabPath.contains(path)) {
      saveActiveTabDraft()
      activeTabPath = Some(path)
      restoreWorkingCopies(tab)
    }

    resolveIndexBrowse(path).foreach { case (template, indices) =>
      indexTemplate = Some(template)
      indexValues = indices
      persistActiveTabDraft()
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
    val decodeType = readCursor
      .get[String]("decodeType")
      .toOption
      .orElse(json.hcursor.get[String]("decodeType").toOption)
    lastDecodeType = decodeType
    decodeType.foreach { id =>
      openTabs.update(path, openTabs.getOrElse(path, OpenTab(path)).copy(decodeType = Some(id)))
    }

    byId("detail-path").textContent = path
    val now = js.Date.now()
    if (!fieldFreshAt.contains(stampKey(path, Nil, overlayPanel = false))) {
      fieldFreshAt.update(stampKey(path, Nil, overlayPanel = false), now)
    }
    if (
      overlayDecoded.isDefined &&
      !fieldFreshAt.contains(stampKey(path, Nil, overlayPanel = true))
    ) {
      fieldFreshAt.update(stampKey(path, Nil, overlayPanel = true), now)
    }
    setDualDecodeView(path, decoded, overlayDecoded)

    setEditorOpen(editorOpen)
    decodeType.foreach { id =>
      val current = openTabs.getOrElse(path, OpenTab(path))
      if (current.editingStructId.isEmpty && current.draftMembers.isEmpty) {
        editingStructId = Some(id)
        inputById("edit-struct").value = id
        persistActiveTabDraft()
        loadDraftForStruct(id)
      } else {
        inputById("edit-struct").value = editingStructId.getOrElse(id)
        renderFieldEditor()
      }
    }
    renderTabBar()
  }

  private def showErrorDetail(path: String, message: String): Unit = {
    // Only used when no tabs are open (failed first load).
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    val tabBar = byId("tab-bar")
    empty.removeAttribute("hidden")
    empty.textContent = s"Error loading $path: $message"
    body.setAttribute("hidden", "true")
    tabBar.classList.remove("has-tabs")
    tabBar.innerHTML = ""
  }

  private def showPlaceholder(path: String): Unit = {
    val empty = byId("detail-empty")
    val body = byId("detail-body")
    body.setAttribute("hidden", "true")
    byId("tab-bar").classList.remove("has-tabs")
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
    setDualPlain(
      s"Set index values below, then Fetch (or press ↻). Use Prev/Next to walk the chain.",
      ""
    )
    renderIndexBar()
  }

  private def ensureWorkspaceVisible(): Unit = {
    byId("detail-empty").setAttribute("hidden", "true")
    byId("detail-body").removeAttribute("hidden")
  }

  private def openOrFocusTab(path: String): Unit = {
    saveActiveTabDraft()
    if (!openTabs.contains(path)) {
      openTabs.update(path, OpenTab(path))
    }
    activateTab(path)
  }

  private def activateTab(path: String): Unit = {
    if (!openTabs.contains(path)) return
    if (!activeTabPath.contains(path)) {
      saveActiveTabDraft()
      activeTabPath = Some(path)
      restoreWorkingCopies(openTabs(path))
    } else {
      activeTabPath = Some(path)
    }
    selected = Some(path)
    renderTabBar()
    ensureWorkspaceVisible()
    renderIndexBar()
    setEditorOpen(editorOpen)
  }

  private def closeTab(path: String): Unit = {
    val wasActive = activeTabPath.contains(path)
    if (wasActive) saveActiveTabDraft()
    openTabs.remove(path)
    if (!wasActive) {
      renderTabBar()
      return
    }
    val remaining = openTabs.keys.toList
    remaining.lastOption match {
      case Some(next) =>
        activeTabPath = None
        activateTab(next)
        payloads.get(next).foreach(showDetail(next, _))
      case None =>
        clearWorkspace()
    }
  }

  private def clearWorkspace(): Unit = {
    openTabs.clear()
    activeTabPath = None
    editingStructId = None
    draftMembers = Nil
    lastDecodeType = None
    editorOpen = false
    clearIndexBrowse()
    byId("tab-bar").classList.remove("has-tabs")
    byId("tab-bar").innerHTML = ""
    byId("detail-body").setAttribute("hidden", "true")
    byId("editor-panel").setAttribute("hidden", "true")
    val empty = byId("detail-empty")
    empty.removeAttribute("hidden")
    empty.textContent = "Search for a route, then press ↻ to load memory."
  }

  private def renderTabBar(): Unit = {
    val bar = byId("tab-bar")
    bar.innerHTML = ""
    if (openTabs.isEmpty) {
      bar.classList.remove("has-tabs")
      return
    }
    bar.classList.add("has-tabs")
    openTabs.values.foreach { tab =>
      val btn = el("div")
      btn.className = "tab" + (if (activeTabPath.contains(tab.path)) " active" else "")
      btn.setAttribute("role", "tab")
      btn.setAttribute("tabindex", "0")
      btn.setAttribute("aria-selected", activeTabPath.contains(tab.path).toString)
      btn.title = tab.path
      val label = el("span")
      label.className = "tab-label"
      label.textContent = shortenPath(tab.path)
      val close = el("button")
      close.className = "tab-close"
      close.setAttribute("type", "button")
      close.textContent = "×"
      close.title = s"Close ${tab.path}"
      close.onclick = { (e: MouseEvent) =>
        e.stopPropagation()
        closeTab(tab.path)
      }
      btn.onclick = { (_: MouseEvent) =>
        if (!activeTabPath.contains(tab.path)) {
          activateTab(tab.path)
          payloads.get(tab.path).foreach(showDetail(tab.path, _))
        }
      }
      btn.appendChild(label)
      btn.appendChild(close)
      bar.appendChild(btn)
    }
  }

  private def saveActiveTabDraft(): Unit =
    activeTabPath.foreach { path =>
      syncDraftFromDom()
      openTabs.get(path).foreach { tab =>
        openTabs.update(
          path,
          tab.copy(
            decodeType = lastDecodeType.orElse(tab.decodeType),
            editingStructId = editingStructId,
            draftMembers = draftMembers,
            editorOpen = editorOpen,
            indexTemplate = indexTemplate,
            indexValues = indexValues
          )
        )
      }
    }

  private def persistActiveTabDraft(): Unit =
    activeTabPath.foreach { path =>
      openTabs.get(path).foreach { tab =>
        openTabs.update(
          path,
          tab.copy(
            decodeType = lastDecodeType.orElse(tab.decodeType),
            editingStructId = editingStructId,
            draftMembers = draftMembers,
            editorOpen = editorOpen,
            indexTemplate = indexTemplate,
            indexValues = indexValues
          )
        )
      }
    }

  private def restoreWorkingCopies(tab: OpenTab): Unit = {
    lastDecodeType = tab.decodeType
    editingStructId = tab.editingStructId
    draftMembers = tab.draftMembers
    editorOpen = tab.editorOpen
    indexTemplate = tab.indexTemplate
    indexValues = tab.indexValues
    inputById("edit-struct").value = editingStructId.getOrElse("")
  }

  private def updateActiveTabIndex(
      path: String,
      template: Option[String],
      indices: List[Int]
  ): Unit = {
    indexTemplate = template
    indexValues = indices
    openTabs.get(path).foreach { tab =>
      openTabs.update(path, tab.copy(indexTemplate = template, indexValues = indices))
    }
    if (activeTabPath.contains(path)) renderIndexBar()
  }

  private def setPlainView(id: String, text: String, error: Boolean = false): Unit = {
    val host = byId(id)
    host.innerHTML = ""
    host.className = if (error) "json-view plain error" else "json-view plain"
    host.textContent = text
  }

  private def setDualPlain(
      sourceText: String,
      overlayText: String,
      error: Boolean = false
  ): Unit = {
    val host = byId("detail-dual")
    host.innerHTML = ""
    host.className = "json-view dual-scroll"
    val row = el("div")
    row.className = "dual-row"
    val left = el("div")
    left.className = "dual-cell" + (if (error) " error" else "")
    val leftLine = el("div")
    leftLine.className = if (error) "jv-line jv-null" else "jv-line"
    leftLine.style.color = if (error) "var(--err)" else "var(--muted)"
    leftLine.textContent = sourceText
    left.appendChild(leftLine)
    val right = el("div")
    right.className = "dual-cell"
    val rightLine = el("div")
    rightLine.className = "jv-line"
    rightLine.style.color = "var(--muted)"
    rightLine.textContent = overlayText
    right.appendChild(rightLine)
    row.appendChild(left)
    row.appendChild(right)
    host.appendChild(row)
  }

  private def setDualDecodeView(
      basePath: String,
      source: Json,
      overlay: Option[Json]
  ): Unit = {
    dualLineByStamp.clear()
    dualValueByStamp.clear()
    val host = byId("detail-dual")
    host.innerHTML = ""
    host.className = "json-view dual-scroll"
    host.appendChild(
      renderDualNode(
        source = Some(source),
        overlay = overlay,
        forceOpen = true,
        sourceKeyName = None,
        overlayKeyName = None,
        basePath = basePath,
        sourceSegments = Nil,
        overlaySegments = Nil,
        sourceAddress = None,
        overlayAddress = None,
        fetchable = cachedFetchablePaths,
        depth = 0
      )
    )
  }

  private def repaintJsonViews(basePath: String): Unit =
    payloads.get(basePath).foreach { payload =>
      setDualDecodeView(basePath, extractDecoded(payload), extractOverlayDecoded(payload))
    }

  // DESNOTE(jbarber, 2026-07-20): Rapid watch updates used to call setDualDecodeView and
  // recreate every button. Clicks on ◎ never completed (mousedown target destroyed before
  // mouseup). Patch leaf text/age in place; fall back to a full rebuild only on shape change.
  // Coalesce to animation frames and scope to the watched subtree when possible — DAP can
  // stream far faster than walking a full Melee struct (or player_slots[]) in the DOM.
  private def patchJsonViews(basePath: String, focusSegments: List[String] = Nil): Unit = {
    val focusKey = if (focusSegments.isEmpty) "" else focusSegments.mkString("/")
    val focuses = pendingPatchFocus.getOrElseUpdate(basePath, scala.collection.mutable.Set.empty)
    if (focusKey.isEmpty) {
      focuses.clear()
      focuses.add("")
    } else if (!focuses.contains("")) {
      focuses.add(focusKey)
    }
    if (!patchRafScheduled) {
      patchRafScheduled = true
      val _ = dom.window.requestAnimationFrame { (_: Double) =>
        patchRafScheduled = false
        val batch = pendingPatchFocus.toList
        pendingPatchFocus.clear()
        batch.foreach { case (path, focusSet) =>
          if (focusSet.contains("")) flushPatchJsonViews(path, Nil)
          else
            focusSet.foreach { key =>
              val segs = if (key.isEmpty) Nil else key.split("/").toList.filter(_.nonEmpty)
              flushPatchJsonViews(path, segs)
            }
        }
      }
    }
  }

  private def flushPatchJsonViews(basePath: String, focusSegments: List[String]): Unit =
    payloads.get(basePath).foreach { payload =>
      val source = extractDecoded(payload)
      val overlay = extractOverlayDecoded(payload)
      if (
        dualLineByStamp.isEmpty ||
        !patchDualDecodeView(basePath, source, overlay, focusSegments)
      )
        setDualDecodeView(basePath, source, overlay)
      else
        refreshDualAges(basePath, focusSegments)
    }

  private def patchDualDecodeView(
      basePath: String,
      source: Json,
      overlay: Option[Json],
      focusSegments: List[String]
  ): Boolean = {
    def walk(json: Json, segments: List[String], overlayPanel: Boolean): Boolean = {
      val key = stampKey(basePath, segments, overlayPanel)
      json.arrayOrObject(
        or = {
          dualValueByStamp.get(key) match {
            case Some(el) =>
              val text = jsonPrimitiveText(json)
              // Skip no-op writes — full-tree watches otherwise thrash every leaf each tick.
              if (el.textContent != text) {
                el.className = jsonPrimitiveClass(json)
                el.textContent = text
                dualLineByStamp.get(key).foreach { line =>
                  Option(line.closest(".dual-cell")).foreach(_.classList.remove("missing"))
                }
              }
              true
            case None =>
              dualLineByStamp.get(key) match {
                case Some(_) =>
                  // DOM has a row at this path but not a leaf value — shape changed.
                  false
                case None =>
                  // Collapsed / not built yet — expand will re-read from payloads.
                  true
              }
          }
        },
        jsonArray = arr =>
          dualLineByStamp.get(key) match {
            case Some(line) if line.classList.contains("jv-leaf") && arr.nonEmpty =>
              false
            case _ =>
              arr.toList.zipWithIndex.forall { case (value, index) =>
                walk(value, segments :+ index.toString, overlayPanel)
              }
          },
        jsonObject = obj =>
          dualLineByStamp.get(key) match {
            case Some(line)
                if line.classList.contains("jv-leaf") &&
                  obj.keys.exists(k => !DualDecodeAlign.isMetaKey(k)) =>
              false
            case _ =>
              obj.toList.forall { case (name, value) =>
                if (DualDecodeAlign.isMetaKey(name)) true
                else walk(value, segments :+ name, overlayPanel)
              }
          }
      )
    }

    def walkFrom(root: Json, overlayPanel: Boolean): Boolean =
      if (focusSegments.isEmpty) walk(root, Nil, overlayPanel)
      else
        getAtPath(root, focusSegments) match {
          case None        => true
          case Some(focus) => walk(focus, focusSegments, overlayPanel)
        }

    walkFrom(source, overlayPanel = false) &&
    overlay.forall(o => walkFrom(o, overlayPanel = true))
  }

  private def refreshDualAges(basePath: String, focusSegments: List[String]): Unit = {
    val focusPrefix =
      if (focusSegments.isEmpty) basePath
      else basePath + "/" + focusSegments.mkString("/")
    dualLineByStamp.foreach { case (stamp, line) =>
      val overlayPanel = stamp.startsWith("ov:")
      val raw = if (overlayPanel) stamp.stripPrefix("ov:") else stamp
      if (raw == focusPrefix || raw.startsWith(focusPrefix + "/")) {
        val segments =
          if (raw == basePath) Nil
          else raw.stripPrefix(basePath + "/").split("/").toList.filter(_.nonEmpty)
        applyAgeAttributes(line, basePath, segments, overlayPanel)
      }
    }
  }

  private def trackDualLine(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit = {
    applyAgeAttributes(line, basePath, segments, overlayPanel)
    dualLineByStamp.update(stampKey(basePath, segments, overlayPanel), line)
  }

  private def trackDualValue(
      value: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit =
    dualValueByStamp.update(stampKey(basePath, segments, overlayPanel), value)

  /** Shared expand key so renamed source/overlay fields still toggle together. */
  private def dualStampKey(
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String]
  ): String =
    stampKey(basePath, sourceSegments, overlayPanel = false) + "\n" +
      stampKey(basePath, overlaySegments, overlayPanel = true)

  private def getAtPath(json: Json, segments: List[String]): Option[Json] =
    segments.foldLeft(Option(json)) { (acc, seg) =>
      acc.flatMap { j =>
        j.asObject
          .flatMap(_.apply(seg))
          .orElse(
            j.asArray.flatMap { arr =>
              Try(seg.toInt).toOption.flatMap(i => arr.lift(i))
            }
          )
      }
    }

  private def dualObjectFieldCount(obj: io.circe.JsonObject): Int =
    obj.keys.count(k => !DualDecodeAlign.isMetaKey(k))

  private def alignDualChildren(source: Option[Json], overlay: Option[Json]): List[DualChild] = {
    val srcObj = source.flatMap(_.asObject)
    val ovObj = overlay.flatMap(_.asObject)
    val srcArr = source.flatMap(_.asArray)
    val ovArr = overlay.flatMap(_.asArray)
    if (srcObj.isDefined || ovObj.isDefined)
      DualDecodeAlign.alignObjects(srcObj, ovObj)
    else if (srcArr.isDefined || ovArr.isDefined)
      DualDecodeAlign.alignArrays(
        srcArr.map(_.toList).getOrElse(Nil),
        ovArr.map(_.toList).getOrElse(Nil)
      )
    else Nil
  }

  private def parseHexAddressUi(raw: String): Option[Long] = {
    val hex = raw.trim.toLowerCase.stripPrefix("0x")
    if (hex.isEmpty || !hex.forall(c => c.isDigit || (c >= 'a' && c <= 'f'))) None
    else
      try Some(java.lang.Long.parseUnsignedLong(hex, 16))
      catch { case _: NumberFormatException => None }
  }

  private def structAbsoluteAddress(json: Option[Json]): Option[Long] =
    json
      .flatMap(_.asObject)
      .flatMap(_("_address"))
      .flatMap(_.asString)
      .flatMap(parseHexAddressUi)

  private def memberAbsoluteAddress(
      parentJson: Option[Json],
      parentAddr: Option[Long],
      fieldName: Option[String],
      fieldJson: Option[Json]
  ): Option[Long] =
    structAbsoluteAddress(fieldJson).orElse {
      for {
        name <- fieldName if !DualDecodeAlign.isMetaKey(name)
        base <- structAbsoluteAddress(parentJson).orElse(parentAddr)
        off <- parentJson
          .flatMap(_.asObject)
          .map(DualDecodeAlign.parseOffsets)
          .flatMap(_.get(name))
      } yield base + off
    }

  private def renderDualNode(
      source: Option[Json],
      overlay: Option[Json],
      forceOpen: Boolean,
      sourceKeyName: Option[String],
      overlayKeyName: Option[String],
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String],
      sourceAddress: Option[Long],
      overlayAddress: Option[Long],
      fetchable: Set[String],
      depth: Int
  ): HTMLElement = {
    val srcObj = source.flatMap(_.asObject)
    val ovObj = overlay.flatMap(_.asObject)
    val srcArr = source.flatMap(_.asArray)
    val ovArr = overlay.flatMap(_.asArray)
    val resolvedSrcAddr = structAbsoluteAddress(source).orElse(sourceAddress)
    val resolvedOvAddr = structAbsoluteAddress(overlay).orElse(overlayAddress)

    if (srcObj.isDefined || ovObj.isDefined) {
      val children = alignDualChildren(source, overlay)
      val srcCount = srcObj.map(dualObjectFieldCount).getOrElse(0)
      val ovCount = ovObj.map(dualObjectFieldCount).getOrElse(0)
      renderDualComposite(
        forceOpen = forceOpen,
        openPunct = "{",
        closePunct = "}",
        sourcePreview =
          if (source.isEmpty) "—" else if (srcCount == 0) "{}" else s"{$srcCount}",
        overlayPreview =
          if (overlay.isEmpty) "—" else if (ovCount == 0) "{}" else s"{$ovCount}",
        sourceKeyName = sourceKeyName,
        overlayKeyName = overlayKeyName,
        basePath = basePath,
        sourceSegments = sourceSegments,
        overlaySegments = overlaySegments,
        sourceAddress = resolvedSrcAddr,
        overlayAddress = resolvedOvAddr,
        fetchable = fetchable,
        depth = depth,
        sourcePresent = source.isDefined,
        overlayPresent = overlay.isDefined,
        sourceJson = source,
        overlayJson = overlay,
        children = children
      )
    } else if (srcArr.isDefined || ovArr.isDefined) {
      val children = alignDualChildren(source, overlay)
      renderDualComposite(
        forceOpen = forceOpen,
        openPunct = "[",
        closePunct = "]",
        sourcePreview = srcArr.map(a => if (a.isEmpty) "[]" else s"[${a.size}]").getOrElse("—"),
        overlayPreview = ovArr.map(a => if (a.isEmpty) "[]" else s"[${a.size}]").getOrElse("—"),
        sourceKeyName = sourceKeyName,
        overlayKeyName = overlayKeyName,
        basePath = basePath,
        sourceSegments = sourceSegments,
        overlaySegments = overlaySegments,
        sourceAddress = resolvedSrcAddr,
        overlayAddress = resolvedOvAddr,
        fetchable = fetchable,
        depth = depth,
        sourcePresent = source.isDefined,
        overlayPresent = overlay.isDefined,
        sourceJson = source,
        overlayJson = overlay,
        children = children
      )
    } else {
      renderDualLeaf(
        source,
        overlay,
        sourceKeyName,
        overlayKeyName,
        basePath,
        sourceSegments,
        overlaySegments,
        sourceAddress,
        overlayAddress,
        fetchable,
        depth
      )
    }
  }

  private def renderDualLeaf(
      source: Option[Json],
      overlay: Option[Json],
      sourceKeyName: Option[String],
      overlayKeyName: Option[String],
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String],
      sourceAddress: Option[Long],
      overlayAddress: Option[Long],
      fetchable: Set[String],
      depth: Int
  ): HTMLElement = {
    val row = el("div")
    row.className = "dual-row"
    row.style.paddingLeft = s"${depth * 0.55}rem"

    val left = el("div")
    left.className = "dual-cell" + (if (source.isEmpty) " missing" else "")
    val leftLine = el("div")
    leftLine.className = "jv-line jv-leaf"
    sourceKeyName.foreach { name =>
      val keyEl = el("span")
      keyEl.className = "jv-key"
      keyEl.textContent = name
      leftLine.appendChild(keyEl)
    }
    source match {
      case Some(json) =>
        trackDualLine(leftLine, basePath, sourceSegments, overlayPanel = false)
        val leaf = el("span")
        leaf.className = jsonPrimitiveClass(json)
        leaf.textContent = jsonPrimitiveText(json)
        trackDualValue(leaf, basePath, sourceSegments, overlayPanel = false)
        leftLine.appendChild(leaf)
        makeValueEditable(
          leaf,
          sourceAddress,
          sourceSegments,
          overlayPanel = false,
          json
        )
        appendFieldActions(leftLine, basePath, sourceSegments, fetchable, overlayPanel = false)
        appendPointerFocus(leftLine, basePath, sourceSegments, Some(json), overlay)
      case None =>
        val leaf = el("span")
        leaf.className = "jv-punct"
        leaf.textContent = "—"
        leftLine.appendChild(leaf)
    }
    left.appendChild(leftLine)

    val right = el("div")
    right.className = "dual-cell" + (if (overlay.isEmpty) " missing" else "")
    val rightLine = el("div")
    rightLine.className = "jv-line jv-leaf"
    overlayKeyName.foreach { name =>
      val keyEl = el("span")
      keyEl.className = "jv-key"
      keyEl.textContent = name
      rightLine.appendChild(keyEl)
    }
    overlay match {
      case Some(json) =>
        trackDualLine(rightLine, basePath, overlaySegments, overlayPanel = true)
        val leaf = el("span")
        leaf.className = jsonPrimitiveClass(json)
        leaf.textContent = jsonPrimitiveText(json)
        trackDualValue(leaf, basePath, overlaySegments, overlayPanel = true)
        rightLine.appendChild(leaf)
        makeValueEditable(
          leaf,
          overlayAddress,
          overlaySegments,
          overlayPanel = true,
          json
        )
        appendFieldActions(rightLine, basePath, overlaySegments, fetchable, overlayPanel = true)
      case None =>
        val leaf = el("span")
        leaf.className = "jv-punct"
        leaf.textContent =
          if (sourceSegments.isEmpty && source.isDefined) "No overlay applied for this type."
          else "—"
        rightLine.appendChild(leaf)
    }
    right.appendChild(rightLine)

    row.appendChild(left)
    row.appendChild(right)
    row
  }

  private def renderDualComposite(
      forceOpen: Boolean,
      openPunct: String,
      closePunct: String,
      sourcePreview: String,
      overlayPreview: String,
      sourceKeyName: Option[String],
      overlayKeyName: Option[String],
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String],
      sourceAddress: Option[Long],
      overlayAddress: Option[Long],
      fetchable: Set[String],
      depth: Int,
      sourcePresent: Boolean,
      overlayPresent: Boolean,
      sourceJson: Option[Json],
      overlayJson: Option[Json],
      children: List[DualChild]
  ): HTMLElement = {
    val pathKey = dualStampKey(basePath, sourceSegments, overlaySegments)
    val isOpen = forceOpen || jsonExpanded.contains(pathKey)
    val root = el("div")
    root.className = if (isOpen) "jv-composite" else "jv-composite collapsed"

    val leftCell = el("div")
    leftCell.className = "dual-cell" + (if (!sourcePresent) " missing" else "")
    val leftLine = el("div")
    leftLine.className = if (children.isEmpty) "jv-line jv-leaf" else "jv-line"
    if (sourcePresent)
      trackDualLine(leftLine, basePath, sourceSegments, overlayPanel = false)
    lazy val leftTwist = {
      val twist = el("span")
      twist.className = "jv-twist"
      twist.textContent = if (isOpen) "▼" else "▶"
      leftLine.appendChild(twist)
      twist
    }
    if (children.nonEmpty) leftTwist
    sourceKeyName.foreach { name =>
      val keyEl = el("span")
      keyEl.className = "jv-key"
      keyEl.textContent = name
      leftLine.appendChild(keyEl)
    }
    val leftOpen = el("span")
    leftOpen.className = "jv-punct jv-open"
    leftOpen.textContent = if (sourcePresent) openPunct else ""
    val leftPreview = el("span")
    leftPreview.className = "jv-preview"
    leftPreview.textContent = if (sourcePresent) sourcePreview else "—"
    if (children.isEmpty && sourcePresent) {
      leftOpen.className = "jv-punct"
      leftOpen.textContent = openPunct + closePunct
      leftPreview.textContent = ""
    }
    leftLine.appendChild(leftOpen)
    leftLine.appendChild(leftPreview)
    if (sourcePresent)
      appendFieldActions(leftLine, basePath, sourceSegments, fetchable, overlayPanel = false)
    appendPointerFocus(leftLine, basePath, sourceSegments, sourceJson, overlayJson)
    leftCell.appendChild(leftLine)

    val rightCell = el("div")
    rightCell.className = "dual-cell" + (if (!overlayPresent) " missing" else "")
    val rightLine = el("div")
    rightLine.className = if (children.isEmpty) "jv-line jv-leaf" else "jv-line"
    if (overlayPresent)
      trackDualLine(rightLine, basePath, overlaySegments, overlayPanel = true)
    lazy val rightTwist = {
      val twist = el("span")
      twist.className = "jv-twist"
      twist.textContent = if (isOpen) "▼" else "▶"
      rightLine.appendChild(twist)
      twist
    }
    if (children.nonEmpty) rightTwist
    overlayKeyName.foreach { name =>
      val keyEl = el("span")
      keyEl.className = "jv-key"
      keyEl.textContent = name
      rightLine.appendChild(keyEl)
    }
    val rightOpen = el("span")
    rightOpen.className = "jv-punct jv-open"
    rightOpen.textContent = if (overlayPresent) openPunct else ""
    val rightPreview = el("span")
    rightPreview.className = "jv-preview"
    rightPreview.textContent =
      if (overlayPresent) overlayPreview
      else if (sourceSegments.isEmpty) "No overlay applied for this type."
      else "—"
    if (children.isEmpty && overlayPresent) {
      rightOpen.className = "jv-punct"
      rightOpen.textContent = openPunct + closePunct
      rightPreview.textContent = ""
    }
    rightLine.appendChild(rightOpen)
    rightLine.appendChild(rightPreview)
    if (overlayPresent)
      appendFieldActions(rightLine, basePath, overlaySegments, fetchable, overlayPanel = true)
    rightCell.appendChild(rightLine)

    val headerRow = el("div")
    headerRow.className = "dual-row"
    headerRow.style.paddingLeft = s"${depth * 0.55}rem"
    headerRow.appendChild(leftCell)
    headerRow.appendChild(rightCell)
    root.appendChild(headerRow)

    if (children.nonEmpty) {
      var built = false
      def ensureChildren(): Unit =
        if (!built) {
          built = true
          val kids = payloads.get(basePath) match {
            case Some(payload) =>
              val srcAt = getAtPath(extractDecoded(payload), sourceSegments)
              val ovAt = extractOverlayDecoded(payload).flatMap(getAtPath(_, overlaySegments))
              alignDualChildren(srcAt, ovAt)
            case None =>
              children
          }
          val childUl = el("ul")
          childUl.className = "dual-children"
          kids.foreach { child =>
            val childSrcSegs =
              child.sourceName.map(sourceSegments :+ _).getOrElse(sourceSegments)
            val childOvSegs =
              child.overlayName.map(overlaySegments :+ _).getOrElse(overlaySegments)
            val childSrcAddr = memberAbsoluteAddress(
              sourceJson,
              sourceAddress,
              child.sourceName,
              child.source
            )
            val childOvAddr = memberAbsoluteAddress(
              overlayJson,
              overlayAddress,
              child.overlayName,
              child.overlay
            )
            val li = el("li")
            li.appendChild(
              renderDualNode(
                source = child.source,
                overlay = child.overlay,
                forceOpen = false,
                sourceKeyName = child.sourceName,
                overlayKeyName = child.overlayName,
                basePath = basePath,
                sourceSegments = childSrcSegs,
                overlaySegments = childOvSegs,
                sourceAddress = childSrcAddr,
                overlayAddress = childOvAddr,
                fetchable = fetchable,
                depth = depth + 1
              )
            )
            childUl.appendChild(li)
          }
          root.appendChild(childUl)

          val closeRow = el("div")
          closeRow.className = "dual-row dual-close"
          closeRow.style.paddingLeft = s"${depth * 0.55}rem"
          def closeCell(present: Boolean): HTMLElement = {
            val cell = el("div")
            cell.className = "dual-cell" + (if (!present) " missing" else "")
            val line = el("div")
            line.className = "jv-line jv-close"
            val mark = el("span")
            mark.className = "jv-punct"
            mark.textContent = if (present) closePunct else ""
            line.appendChild(mark)
            cell.appendChild(line)
            cell
          }
          closeRow.appendChild(closeCell(sourcePresent))
          closeRow.appendChild(closeCell(overlayPresent))
          root.appendChild(closeRow)
        }

      if (isOpen) ensureChildren()

      val toggle = (_: MouseEvent) => {
        val collapsed = root.classList.toggle("collapsed")
        if (collapsed) {
          jsonExpanded.remove(pathKey)
          leftTwist.textContent = "▶"
          rightTwist.textContent = "▶"
        } else {
          jsonExpanded.add(pathKey)
          ensureChildren()
          leftTwist.textContent = "▼"
          rightTwist.textContent = "▼"
        }
      }
      List(leftTwist, rightTwist, leftOpen, rightOpen, leftPreview, rightPreview).foreach { el =>
        el.onclick = toggle
        el.style.cursor = "pointer"
      }
    }

    root
  }

  // Kept for any single-pane fallbacks; dual view is the primary decode UI.
  private def setJsonView(
      id: String,
      json: Json,
      rootOpen: Boolean,
      basePath: String,
      overlayPanel: Boolean
  ): Unit = {
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
        fetchable = cachedFetchablePaths,
        overlayPanel = overlayPanel,
        absoluteAddress = structAbsoluteAddress(Some(json))
      )
    )
  }

  private def renderJsonNode(
      json: Json,
      forceOpen: Boolean,
      key: Option[HTMLElement],
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      overlayPanel: Boolean,
      absoluteAddress: Option[Long]
  ): HTMLElement = {
    val resolvedAddr = structAbsoluteAddress(Some(json)).orElse(absoluteAddress)
    json.arrayOrObject(
      or = {
        val row = el("div")
        row.className = "jv-line jv-leaf"
        applyAgeAttributes(row, basePath, segments, overlayPanel)
        key.foreach(row.appendChild)
        val leaf = el("span")
        leaf.className = jsonPrimitiveClass(json)
        leaf.textContent = jsonPrimitiveText(json)
        row.appendChild(leaf)
        makeValueEditable(leaf, resolvedAddr, segments, overlayPanel, json)
        appendFieldActions(row, basePath, segments, fetchable, overlayPanel)
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
          overlayPanel = overlayPanel,
          parentJson = Some(json),
          absoluteAddress = resolvedAddr,
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
          overlayPanel = overlayPanel,
          parentJson = Some(json),
          absoluteAddress = resolvedAddr,
          children = obj.toList.collect {
            case (name, value) if !DualDecodeAlign.isMetaKey(name) =>
              (name, value, segments :+ name)
          }
        )
    )
  }

  private def renderJsonComposite(
      forceOpen: Boolean,
      openPunct: String,
      closePunct: String,
      preview: String,
      key: Option[HTMLElement],
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      overlayPanel: Boolean,
      parentJson: Option[Json],
      absoluteAddress: Option[Long],
      children: List[(String, Json, List[String])]
  ): HTMLElement = {
    val pathKey = stampKey(basePath, segments, overlayPanel)
    val isOpen = forceOpen || jsonExpanded.contains(pathKey)
    val root = el("div")
    root.className = if (isOpen) "jv-composite" else "jv-composite collapsed"

    val line = el("div")
    line.className = if (children.isEmpty) "jv-line jv-leaf" else "jv-line"
    applyAgeAttributes(line, basePath, segments, overlayPanel)

    lazy val twist = {
      val t = el("span")
      t.className = "jv-twist"
      t.textContent = if (isOpen) "▼" else "▶"
      line.appendChild(t)
      t
    }
    if (children.nonEmpty) twist

    val openMark = el("span")
    openMark.className = "jv-punct jv-open"
    openMark.textContent = openPunct

    val previewEl = el("span")
    previewEl.className = "jv-preview"
    previewEl.textContent = preview

    key.foreach(line.appendChild)
    line.appendChild(openMark)
    line.appendChild(previewEl)
    appendFieldActions(line, basePath, segments, fetchable, overlayPanel)
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
            val childAddr =
              memberAbsoluteAddress(parentJson, absoluteAddress, Some(name), Some(value))
            val li = el("li")
            li.appendChild(
              renderJsonNode(
                value,
                forceOpen = false,
                key = Some(keyEl),
                basePath = basePath,
                segments = childSegments,
                fetchable = fetchable,
                overlayPanel = overlayPanel,
                absoluteAddress = childAddr
              )
            )
            childUl.appendChild(li)
          }
          root.appendChild(childUl)

          val closeLine = el("div")
          closeLine.className = "jv-line jv-close"
          val closeMark = el("span")
          closeMark.className = "jv-punct"
          closeMark.textContent = closePunct
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

  private def appendFieldActions(
      row: HTMLElement,
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      overlayPanel: Boolean
  ): Unit = {
    def prepend(node: HTMLElement): Unit =
      Option(row.firstChild) match {
        case Some(first) => val _ = row.insertBefore(node, first)
        case None        => row.appendChild(node)
      }

    if (overlayPanel) {
      if (isOverlaySegmentWatched(basePath, segments)) {
        row.classList.add("jv-watching-row")
        val watchBtn = el("button").asInstanceOf[dom.html.Button]
        watchBtn.className = "jv-watch jv-watching"
        watchBtn.textContent = "◉"
        watchBtn.title = "Watched via overlapping source field"
        watchBtn.disabled = true
        prepend(watchBtn)
      }
    } else
      httpPathForJsonField(basePath, segments, fetchable).foreach { httpPath =>
        if (activeWatches.contains(httpPath)) row.classList.add("jv-watching-row")
        val watchBtn = el("button").asInstanceOf[dom.html.Button]
        val watching = activeWatches.contains(httpPath)
        watchBtn.className = if (watching) "jv-watch jv-watching" else "jv-watch"
        watchBtn.textContent = if (watching) "◉" else "◎"
        watchBtn.title =
          if (watching) s"Stop watching $httpPath" else s"Watch $httpPath in realtime"
        // pointerdown: click is lost when a watch rebuild replaces the button between down/up.
        watchBtn.onpointerdown = { (e: dom.PointerEvent) =>
          e.stopPropagation()
          e.preventDefault()
          toggleWatch(httpPath)
        }

        val btn = el("button").asInstanceOf[dom.html.Button]
        btn.className = "jv-refresh"
        btn.textContent = "↻"
        btn.title = s"Refresh $httpPath"
        if (fieldLoading.contains(httpPath)) btn.disabled = true
        btn.onpointerdown = { (e: dom.PointerEvent) =>
          e.stopPropagation()
          e.preventDefault()
          refreshJsonField(basePath, segments, httpPath)
        }
        // Left margin order: watch, then refresh (prepend reverse).
        prepend(btn)
        prepend(watchBtn)
      }
  }

  private def isOverlaySegmentWatched(basePath: String, segments: List[String]): Boolean =
    activeWatches.contains(basePath) ||
      watchOverlaySegments.exists { case (sourcePath, overlaySegs) =>
        (sourcePath == basePath || sourcePath.startsWith(basePath + "/")) &&
        overlaySegs.exists(segs => segments == segs || segments.startsWith(segs))
      }

  private def toggleWatch(httpPath: String): Unit =
    activeWatches.get(httpPath) match {
      case Some(watchId) =>
        deleteJson(s"/watches/$watchId").onComplete {
          case Success(_) =>
            activeWatches.remove(httpPath)
            watchOverlaySegments.remove(httpPath)
            detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
            setIndexStatus(s"Stopped watching $httpPath", ok = true)
          case Failure(err) =>
            setIndexStatus(err.getMessage, ok = false)
        }
      case None =>
        postJson("/watches", Json.obj("path" -> Json.fromString(httpPath))).onComplete {
          case Success(json) =>
            json.hcursor.get[Int]("watchId") match {
              case Right(watchId) =>
                activeWatches.update(httpPath, watchId)
                val overlaySegs = json.hcursor
                  .downField("overlaySegments")
                  .as[List[List[String]]]
                  .getOrElse(Nil)
                if (overlaySegs.nonEmpty) watchOverlaySegments.update(httpPath, overlaySegs)
                else watchOverlaySegments.remove(httpPath)
                detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
                val overlayNote =
                  if (overlaySegs.isEmpty) ""
                  else s" (overlay: ${overlaySegs.map(_.mkString("/")).mkString(", ")})"
                setIndexStatus(s"Watching $httpPath (#$watchId)$overlayNote", ok = true)
              case Left(err) =>
                val msg = json.hcursor
                  .get[String]("error")
                  .toOption
                  .getOrElse(err.message)
                setIndexStatus(s"Watch failed: $msg", ok = false)
            }
          case Failure(err) =>
            setIndexStatus(err.getMessage, ok = false)
        }
    }

  private def connectWatchSocket(): Unit = {
    watchSocket.foreach { ws =>
      try ws.close()
      catch { case _: Throwable => () }
    }
    val proto = if (dom.window.location.protocol == "https:") "wss:" else "ws:"
    val ws = new dom.WebSocket(s"$proto//${dom.window.location.host}/ws")
    watchSocket = Some(ws)
    ws.onmessage = { (event: dom.MessageEvent) =>
      parse(event.data.toString) match {
        case Right(json) => handleWatchSocketMessage(json)
        case Left(_)     => ()
      }
    }
    ws.onclose = { (_: dom.CloseEvent) =>
      watchSocket = None
      dom.window.setTimeout(() => connectWatchSocket(), 2000.0)
    }
    ws.onerror = { (_: dom.Event) =>
      try ws.close()
      catch { case _: Throwable => () }
    }
  }

  private def handleWatchSocketMessage(json: Json): Unit = {
    val cursor = json.hcursor
    cursor.get[String]("type").toOption match {
      case Some("watchesCleared") =>
        activeWatches.clear()
        watchOverlaySegments.clear()
        detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
        setIndexStatus("DAP reconnected — watches cleared", ok = false)
      case Some("watchesRebound") =>
        syncWatchesFromJsonList(json.hcursor.downField("watches").as[List[Json]].getOrElse(Nil))
        setIndexStatus("Watches rebound after overlay/model reload", ok = true)
      case Some("memoryChanged") =>
        val pathOpt = cursor.get[String]("path").toOption
        val decodedOpt = cursor.downField("decoded").focus
        val overlayOpt = cursor.downField("overlayDecoded").focus.filterNot(_.isNull)
        val overlayUpdates = cursor
          .downField("overlayUpdates")
          .as[List[Json]]
          .toOption
          .getOrElse(Nil)
          .flatMap { item =>
            for {
              segs <- item.hcursor.get[List[String]]("segments").toOption
              value <- item.hcursor.downField("decoded").focus
            } yield segs -> value
          }
        (pathOpt, decodedOpt) match {
          case (Some(watchedPath), Some(decoded)) =>
            applyWatchUpdate(watchedPath, decoded, overlayOpt, overlayUpdates)
          case _ =>
            ()
        }
      case _ =>
        ()
    }
  }

  private def applyWatchUpdate(
      watchedPath: String,
      decoded: Json,
      overlay: Option[Json],
      overlayUpdates: List[(List[String], Json)]
  ): Unit = {
    val parents =
      payloads.keys.filter(base => watchedPath == base || watchedPath.startsWith(base + "/")).toList
    parents.foreach { basePath =>
      payloads.get(basePath).foreach { parent =>
        val segments =
          if (watchedPath == basePath) Nil
          else watchedPath.stripPrefix(basePath + "/").split("/").toList.filter(_.nonEmpty)
        val mergedDecoded = replaceAtPath(extractDecoded(parent), segments, decoded)
        // DESNOTE(jbarber, 2026-07-20): Member watches send overlayUpdates without a full
        // overlayDecoded payload. Seed an empty object when needed so byte-mapped overlay
        // fields still patch in realtime (previously `.map` dropped updates when the parent
        // had no overlayDecoded yet).
        val baseOverlay =
          (extractOverlayDecoded(parent).filterNot(_.isNull), overlay.filterNot(_.isNull)) match {
            case (_, Some(piece)) if segments.isEmpty =>
              Some(piece)
            case (Some(rootOverlay), Some(piece)) =>
              Some(replaceAtPath(rootOverlay, segments, piece))
            case (Some(rootOverlay), None) =>
              Some(rootOverlay)
            case (None, Some(piece)) =>
              Some(replaceAtPath(Json.obj(), segments, piece))
            case (None, None) if overlayUpdates.nonEmpty =>
              Some(Json.obj())
            case _ =>
              None
          }
        val mergedOverlay =
          overlayUpdates.foldLeft(baseOverlay) { case (acc, (overlaySegs, value)) =>
            Some(replaceAtPath(acc.getOrElse(Json.obj()), overlaySegs, value))
          }
        val updated = writeDecodedFields(parent, mergedDecoded, mergedOverlay)
        payloads.update(basePath, updated)
        bumpRefreshFreshnessAt(basePath, segments, overlayPanel = false)
        if (overlay.isDefined || overlayUpdates.nonEmpty)
          bumpRefreshFreshnessAt(basePath, segments, overlayPanel = true)
        overlayUpdates.foreach { case (overlaySegs, _) =>
          bumpRefreshFreshnessAt(basePath, overlaySegs, overlayPanel = true)
        }
        watchOverlaySegments.getOrElse(watchedPath, Nil).foreach { overlaySegs =>
          bumpRefreshFreshnessAt(basePath, overlaySegs, overlayPanel = true)
        }
        if (detailViewPath.contains(basePath)) patchJsonViews(basePath, segments)
      }
    }
  }

  private def makeValueEditable(
      leafEl: HTMLElement,
      address: Option[Long],
      segments: List[String],
      overlayPanel: Boolean,
      current: Json
  ): Unit = {
    if (address.isEmpty || current.isObject || current.isArray || current.isNull) ()
    else {
      val addr = address.get
      leafEl.style.cursor = "text"
      leafEl.title = f"Double-click to edit (0x$addr%x)"
      leafEl.ondblclick = { (e: MouseEvent) =>
        e.stopPropagation()
        e.preventDefault()
        // Prefer live DOM text (watch patches update textContent without a JSON cache).
        val live = Option(leafEl.textContent).map(_.trim).filter(_.nonEmpty) match {
          case Some(text) => parseEditValue(text, current)
          case None       => current
        }
        beginFieldEdit(leafEl, addr, segments, overlayPanel, live)
      }
    }
  }

  private def beginFieldEdit(
      leafEl: HTMLElement,
      address: Long,
      segments: List[String],
      overlayPanel: Boolean,
      current: Json
  ): Unit = {
    val input = dom.document.createElement("input").asInstanceOf[dom.html.Input]
    input.className = "jv-edit"
    input.`type` = "text"
    input.value = editDisplayText(current)
    val parent = leafEl.parentNode
    if (parent != null) {
      parent.replaceChild(input, leafEl)
      input.focus()
      input.select()
      var finished = false
      var committing = false
      def restoreLeaf(): Unit =
        if (input.parentNode == parent) {
          parent.replaceChild(leafEl, input)
        }
      def finish(restore: Boolean): Unit =
        if (!finished) {
          finished = true
          if (restore) restoreLeaf()
          else input.disabled = true
        }
      def commit(): Unit =
        if (!finished) {
          committing = true
          val parsed = parseEditValue(input.value, current)
          val decodeType =
            lastDecodeType.orElse(detailViewPath.flatMap(openTabs.get).flatMap(_.decodeType))
          decodeType match {
            case None =>
              setIndexStatus("Cannot write: missing decodeType for this tab", ok = false)
              finish(restore = true)
            case Some(_) =>
              finish(restore = false)
              writeMemoryValue(address, segments, overlayPanel, parsed) {
                case false =>
                  // Write failed — put the leaf back so the user can try again.
                  finished = false
                  committing = false
                  input.disabled = false
                  if (input.parentNode == null && parent != null) {
                    parent.replaceChild(input, leafEl)
                    input.focus()
                    input.select()
                  }
                case true =>
                  ()
              }
          }
        }
      input.addEventListener(
        "keydown",
        { (e: KeyboardEvent) =>
          if (e.key == "Enter" || e.keyCode == 13) {
            e.preventDefault()
            e.stopPropagation()
            commit()
          } else if (e.key == "Escape" || e.keyCode == 27) {
            e.preventDefault()
            finish(restore = true)
          }
        }
      )
      input.addEventListener(
        "blur",
        { (_: dom.FocusEvent) =>
          // Enter disables the input and fires blur; don't treat that as cancel.
          if (!committing) finish(restore = true)
        }
      )
    }
  }

  private def editDisplayText(json: Json): String =
    json.asString.getOrElse(jsonPrimitiveText(json))

  private def parseEditValue(raw: String, previous: Json): Json = {
    val trimmed = raw.trim
    if (previous.isBoolean)
      Json.fromBoolean(trimmed.equalsIgnoreCase("true") || trimmed == "1")
    else if (previous.isNumber) {
      io.circe.parser
        .parse(trimmed)
        .toOption
        .filter(_.isNumber)
        .orElse(Try(trimmed.toLong).toOption.map(Json.fromLong))
        .orElse(
          Try(trimmed.toDouble).toOption.flatMap(d => Json.fromDouble(d))
        )
        .getOrElse(Json.fromString(trimmed))
    } else
      Json.fromString(trimmed)
  }

  private def writeMemoryValue(
      address: Long,
      segments: List[String],
      overlayPanel: Boolean,
      value: Json
  )(onCompleteResult: Boolean => Unit): Unit = {
    val decodeType =
      lastDecodeType.orElse(detailViewPath.flatMap(openTabs.get).flatMap(_.decodeType))
    decodeType match {
      case None =>
        setIndexStatus("Cannot write: missing decodeType for this tab", ok = false)
        onCompleteResult(false)
      case Some(dt) =>
        val writeSegs = segments.filterNot(DualDecodeAlign.isMetaKey)
        val body = Json.obj(
          "address" -> Json.fromString(f"0x$address%x"),
          "value" -> value,
          "decodeType" -> Json.fromString(dt),
          "segments" -> writeSegs.asJson,
          "overlay" -> Json.fromBoolean(overlayPanel)
        )
        setIndexStatus(f"Writing 0x$address%x…", ok = true)
        // DESNOTE(jbarber, 2026-07-21): writeMemory moved from PUT /memory to
        // POST /dap-proxy/writeMemory (DAP-shaped). Keep POST; putJson is for /overlays.
        postJson("/dap-proxy/writeMemory", body).onComplete {
          case Success(resp) =>
            if (resp.hcursor.get[Boolean]("success").toOption.contains(false)) {
              val err =
                resp.hcursor.get[String]("message").getOrElse("writeMemory failed")
              setIndexStatus(s"Write failed: $err", ok = false)
              detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
              onCompleteResult(false)
            } else {
              setIndexStatus(f"Wrote 0x$address%x", ok = true)
              detailViewPath.foreach(refreshPath)
              onCompleteResult(true)
            }
          case Failure(err) =>
            setIndexStatus(s"Write failed: ${err.getMessage}", ok = false)
            detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
            onCompleteResult(false)
        }
    }
  }

  private def isPointerPointee(json: Json): Boolean =
    json.hcursor.get[Boolean]("_pointer").toOption.contains(true)

  private def appendPointerFocus(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      source: Option[Json],
      overlay: Option[Json]
  ): Unit =
    source.filter(isPointerPointee).foreach { src =>
      val btn = el("button").asInstanceOf[dom.html.Button]
      btn.className = "jv-focus"
      btn.textContent = "⌖"
      val focusPath =
        if (segments.isEmpty) basePath
        else segments.foldLeft(basePath)((p, s) => s"$p/$s")
      btn.title = s"Open $focusPath as root"
      btn.onpointerdown = { (e: dom.PointerEvent) =>
        e.stopPropagation()
        e.preventDefault()
        focusPointerValue(basePath, segments, src, overlay)
      }
      line.appendChild(btn)
    }

  private def focusPointerValue(
      basePath: String,
      segments: List[String],
      source: Json,
      overlay: Option[Json]
  ): Unit = {
    val focusPath =
      if (segments.isEmpty) basePath
      else segments.foldLeft(basePath)((p, s) => s"$p/$s")
    httpPathForJsonField(basePath, segments, cachedFetchablePaths) match {
      case Some(httpPath) =>
        refreshPath(httpPath)
      case None =>
        val envelope = Json
          .obj(
            "path" -> Json.fromString(focusPath),
            "decoded" -> source
          )
          .deepMerge(
            overlay
              .filterNot(_.isNull)
              .map(o => Json.obj("overlayDecoded" -> o))
              .getOrElse(Json.obj())
          )
        payloads.update(focusPath, envelope)
        bumpRefreshFreshness(focusPath, source, overlay)
        openOrFocusTab(focusPath)
        showDetail(focusPath, envelope)
        setIndexStatus(s"Focused $focusPath", ok = true)
        renderResults()
    }
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
      if (isFetchablePath(path, fetchable)) Some(path)
      else if (nestedUnderFetchableAncestor(basePath, segments, fetchable)) Some(path)
      else None
    }
  }

  private def isFetchablePath(path: String, fetchable: Set[String]): Boolean =
    fetchable.contains(path) ||
      fetchable.exists(template =>
        template.contains("{index}") && concreteMatchesTemplate(path, template)
      )

  /** Fields nested under a fetchable ancestor (e.g. `…/player_slots/0/x` under `…/0`). */
  private def nestedUnderFetchableAncestor(
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): Boolean =
    segments.inits.toList
      .drop(1) // exclude the full path (already checked)
      .exists { prefix =>
        prefix.nonEmpty && {
          val ancestor = prefix.foldLeft(basePath)((p, s) => s"$p/$s")
          isFetchablePath(ancestor, fetchable)
        }
      }

  private def concreteMatchesTemplate(concrete: String, template: String): Boolean = {
    val cParts = concrete.split('/').toList
    val tParts = template.split('/').toList
    cParts.length == tParts.length && cParts.zip(tParts).forall {
      case (c, "{index}") => c.nonEmpty && c.forall(_.isDigit)
      case (c, t)         => c == t
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
              bumpRefreshFreshnessAt(basePath, segments, overlayPanel = false)
              if (fieldOverlay.isDefined)
                bumpRefreshFreshnessAt(basePath, segments, overlayPanel = true)
              patchJsonViews(basePath, segments)
              setFieldRefreshBusy(httpPath, busy = false)
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
      overlay: Option[Json]
  ): Unit = {
    stampFreshnessAt(basePath, Nil, overlayPanel = false)
    if (overlay.isDefined) stampFreshnessAt(basePath, Nil, overlayPanel = true)
  }

  private def bumpRefreshFreshnessAt(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit =
    stampFreshnessAt(basePath, segments, overlayPanel)

  private def stampFreshnessAt(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit = {
    val now = js.Date.now()
    latestDataTime = now
    clearFreshUnder(basePath, segments, overlayPanel)
    fieldFreshAt.update(stampKey(basePath, segments, overlayPanel), now)
  }

  private def clearFreshUnder(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit = {
    val prefix = stampKey(basePath, segments, overlayPanel)
    fieldFreshAt.filterInPlace { case (k, _) =>
      !(k == prefix || k.startsWith(prefix + "/"))
    }
  }

  private def jsonPointer(segments: List[String]): String =
    if (segments.isEmpty) "" else segments.mkString("/", "/", "")

  private def stampKey(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): String = {
    val raw = basePath + jsonPointer(segments)
    if (overlayPanel) s"ov:$raw" else raw
  }

  /** Epoch ms when this subtree (or nearest ancestor) was last updated. */
  private def fieldFreshMs(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Double = {
    var segs = segments
    while (true) {
      fieldFreshAt.get(stampKey(basePath, segs, overlayPanel)) match {
        case Some(stamped) =>
          return stamped
        case None =>
          if (segs.isEmpty) return latestDataTime
          segs = segs.init
      }
    }
    latestDataTime
  }

  /** Age in ms: how far behind the latest data (or wall clock) this field is. */
  private def fieldAgeMs(freshMs: Double): Double = {
    val latest = math.max(latestDataTime, js.Date.now())
    math.max(0.0, latest - freshMs)
  }

  private def applyAgeAttributes(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit = {
    val freshMs = fieldFreshMs(basePath, segments, overlayPanel)
    line.setAttribute("data-fresh-ms", freshMs.toString)
    applyAgeStyle(line, fieldAgeMs(freshMs))
  }

  private def applyAgeStyle(line: HTMLElement, ageMs: Double): Unit = {
    val (opacity, tint) = ageVisual(ageMs)
    line.style.setProperty("--jv-fade", f"$opacity%.3f")
    line.style.setProperty("--jv-tint", f"$tint%.3f")
    if (ageMs >= StaleAfterMs) line.classList.add("jv-stale")
    else line.classList.remove("jv-stale")
  }

  /** Mild fade for the first minute; a clearer (but still readable) mute after that. */
  private def ageVisual(ageMs: Double): (Double, Double) = {
    val sec = ageMs / 1000.0
    if (sec <= 0) (1.0, 0.0)
    else if (sec < 60.0) {
      val t = sec / 60.0
      (1.0 - t * 0.10, t * 0.22)
    } else {
      val extra = math.min(1.0, (sec - 60.0) / 180.0)
      (0.82 - extra * 0.06, 0.34 + extra * 0.08)
    }
  }

  private def refreshVisibleAgeStyles(): Unit = {
    val lines = dom.document.querySelectorAll(".jv-line[data-fresh-ms]")
    var i = 0
    while (i < lines.length) {
      val line = lines.item(i).asInstanceOf[HTMLElement]
      val freshMs = Try(line.getAttribute("data-fresh-ms").toDouble).getOrElse(latestDataTime)
      applyAgeStyle(line, fieldAgeMs(freshMs))
      i += 1
    }
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
    // Keep open tabs; only reset empty messaging when nothing is open.
    if (openTabs.isEmpty) clearWorkspace()
    else {
      clearIndexBrowse()
    }
  }

  private def clearIndexBrowse(): Unit = {
    indexTemplate = None
    indexValues = Nil
    byId("index-bar").setAttribute("hidden", "true")
    setIndexStatus("", ok = true)
    persistActiveTabDraft()
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
    persistActiveTabDraft()
    setEditorStatus(s"Loading fields for $structId…", ok = true)

    def applyMembers(members: List[OverlayMemberUi]): Unit = {
      draftMembers =
        if (members.nonEmpty) members
        else List(OverlayMemberUi("field0", "u8", isArray = false, None, false))
      persistActiveTabDraft()
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
    persistActiveTabDraft()
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
          persistActiveTabDraft()
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
          persistActiveTabDraft()
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
        persistActiveTabDraft()
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
      persistActiveTabDraft()
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
      persistActiveTabDraft()
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
        persistActiveTabDraft()
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
            syncWatchesFromJsonList(
              json.hcursor.downField("watches").as[List[Json]].getOrElse(Nil)
            )
            json.hcursor
              .downField("watchErrors")
              .as[List[String]]
              .toOption
              .filter(_.nonEmpty)
              .foreach { errs =>
                setEditorStatus(
                  s"Overlays applied; watch rebind errors: ${errs.mkString("; ")}",
                  ok = false
                )
              }
            refreshOpenTabPayloads()
          case Left(err) =>
            setEditorStatus(s"Bad overlay response: ${err.getMessage}", ok = false)
        }
      case Failure(err) =>
        setEditorStatus(err.getMessage, ok = false)
    }
  }

  private def refreshOpenTabPayloads(): Unit = {
    val paths = openTabs.keys.toList
    if (paths.isEmpty) activeTabPath.orElse(selected).foreach(refreshPath)
    else paths.foreach(refreshPath)
  }

  private def syncWatchesFromJsonList(watches: List[Json]): Unit = {
    activeWatches.clear()
    watchOverlaySegments.clear()
    watches.foreach { w =>
      val c = w.hcursor
      (c.get[String]("path").toOption, c.get[Int]("watchId").toOption) match {
        case (Some(path), Some(watchId)) =>
          activeWatches.update(path, watchId)
          val overlaySegs =
            c.downField("overlaySegments").as[List[List[String]]].getOrElse(Nil)
          if (overlaySegs.nonEmpty) watchOverlaySegments.update(path, overlaySegs)
        case _ =>
          ()
      }
    }
    detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
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

  private def showToast(title: String, message: String): Unit = {
    val host = byId("toast-host")
    val toast = el("div")
    toast.className = "toast"
    toast.setAttribute("role", "status")

    val body = el("div")
    body.className = "toast-body"
    val titleEl = el("p")
    titleEl.className = "toast-title"
    titleEl.textContent = title
    val msgEl = el("p")
    msgEl.className = "toast-message"
    msgEl.textContent = message
    body.appendChild(titleEl)
    body.appendChild(msgEl)

    val dismiss = el("button").asInstanceOf[dom.html.Button]
    dismiss.className = "toast-dismiss"
    dismiss.setAttribute("type", "button")
    dismiss.textContent = "×"
    dismiss.title = "Dismiss"
    dismiss.onclick = (_: MouseEvent) => {
      val _ = host.removeChild(toast)
    }

    toast.appendChild(body)
    toast.appendChild(dismiss)
    host.appendChild(toast)
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

  private def postJson(path: String, payload: Json): Future[Json] = {
    val init = new RequestInit {
      method = HttpMethod.POST
      body = payload.noSpaces
      headers = js.Dictionary("Content-Type" -> "application/json")
    }
    dom.fetch(path, init).toFuture.flatMap { response =>
      response.text().toFuture.map { text =>
        parse(text) match {
          case Right(json) =>
            if (!response.ok) {
              val detail = json.hcursor
                .get[String]("message")
                .toOption
                .orElse(json.hcursor.get[String]("error").toOption)
                .getOrElse(s"HTTP ${response.status}")
              throw new RuntimeException(detail)
            }
            json
          case Left(err) =>
            throw new RuntimeException(
              if (!response.ok) s"HTTP ${response.status}: $text" else err.message
            )
        }
      }
    }
  }

  private def deleteJson(path: String): Future[Json] = {
    val init = new RequestInit {
      method = HttpMethod.DELETE
    }
    dom.fetch(path, init).toFuture.flatMap { response =>
      response.text().toFuture.map { text =>
        if (!response.ok)
          throw new RuntimeException(s"HTTP ${response.status}: $text")
        if (text.trim.isEmpty) Json.obj()
        else
          parse(text) match {
            case Right(json) => json
            case Left(err)   => throw new RuntimeException(err.message)
          }
      }
    }
  }
}
