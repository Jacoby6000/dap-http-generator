package io.github.jacoby6000.daphttp.ui

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import io.github.jacoby6000.daphttp.DapAddress
import io.github.jacoby6000.daphttp.DecodedPayload
import io.github.jacoby6000.daphttp.DualDecodeAlign
import io.github.jacoby6000.daphttp.FetchableRoutePath
import io.github.jacoby6000.daphttp.FieldFreshness
import io.github.jacoby6000.daphttp.IndexPath
import io.github.jacoby6000.daphttp.JsonPath
import io.github.jacoby6000.daphttp.JsonPrimitiveDisplay
import io.github.jacoby6000.daphttp.OverlayMember
import io.github.jacoby6000.daphttp.RouteTreeNode
import io.github.jacoby6000.daphttp.RoutesResponse
import io.github.jacoby6000.daphttp.TypeCatalogEntry
import io.github.jacoby6000.daphttp.TypeOverlayDocument
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.RequestInit
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

import scala.annotation.unused
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success
import scala.util.Try

final case class OpenTab(
    path: String,
    decodeType: Option[String] = None,
    editingStructId: Option[String] = None,
    draftMembers: List[OverlayMember] = Nil,
    editorOpen: Boolean = false,
    indexTemplate: Option[String] = None,
    indexValues: List[Int] = Nil
)

object Main extends DualDecodeSupport with OverlayEditorSupport with WatchClientSupport {
  private var catalog: List[RouteTreeNode] = Nil
  private var visible: List[RouteTreeNode] = Nil
  private val expanded = scala.collection.mutable.Set.empty[String]
  protected val payloads = scala.collection.mutable.Map.empty[String, Json]
  private val loading = scala.collection.mutable.Set.empty[String]
  private val loadErrors = scala.collection.mutable.Map.empty[String, String]
  private var selected: Option[String] = None
  private var activeQuery: String = ""
  protected var typeCatalog: List[TypeCatalogEntry] = Nil
  protected var overlays: TypeOverlayDocument = TypeOverlayDocument.empty

  /** Open workspace tabs (insertion order). */
  private val openTabs = scala.collection.mutable.LinkedHashMap.empty[String, OpenTab]

  /** Active tab path — drives decode/editor panes and watch repaint targeting. */
  private var activeTabPath: Option[String] = None

  // Working copies for the active tab (synced via saveActiveTabDraft / restoreActiveTabEditor).
  protected var editingStructId: Option[String] = None
  protected var draftMembers: List[OverlayMember] = Nil
  private var lastDecodeType: Option[String] = None
  protected var editorOpen: Boolean = false

  /** Json-pointer path → epoch ms when that subtree was last refreshed. */
  private val fieldFreshAt = scala.collection.mutable.Map.empty[String, Double]

  /** Most recent successful memory update time (epoch ms). */
  private var latestDataTime: Double = js.Date.now()

  private val fieldLoading = scala.collection.mutable.Set.empty[String]
  protected var cachedFetchablePaths: Set[String] = Set.empty

  /** Expanded JSON composite paths (`stampKey`) — collapsed nodes are not built in the DOM. */
  protected val jsonExpanded = scala.collection.mutable.Set.empty[String]

  /** Dual-view `.jv-line` / value nodes by stampKey — watch updates patch these in place. */
  protected val dualLineByStamp = scala.collection.mutable.Map.empty[String, HTMLElement]
  protected val dualValueByStamp = scala.collection.mutable.Map.empty[String, HTMLElement]

  /** Coalesce rapid watch patches into one rAF flush (avoids multi-kHz DOM work). Value is a set of
    * focus keys (`""` = whole tree, else `seg/seg` under the base path).
    */
  protected val pendingPatchFocus =
    scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
  protected var patchRafScheduled = false

  /** Active realtime watches: HTTP path → server watchId. */
  protected val activeWatches = scala.collection.mutable.Map.empty[String, Int]

  /** Source watch path → overlay JSON segment paths co-watched via byte-range overlap. */
  protected val watchOverlaySegments =
    scala.collection.mutable.Map.empty[String, List[List[String]]]

  protected var watchSocket: Option[dom.WebSocket] = None

  /** Soft ceiling: fields older than this (relative to latest data / now) look distinctly muted. */
  private val StaleAfterMs = FieldFreshness.StaleAfterMs

  /** Template path containing `{index}` slots currently being browsed. */
  private var indexTemplate: Option[String] = None

  /** Current index values for `indexTemplate` (or concrete indexed path). */
  private var indexValues: List[Int] = Nil

  protected def detailViewPath: Option[String] = activeTabPath

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
      draftMembers = draftMembers :+ OverlayMember("field", "u8", isArray = false, None, false)
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
    val _ = js.timers.setInterval(2000)(refreshVisibleAgeStyles())
  }

  protected def loadTypesAndOverlays(): Unit = {
    fetchJson("/types").foreach { json =>
      json.hcursor.downField("types").as[List[TypeCatalogEntry]].foreach { entries =>
        typeCatalog = entries
        renderTypeDatalists()
      }
    }
    fetchJson("/overlays").foreach { json =>
      json.as[TypeOverlayDocument].foreach { doc =>
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
      DapAddress.parse(trimmed) match {
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
        if (DecodedPayload.decodeFailed(json)) {
          loadErrors.update(path, DecodedPayload.decodeErrorMessage(json))
          selected = Some(path)
          renderResults()
          // Errors do not open tabs — keep tree error badge; leave workspace as-is.
          if (openTabs.isEmpty) showErrorDetail(path, DecodedPayload.decodeErrorMessage(json))
          setIndexStatus(s"End of chain / decode failed at $path", ok = false)
        } else {
          payloads.update(path, json)
          bumpRefreshFreshness(
            path,
            DecodedPayload.extractDecoded(json),
            DecodedPayload.extractOverlayDecoded(json)
          )
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
      val _ = root.appendChild(ul)
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
      addrEl.textContent = s" ${DapAddress.format(addr)}"
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

  protected def showDetail(path: String, json: Json): Unit = {
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

  protected def persistActiveTabDraft(): Unit =
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

  protected def appendFieldActions(
      row: HTMLElement,
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      overlayPanel: Boolean
  ): Unit = {
    def prepend(node: HTMLElement): Unit =
      Option(row.firstChild) match {
        case Some(first) => val _ = row.insertBefore(node, first)
        case None        => val _ = row.appendChild(node)
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

  protected def makeValueEditable(
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
      leafEl.title = s"Double-click to edit (${DapAddress.format(addr)})"
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
          val _ = parent.replaceChild(leafEl, input)
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
          "address" -> Json.fromString(DapAddress.format(address)),
          "value" -> value,
          "decodeType" -> Json.fromString(dt),
          "segments" -> writeSegs.asJson,
          "overlay" -> Json.fromBoolean(overlayPanel)
        )
        setIndexStatus(s"Writing ${DapAddress.format(address)}…", ok = true)
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
              setIndexStatus(s"Wrote ${DapAddress.format(address)}", ok = true)
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

  protected def appendPointerFocus(
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
  ): Option[String] =
    FetchableRoutePath.httpPathForField(basePath, segments, fetchable)

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
        if (errorOpt.isDefined || DecodedPayload.decodeFailed(json)) {
          val err = errorOpt.getOrElse(DecodedPayload.decodeErrorMessage(json))
          setIndexStatus(s"Field refresh failed: $err", ok = false)
          setFieldRefreshBusy(httpPath, busy = false)
        } else {
          payloads.update(httpPath, json)
          payloads.get(basePath) match {
            case None =>
              setIndexStatus(s"Parent payload missing for $basePath", ok = false)
              setFieldRefreshBusy(httpPath, busy = false)
            case Some(parent) =>
              val fieldDecoded = DecodedPayload.extractDecoded(json)
              val fieldOverlay = DecodedPayload.extractOverlayDecoded(json)
              val mergedDecoded =
                JsonPath.replace(DecodedPayload.extractDecoded(parent), segments, fieldDecoded)
              val mergedOverlay =
                (DecodedPayload.extractOverlayDecoded(parent), fieldOverlay) match {
                  case (Some(rootOverlay), Some(piece)) =>
                    Some(JsonPath.replace(rootOverlay, segments, piece))
                  case (rootOverlay, _) => rootOverlay
                }
              payloads.update(
                basePath,
                DecodedPayload.writeDecodedFields(parent, mergedDecoded, mergedOverlay)
              )
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

  protected def bumpRefreshFreshnessAt(
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

  protected def stampKey(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): String =
    FieldFreshness.stampKey(basePath, segments, overlayPanel)

  /** Epoch ms when this subtree (or nearest ancestor) was last updated. */
  private def fieldFreshMs(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Double =
    FieldFreshness.resolveFreshMs(
      segs => fieldFreshAt.get(stampKey(basePath, segs, overlayPanel)),
      segments,
      latestDataTime
    )

  /** Age in ms: how far behind the latest data (or wall clock) this field is. */
  private def fieldAgeMs(freshMs: Double): Double =
    FieldFreshness.ageMs(freshMs, latestDataTime, js.Date.now())

  protected def applyAgeAttributes(
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
    val (opacity, tint) = FieldFreshness.ageVisual(ageMs)
    line.style.setProperty("--jv-fade", f"$opacity%.3f")
    line.style.setProperty("--jv-tint", f"$tint%.3f")
    if (ageMs >= StaleAfterMs) line.classList.add("jv-stale")
    else line.classList.remove("jv-stale")
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

  protected def jsonPrimitiveClass(json: Json): String =
    JsonPrimitiveDisplay.cssClass(json)

  protected def jsonPrimitiveText(json: Json): String =
    JsonPrimitiveDisplay.text(json)

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
        val slotCount = IndexPath.countSlots(template)
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
          if (indices.length == IndexPath.countSlots(template)) indices
          else List.fill(IndexPath.countSlots(template))(0)
        indexValues = normalized
        renderIndexBar()
        val path = IndexPath.substitute(template, normalized)
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

  protected def setIndexStatus(message: String, ok: Boolean): Unit = {
    val status = byId("index-status")
    status.textContent = message
    status.className = "index-status" + (if (message.isEmpty) "" else if (ok) " ok" else " err")
  }

  private def resolveIndexBrowse(path: String): Option[(String, List[Int])] = {
    val templates = collectNodes(catalog).map(_.path).filter(_.contains("{index}")).distinct
    IndexPath.resolveBrowse(path, templates, indexTemplate, indexValues)
  }

  private def concretePathForSelection(path: String): String =
    IndexPath.concretePath(path, indexTemplate, indexValues)

  protected def refreshOpenTabPayloads(): Unit = {
    val paths = openTabs.keys.toList
    if (paths.isEmpty) activeTabPath.orElse(selected).foreach(refreshPath)
    else paths.foreach(refreshPath)
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
    val _ = host.appendChild(toast)
  }

  private def shortenPath(path: String): String =
    if (path.startsWith("/api/")) path.stripPrefix("/api/") else path

  private def collectNodes(nodes: List[RouteTreeNode]): List[RouteTreeNode] =
    nodes.flatMap(n => n :: collectNodes(n.children))

  private def searchInput(): HTMLInputElement =
    dom.document.getElementById("route-search").asInstanceOf[HTMLInputElement]

  protected def inputById(id: String): HTMLInputElement =
    dom.document.getElementById(id).asInstanceOf[HTMLInputElement]

  protected def inputEl(): HTMLInputElement =
    dom.document.createElement("input").asInstanceOf[HTMLInputElement]

  protected def byId(id: String): HTMLElement =
    dom.document.getElementById(id).asInstanceOf[HTMLElement]

  protected def el(tag: String): HTMLElement =
    dom.document.createElement(tag).asInstanceOf[HTMLElement]

  protected def fetchJson(path: String): Future[Json] =
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

  protected def putJson(path: String, payload: Json): Future[Json] = {
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

  protected def postJson(path: String, payload: Json): Future[Json] = {
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

  protected def deleteJson(path: String): Future[Json] = {
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
