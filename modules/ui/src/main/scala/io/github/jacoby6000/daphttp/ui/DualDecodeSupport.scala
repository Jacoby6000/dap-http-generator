package io.github.jacoby6000.daphttp.ui

import io.circe.Json
import io.github.jacoby6000.daphttp.DapAddress
import io.github.jacoby6000.daphttp.DecodedPayload
import io.github.jacoby6000.daphttp.DualChild
import io.github.jacoby6000.daphttp.DualDecodeAlign
import io.github.jacoby6000.daphttp.FieldFreshness
import io.github.jacoby6000.daphttp.JsonPath
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.MouseEvent

/** Dual source/overlay JSON tree rendering and in-place watch patches.
  *
  * Mixed into [[Main]]; DOM helpers and field actions remain on the host.
  */
private[ui] trait DualDecodeSupport {
  protected def el(tag: String): HTMLElement
  protected def byId(id: String): HTMLElement
  protected def payloads: scala.collection.mutable.Map[String, Json]
  protected def jsonExpanded: scala.collection.mutable.Set[String]
  protected def dualLineByStamp: scala.collection.mutable.Map[String, HTMLElement]
  protected def dualValueByStamp: scala.collection.mutable.Map[String, HTMLElement]
  protected def pendingPatchFocus
      : scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]
  protected def patchRafScheduled: Boolean
  protected def patchRafScheduled_=(value: Boolean): Unit
  protected def cachedFetchablePaths: Set[String]
  protected def stampKey(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): String
  protected def jsonPrimitiveClass(json: Json): String
  protected def jsonPrimitiveText(json: Json): String
  protected def applyAgeAttributes(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit
  protected def makeValueEditable(
      leafEl: HTMLElement,
      address: Option[Long],
      segments: List[String],
      overlayPanel: Boolean,
      current: Json
  ): Unit
  protected def appendFieldActions(
      row: HTMLElement,
      basePath: String,
      segments: List[String],
      fetchable: Set[String],
      overlayPanel: Boolean
  ): Unit
  protected def appendPointerFocus(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      source: Option[Json],
      overlay: Option[Json]
  ): Unit

  protected def setDualPlain(
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
    val _ = host.appendChild(row)
  }

  protected def setDualDecodeView(
      basePath: String,
      source: Json,
      overlay: Option[Json]
  ): Unit = {
    dualLineByStamp.clear()
    dualValueByStamp.clear()
    val host = byId("detail-dual")
    host.innerHTML = ""
    host.className = "json-view dual-scroll"
    val _ = host.appendChild(
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

  protected def repaintJsonViews(basePath: String): Unit =
    payloads.get(basePath).foreach { payload =>
      setDualDecodeView(
        basePath,
        DecodedPayload.extractDecoded(payload),
        DecodedPayload.extractOverlayDecoded(payload)
      )
    }

  // DESNOTE(jbarber, 2026-07-20): Rapid watch updates used to call setDualDecodeView and
  // recreate every button. Clicks on ◎ never completed (mousedown target destroyed before
  // mouseup). Patch leaf text/age in place; fall back to a full rebuild only on shape change.
  // Coalesce to animation frames and scope to the watched subtree when possible — DAP can
  // stream far faster than walking a full Melee struct (or player_slots[]) in the DOM.
  protected def patchJsonViews(basePath: String, focusSegments: List[String] = Nil): Unit = {
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

  protected def flushPatchJsonViews(basePath: String, focusSegments: List[String]): Unit =
    payloads.get(basePath).foreach { payload =>
      val source = DecodedPayload.extractDecoded(payload)
      val overlay = DecodedPayload.extractOverlayDecoded(payload)
      if (
        dualLineByStamp.isEmpty ||
        !patchDualDecodeView(basePath, source, overlay, focusSegments)
      )
        setDualDecodeView(basePath, source, overlay)
      else
        refreshDualAges(basePath, focusSegments)
    }

  protected def patchDualDecodeView(
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

  protected def refreshDualAges(basePath: String, focusSegments: List[String]): Unit = {
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

  protected def trackDualLine(
      line: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit = {
    applyAgeAttributes(line, basePath, segments, overlayPanel)
    dualLineByStamp.update(stampKey(basePath, segments, overlayPanel), line)
  }

  protected def trackDualValue(
      value: HTMLElement,
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit =
    dualValueByStamp.update(stampKey(basePath, segments, overlayPanel), value)

  /** Shared expand key so renamed source/overlay fields still toggle together. */
  protected def dualStampKey(
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String]
  ): String =
    FieldFreshness.dualStampKey(basePath, sourceSegments, overlaySegments)

  protected def getAtPath(json: Json, segments: List[String]): Option[Json] =
    JsonPath.get(json, segments)

  protected def dualObjectFieldCount(obj: io.circe.JsonObject): Int =
    obj.keys.count(k => !DualDecodeAlign.isMetaKey(k))

  protected def alignDualChildren(source: Option[Json], overlay: Option[Json]): List[DualChild] = {
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

  protected def structAbsoluteAddress(json: Option[Json]): Option[Long] =
    json
      .flatMap(_.asObject)
      .flatMap(_("_address"))
      .flatMap(_.asString)
      .flatMap(DapAddress.parse)

  protected def memberAbsoluteAddress(
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

  protected def renderDualNode(
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

  protected def renderDualLeaf(
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

  protected def renderDualComposite(
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
              val srcAt = getAtPath(DecodedPayload.extractDecoded(payload), sourceSegments)
              val ovAt =
                DecodedPayload.extractOverlayDecoded(payload).flatMap(getAtPath(_, overlaySegments))
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
          val _ = root.appendChild(closeRow)
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
}
