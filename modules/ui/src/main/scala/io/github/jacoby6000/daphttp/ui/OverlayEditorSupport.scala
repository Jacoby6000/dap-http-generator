package io.github.jacoby6000.daphttp.ui

import io.circe.Json
import io.circe.syntax._
import io.github.jacoby6000.daphttp.OverlayDocumentOps
import io.github.jacoby6000.daphttp.OverlayMember
import io.github.jacoby6000.daphttp.TypeCatalogEntry
import io.github.jacoby6000.daphttp.TypeOverlayDocument
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.MouseEvent
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Struct overlay editor DOM and document apply/create/reset flows.
  *
  * Mixed into [[Main]]; HTTP helpers and watch rebinding stay on the host.
  */
private[ui] trait OverlayEditorSupport {
  protected def el(tag: String): HTMLElement
  protected def byId(id: String): HTMLElement
  protected def inputById(id: String): HTMLInputElement
  protected def inputEl(): HTMLInputElement
  protected def overlays: TypeOverlayDocument
  protected def overlays_=(value: TypeOverlayDocument): Unit
  protected def draftMembers: List[OverlayMember]
  protected def draftMembers_=(value: List[OverlayMember]): Unit
  protected def editingStructId: Option[String]
  protected def editingStructId_=(value: Option[String]): Unit
  protected def editorOpen: Boolean
  protected def editorOpen_=(value: Boolean): Unit
  protected def typeCatalog: List[TypeCatalogEntry]
  protected def typeCatalog_=(value: List[TypeCatalogEntry]): Unit
  protected def persistActiveTabDraft(): Unit
  protected def putJson(path: String, body: Json): Future[Json]
  protected def fetchJson(path: String): Future[Json]
  protected def loadTypesAndOverlays(): Unit
  protected def syncWatchesFromJsonList(watches: List[Json]): Unit
  protected def refreshOpenTabPayloads(): Unit

  protected def loadDraftForStruct(structId: String): Unit = {
    editingStructId = Some(structId)
    inputById("edit-struct").value = structId
    persistActiveTabDraft()
    setEditorStatus(s"Loading fields for $structId…", ok = true)

    def applyMembers(members: List[OverlayMember]): Unit = {
      draftMembers =
        if (members.nonEmpty) members
        else List(OverlayMember("field0", "u8", isArray = false, None, false))
      persistActiveTabDraft()
      renderFieldEditor()
      setEditorStatus(s"Loaded ${draftMembers.size} field(s).", ok = true)
    }

    OverlayDocumentOps.membersForStruct(overlays, structId) match {
      case Some(members) =>
        applyMembers(members)
      case None =>
        fetchJson(s"/types/fields?id=${js.URIUtils.encodeURIComponent(structId)}").onComplete {
          case Success(json) =>
            json.hcursor.downField("fields").as[List[OverlayMember]] match {
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

  protected def setEditorOpen(open: Boolean): Unit = {
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

  protected def renderTypeDatalists(): Unit = {
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

  protected def renderFieldEditor(): Unit = {
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
  protected def syncDraftFromDom(): Unit = {
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
      else Some(OverlayMember(name, typeId, isArray, arrayLength, isPointer))
    }.toList
    if (synced.nonEmpty || rows.length == 0) {
      draftMembers = synced
      persistActiveTabDraft()
    }
  }

  protected def applyOverlay(): Unit = {
    syncDraftFromDom()
    val structId = inputById("edit-struct").value.trim
    if (structId.isEmpty) {
      setEditorStatus("Select a struct to edit.", ok = false)
    } else {
      OverlayDocumentOps.validateDraftMembers(draftMembers) match {
        case Some(err) =>
          setEditorStatus(err, ok = false)
        case None =>
          editingStructId = Some(structId)
          persistActiveTabDraft()
          putOverlays(OverlayDocumentOps.applyStructMembers(overlays, structId, draftMembers))
      }
    }
  }

  protected def resetCurrentStruct(): Unit = {
    editingStructId match {
      case None =>
        setEditorStatus("Select a struct to reset.", ok = false)
      case Some(structId) =>
        putOverlays(OverlayDocumentOps.removeStructOverlay(overlays, structId))
    }
  }

  protected def createNewStruct(): Unit = {
    val raw = inputById("new-struct-id").value.trim
    OverlayDocumentOps.addNewStruct(
      overlays,
      raw,
      List(OverlayMember("field0", "u8", isArray = false, None, false))
    ) match {
      case Left(err) =>
        setEditorStatus(err, ok = false)
      case Right((next, id)) =>
        val members = next.newStructs.find(_.id == id).map(_.members).getOrElse(Nil)
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

  protected def putOverlays(document: TypeOverlayDocument): Unit = {
    setEditorStatus("Saving overlays…", ok = true)
    putJson("/overlays", document.asJson).onComplete {
      case Success(json) =>
        json.as[TypeOverlayDocument] match {
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

  protected def setEditorStatus(message: String, ok: Boolean): Unit = {
    val elStatus = byId("editor-status")
    elStatus.textContent = message
    elStatus.className = if (ok) "ok" else "err"
  }

}
