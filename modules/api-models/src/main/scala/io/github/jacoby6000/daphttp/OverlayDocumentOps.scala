package io.github.jacoby6000.daphttp

/** Pure overlay-document mutations used by the explorer struct editor. */
object OverlayDocumentOps {
  def normalizeNewStructId(raw: String): String = {
    val trimmed = raw.trim
    if (trimmed.contains("#")) trimmed else s"overlay#$trimmed"
  }

  def membersForStruct(
      document: TypeOverlayDocument,
      structId: String
  ): Option[List[OverlayMember]] = {
    val trimmed = structId.trim
    val normalizedNew = normalizeNewStructId(trimmed)
    val matchesNew =
      document.newStructs.exists(ns => normalizeNewStructId(ns.id) == normalizedNew)
    document.structs
      .get(trimmed)
      .orElse(if (matchesNew) document.structs.get(normalizedNew) else None)
      .map(_.members)
      .orElse(
        document.newStructs
          .find(ns => normalizeNewStructId(ns.id) == normalizedNew)
          .map(_.members)
      )
  }

  def validateDraftMembers(members: List[OverlayMember]): Option[String] =
    if (members.isEmpty || members.exists(_.name.trim.isEmpty))
      Some("Each field needs a non-empty name.")
    else if (members.exists(_.typeId.trim.isEmpty))
      Some("Each field needs a typeId.")
    else None

  def applyStructMembers(
      document: TypeOverlayDocument,
      structId: String,
      members: List[OverlayMember]
  ): TypeOverlayDocument = {
    val trimmed = structId.trim
    val normalizedNew = normalizeNewStructId(trimmed)
    val matchesNew =
      document.newStructs.exists(ns => normalizeNewStructId(ns.id) == normalizedNew)
    // DESNOTE(jbarber, 2026-07-21): Only force `overlay#…` when editing a client-created
    // struct. Unqualified IR names (e.g. `Player`) must keep their raw key so PUT
    // normalizeOverlayKeys / TypeOverlay.structDefFor can resolve them to `ns#Player`.
    val key = if (matchesNew) normalizedNew else trimmed
    val updatedStructs =
      (document.structs - trimmed - normalizedNew) + (key -> OverlayStructDef(members))
    val updatedNew = document.newStructs.map { ns =>
      if (normalizeNewStructId(ns.id) == normalizedNew) ns.copy(members = members)
      else ns
    }
    document.copy(structs = updatedStructs, newStructs = updatedNew)
  }

  def removeStructOverlay(
      document: TypeOverlayDocument,
      structId: String
  ): TypeOverlayDocument = {
    val trimmed = structId.trim
    val normalizedNew = normalizeNewStructId(trimmed)
    val matchesNew =
      document.newStructs.exists(ns => normalizeNewStructId(ns.id) == normalizedNew)
    // DESNOTE(jbarber, 2026-07-21): Apply dual-writes newStruct edits into `structs` and
    // `newStructs`. Reset of a client-created type must clear both (and both spellings of the
    // structs key). Reset of an IR overlay only drops structs entries — never a coincidental
    // newStruct that shares an unqualified name.
    if (matchesNew)
      document.copy(
        structs = document.structs - trimmed - normalizedNew,
        newStructs = document.newStructs.filterNot { ns =>
          normalizeNewStructId(ns.id) == normalizedNew
        }
      )
    else
      document.copy(structs = document.structs - trimmed)
  }

  def addNewStruct(
      document: TypeOverlayDocument,
      rawId: String,
      members: List[OverlayMember]
  ): Either[String, (TypeOverlayDocument, String)] = {
    val id = normalizeNewStructId(rawId)
    if (rawId.trim.isEmpty) Left("Enter a name for the new struct.")
    else if (document.newStructs.exists(ns => normalizeNewStructId(ns.id) == id))
      Left(s"$id already exists.")
    else
      Right(
        document.copy(newStructs = document.newStructs :+ OverlayNewStruct(id, members)) -> id
      )
  }
}
