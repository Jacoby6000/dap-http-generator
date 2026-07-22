package io.github.jacoby6000.daphttp

/** Pure overlay-document mutations used by the explorer struct editor. */
object OverlayDocumentOps {
  def normalizeNewStructId(raw: String): String = {
    val trimmed = raw.trim
    if (trimmed.contains("#")) trimmed else s"overlay#$trimmed"
  }

  /** Keys in `structs` that refer to the same editor target as `structId`.
    *
    * After `PUT /overlays`, unqualified IR names are rewritten to `ns#Name`. The explorer may still
    * hold the short id, so lookup/reset must recognize the namespaced form. Client-created
    * `overlay#…` keys are only included when that newStruct exists.
    */
  def matchingStructKeys(document: TypeOverlayDocument, structId: String): List[String] = {
    val trimmed = structId.trim
    val normalizedNew = normalizeNewStructId(trimmed)
    val matchesNew =
      document.newStructs.exists(ns => normalizeNewStructId(ns.id) == normalizedNew)
    document.structs.keys.filter { key =>
      key == trimmed ||
      (matchesNew && key == normalizedNew) ||
      (!trimmed.contains("#") && key.endsWith("#" + trimmed) &&
        (matchesNew || !key.startsWith("overlay#")))
    }.toList
  }

  def membersForStruct(
      document: TypeOverlayDocument,
      structId: String
  ): Option[List[OverlayMember]] = {
    val trimmed = structId.trim
    val normalizedNew = normalizeNewStructId(trimmed)
    matchingStructKeys(document, trimmed).headOption
      .flatMap(document.structs.get)
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
    // struct. Unqualified IR names keep/reuse an existing `ns#Name` key when present so PUT
    // round-trips and TypeOverlay.structDefFor stay aligned.
    val existing = matchingStructKeys(document, trimmed)
    val key =
      if (matchesNew) normalizedNew
      else existing.headOption.getOrElse(trimmed)
    val updatedStructs =
      (document.structs -- existing - trimmed - normalizedNew) + (key -> OverlayStructDef(members))
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
    val keys = matchingStructKeys(document, trimmed)
    // DESNOTE(jbarber, 2026-07-21): Apply dual-writes newStruct edits into `structs` and
    // `newStructs`. Reset of a client-created type must clear both. Reset of an IR overlay
    // drops every matching structs key (short or `ns#Name`) without deleting a coincidental
    // newStruct that shares the unqualified name.
    if (matchesNew)
      document.copy(
        structs = document.structs -- keys - trimmed - normalizedNew,
        newStructs = document.newStructs.filterNot { ns =>
          normalizeNewStructId(ns.id) == normalizedNew
        }
      )
    else
      document.copy(structs = document.structs -- keys)
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
