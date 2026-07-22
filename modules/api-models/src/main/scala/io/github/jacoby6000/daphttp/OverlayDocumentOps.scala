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
    val normalized = normalizeNewStructId(structId)
    document.structs
      .get(structId)
      .orElse(document.structs.get(normalized))
      .map(_.members)
      .orElse(
        document.newStructs
          .find(ns => normalizeNewStructId(ns.id) == normalized)
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
    val normalized = normalizeNewStructId(structId)
    // DESNOTE(jbarber, 2026-07-21): Always persist under the normalized key and drop the
    // raw/alternate spelling so Apply with `Foo` cannot leave a second `overlay#Foo` entry
    // (or the reverse) that Reset and TypeOverlay lookup would disagree about.
    val updatedStructs =
      (document.structs - structId - normalized) + (normalized -> OverlayStructDef(members))
    val updatedNew = document.newStructs.map { ns =>
      if (normalizeNewStructId(ns.id) == normalized) ns.copy(members = members)
      else ns
    }
    document.copy(structs = updatedStructs, newStructs = updatedNew)
  }

  def removeStructOverlay(
      document: TypeOverlayDocument,
      structId: String
  ): TypeOverlayDocument = {
    val normalized = normalizeNewStructId(structId)
    document.copy(
      // DESNOTE(jbarber, 2026-07-21): Drop both the raw and normalized keys so Reset with
      // `Foo` clears an Apply that stored `overlay#Foo` (and the reverse). Apply dual-writes
      // newStruct edits into `structs` and `newStructs`; Reset must clear matching newStructs
      // too or reinterpretation keeps the applied members via the newStructs fallback.
      structs = document.structs - structId - normalized,
      newStructs = document.newStructs.filterNot { ns =>
        normalizeNewStructId(ns.id) == normalized
      }
    )
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
