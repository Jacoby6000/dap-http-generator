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
  ): Option[List[OverlayMember]] =
    document.structs
      .get(structId)
      .map(_.members)
      .orElse(
        document.newStructs
          .find(ns => ns.id == structId || s"overlay#${ns.id}" == structId)
          .map(_.members)
      )

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
    val updatedStructs = document.structs + (structId -> OverlayStructDef(members))
    val updatedNew = document.newStructs.map { ns =>
      val fullId = if (ns.id.contains("#")) ns.id else s"overlay#${ns.id}"
      if (fullId == structId || ns.id == structId) ns.copy(members = members)
      else ns
    }
    document.copy(structs = updatedStructs, newStructs = updatedNew)
  }

  def removeStructOverlay(
      document: TypeOverlayDocument,
      structId: String
  ): TypeOverlayDocument =
    document.copy(structs = document.structs - structId)

  def addNewStruct(
      document: TypeOverlayDocument,
      rawId: String,
      members: List[OverlayMember]
  ): Either[String, (TypeOverlayDocument, String)] = {
    val id = normalizeNewStructId(rawId)
    if (rawId.trim.isEmpty) Left("Enter a name for the new struct.")
    else if (document.newStructs.exists(ns => ns.id == rawId.trim || ns.id == id))
      Left(s"$id already exists.")
    else
      Right(
        document.copy(newStructs = document.newStructs :+ OverlayNewStruct(id, members)) -> id
      )
  }
}
