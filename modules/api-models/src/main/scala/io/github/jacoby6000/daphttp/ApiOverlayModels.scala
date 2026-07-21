package io.github.jacoby6000.daphttp

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._

/** Client-driven struct reinterpretation overlays (wire / persistence JSON). */
final case class OverlayMember(
    name: String,
    typeId: String,
    isArray: Boolean = false,
    arrayLength: Option[Int] = None,
    isPointer: Boolean = false
)

object OverlayMember {
  implicit val decoder: Decoder[OverlayMember] = Decoder.instance { c =>
    for {
      name <- c.get[String]("name")
      typeId <- c.get[String]("typeId")
      isArray <- c.getOrElse[Boolean]("isArray")(false)
      arrayLength <- c.get[Option[Int]]("arrayLength")
      isPointer <- c.getOrElse[Boolean]("isPointer")(false)
    } yield OverlayMember(name, typeId, isArray, arrayLength, isPointer)
  }

  implicit val encoder: Encoder[OverlayMember] = Encoder.instance { m =>
    Json.obj(
      "name" -> Json.fromString(m.name),
      "typeId" -> Json.fromString(m.typeId),
      "isArray" -> Json.fromBoolean(m.isArray),
      "arrayLength" -> m.arrayLength.fold(Json.Null)(Json.fromInt),
      "isPointer" -> Json.fromBoolean(m.isPointer)
    )
  }
}

final case class OverlayStructDef(members: List[OverlayMember])

object OverlayStructDef {
  implicit val decoder: Decoder[OverlayStructDef] =
    Decoder.instance(_.get[List[OverlayMember]]("members").map(OverlayStructDef.apply))

  implicit val encoder: Encoder[OverlayStructDef] =
    Encoder.instance(d => Json.obj("members" -> d.members.asJson))
}

final case class OverlayNewStruct(id: String, members: List[OverlayMember])

object OverlayNewStruct {
  implicit val decoder: Decoder[OverlayNewStruct] = Decoder.instance { c =>
    for {
      id <- c.get[String]("id")
      members <- c.get[List[OverlayMember]]("members")
    } yield OverlayNewStruct(id, members)
  }

  implicit val encoder: Encoder[OverlayNewStruct] = Encoder.instance { s =>
    Json.obj("id" -> Json.fromString(s.id), "members" -> s.members.asJson)
  }
}

final case class TypeOverlayDocument(
    structs: Map[String, OverlayStructDef] = Map.empty,
    newStructs: List[OverlayNewStruct] = Nil
)

object TypeOverlayDocument {
  val empty: TypeOverlayDocument = TypeOverlayDocument()

  // DESNOTE(jbarber, 2026-07-21): Aliases kept for call sites that referenced the old
  // TypeOverlayDocument.overlayMemberEncoder name before codecs moved onto OverlayMember.
  implicit val overlayMemberDecoder: Decoder[OverlayMember] = OverlayMember.decoder
  implicit val overlayMemberEncoder: Encoder[OverlayMember] = OverlayMember.encoder

  implicit val overlayStructDefDecoder: Decoder[OverlayStructDef] = OverlayStructDef.decoder
  implicit val overlayStructDefEncoder: Encoder[OverlayStructDef] = OverlayStructDef.encoder
  implicit val overlayNewStructDecoder: Decoder[OverlayNewStruct] = OverlayNewStruct.decoder
  implicit val overlayNewStructEncoder: Encoder[OverlayNewStruct] = OverlayNewStruct.encoder

  implicit val documentDecoder: Decoder[TypeOverlayDocument] = Decoder.instance { c =>
    for {
      structs <- c.getOrElse[Map[String, OverlayStructDef]]("structs")(Map.empty)
      newStructs <- c.getOrElse[List[OverlayNewStruct]]("newStructs")(Nil)
    } yield TypeOverlayDocument(structs, newStructs)
  }

  implicit val documentEncoder: Encoder[TypeOverlayDocument] = Encoder.instance { d =>
    Json.obj(
      "structs" -> d.structs.asJson,
      "newStructs" -> d.newStructs.asJson
    )
  }
}

final case class TypeCatalogEntry(
    id: String,
    kind: String,
    /** Member names (legacy summary). Prefer `fields` for full editor pre-population. */
    members: Option[List[String]] = None,
    fields: Option[List[OverlayMember]] = None
)

object TypeCatalogEntry {
  implicit val encoder: Encoder[TypeCatalogEntry] = Encoder.instance { e =>
    Json.obj(
      Seq(
        Some("id" -> Json.fromString(e.id)),
        Some("kind" -> Json.fromString(e.kind)),
        e.members.map(ms => "members" -> ms.asJson),
        e.fields.map(fs => "fields" -> fs.asJson)
      ).flatten: _*
    )
  }

  implicit val decoder: Decoder[TypeCatalogEntry] = Decoder.instance { c =>
    for {
      id <- c.get[String]("id")
      kind <- c.get[String]("kind")
      members <- c.get[Option[List[String]]]("members")
      fields <- c.get[Option[List[OverlayMember]]]("fields")
    } yield TypeCatalogEntry(id, kind, members, fields)
  }
}
