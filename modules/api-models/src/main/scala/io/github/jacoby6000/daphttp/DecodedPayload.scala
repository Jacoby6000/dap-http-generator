package io.github.jacoby6000.daphttp

import io.circe.Json

/** Read/write `decoded` / `overlayDecoded` on DAP HTTP response envelopes. */
object DecodedPayload {
  def extractDecoded(json: Json): Json = {
    val cursor = json.hcursor
    val reads = cursor.downField("reads")
    if (reads.succeeded)
      reads.downArray.downField("decoded").focus.getOrElse(Json.Null)
    else
      cursor.downField("decoded").focus.getOrElse(Json.Null)
  }

  def extractOverlayDecoded(json: Json): Option[Json] = {
    val cursor = json.hcursor
    val reads = cursor.downField("reads")
    if (reads.succeeded) reads.downArray.downField("overlayDecoded").focus
    else cursor.downField("overlayDecoded").focus
  }

  def writeDecodedFields(
      payload: Json,
      decoded: Json,
      overlay: Option[Json]
  ): Json = {
    def patchObject(base: Json): Json = {
      val withDecoded = base.mapObject(_.add("decoded", decoded))
      overlay match {
        case Some(od) =>
          withDecoded.mapObject(_.add("overlayDecoded", od))
        case None =>
          if (base.hcursor.downField("overlayDecoded").succeeded)
            withDecoded.mapObject(_.remove("overlayDecoded"))
          else withDecoded
      }
    }

    payload.hcursor.downField("reads").focus match {
      case Some(readsJson) =>
        readsJson.asArray match {
          case Some(reads) if reads.nonEmpty =>
            payload.mapObject(
              _.add("reads", Json.fromValues(patchObject(reads.head) +: reads.tail.toList))
            )
          case Some(_) =>
            // DESNOTE(jbarber, 2026-07-21): Empty `reads: []` is still a reads envelope;
            // writing top-level decoded here would be invisible to extractDecoded.
            payload.mapObject(_.add("reads", Json.fromValues(List(patchObject(Json.obj())))))
          case None =>
            patchObject(payload)
        }
      case None =>
        patchObject(payload)
    }
  }

  def decodeFailed(json: Json): Boolean = {
    val cursor = json.hcursor
    val topError = cursor.get[String]("error").toOption.exists(_.nonEmpty)
    cursor.downField("reads").focus match {
      case Some(readsJson) =>
        readsJson.asArray match {
          case Some(reads) if reads.isEmpty =>
            // DESNOTE(jbarber, 2026-07-21): Empty `reads: []` is a valid non-DAP envelope;
            // do not treat a missing reads[0].decoded as failure.
            topError
          case Some(reads) =>
            val read = reads.head.hcursor
            val readError = read.get[String]("error").toOption.exists(_.nonEmpty)
            val decoded = read.downField("decoded").focus
            topError || readError || decoded.forall(_.isNull)
          case None =>
            topError || cursor.downField("decoded").focus.forall(_.isNull)
        }
      case None =>
        topError || cursor.downField("decoded").focus.forall(_.isNull)
    }
  }

  def decodeErrorMessage(json: Json): String =
    json.hcursor
      .get[String]("error")
      .toOption
      .filter(_.nonEmpty)
      .orElse(
        json.hcursor.downField("reads").downArray.get[String]("error").toOption.filter(_.nonEmpty)
      )
      .getOrElse("Decode returned null / empty result.")
}
