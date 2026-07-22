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

  def extractOverlayDecoded(json: Json): Option[Json] =
    json.hcursor
      .downField("reads")
      .downArray
      .downField("overlayDecoded")
      .focus
      .orElse(json.hcursor.downField("overlayDecoded").focus)

  def writeDecodedFields(
      payload: Json,
      decoded: Json,
      overlay: Option[Json]
  ): Json =
    payload.hcursor.downField("reads").as[Vector[Json]] match {
      case Right(reads) if reads.nonEmpty =>
        val head = reads.head
        val withDecoded = head.mapObject(_.add("decoded", decoded))
        val updatedHead = overlay match {
          case Some(od) =>
            withDecoded.mapObject(_.add("overlayDecoded", od))
          case None =>
            if (head.hcursor.downField("overlayDecoded").succeeded)
              withDecoded.mapObject(_.remove("overlayDecoded"))
            else withDecoded
        }
        payload.mapObject(_.add("reads", Json.fromValues(updatedHead +: reads.tail)))
      case _ =>
        val withDecoded = payload.mapObject(_.add("decoded", decoded))
        overlay match {
          case Some(od) =>
            withDecoded.mapObject(_.add("overlayDecoded", od))
          case None =>
            if (payload.hcursor.downField("overlayDecoded").succeeded)
              withDecoded.mapObject(_.remove("overlayDecoded"))
            else withDecoded
        }
    }

  def decodeFailed(json: Json): Boolean = {
    val cursor = json.hcursor
    val topError = cursor.get[String]("error").toOption.exists(_.nonEmpty)
    val reads = cursor.downField("reads")
    if (reads.succeeded) {
      val read = reads.downArray
      val readError = read.get[String]("error").toOption.exists(_.nonEmpty)
      val decoded = read.downField("decoded").focus
      topError || readError || decoded.forall(_.isNull)
    } else {
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
