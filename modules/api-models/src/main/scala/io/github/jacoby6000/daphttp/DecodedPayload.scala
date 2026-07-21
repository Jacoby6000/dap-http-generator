package io.github.jacoby6000.daphttp

import io.circe.Json

/** Read/write `decoded` / `overlayDecoded` on DAP HTTP response envelopes. */
object DecodedPayload {
  def extractDecoded(json: Json): Json =
    json.hcursor
      .downField("reads")
      .downArray
      .downField("decoded")
      .focus
      .orElse(json.hcursor.downField("decoded").focus)
      .getOrElse(json)

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
    val hasError = cursor.get[String]("error").toOption.exists(_.nonEmpty)
    val decodedFocus = cursor
      .downField("decoded")
      .focus
      .orElse(cursor.downField("reads").downArray.downField("decoded").focus)
    hasError || decodedFocus.exists(_.isNull)
  }

  def decodeErrorMessage(json: Json): String =
    json.hcursor
      .get[String]("error")
      .toOption
      .filter(_.nonEmpty)
      .getOrElse("Decode returned null / empty result.")
}
