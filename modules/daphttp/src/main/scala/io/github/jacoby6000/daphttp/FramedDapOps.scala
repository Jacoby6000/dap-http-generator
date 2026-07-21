package io.github.jacoby6000.daphttp

import io.circe.Json

/** Shared DAP request/response helpers for an already-open [[DapFramedSession]].
  *
  * DESNOTE(jbarber, 2026-07-21): TCP and pipe clients keep distinct connect/reconnect and
  * serialization policies (JVM monitor vs cats-effect Mutex). Only the framed command payloads and
  * response parsing are shared here.
  */
private[daphttp] object FramedDapOps {
  def readMemory(
      session: DapFramedSession,
      address: Long,
      sizeBytes: Int,
      timeoutMs: Int
  ): Either[String, String] =
    session
      .sendRequest(
        command = "readMemory",
        arguments = Some(
          Json.obj(
            "memoryReference" -> Json.fromString(f"0x$address%x"),
            "count" -> Json.fromInt(sizeBytes)
          )
        ),
        timeoutMs = timeoutMs
      )
      .flatMap { body =>
        body.hcursor
          .downField("data")
          .as[String]
          .toOption
          .toRight("DAP readMemory response did not include body.data.")
      } match {
      case Right(value) =>
        DapHttpLoggers.dap.debug(
          "readMemory address=0x{} succeeded bytes={}",
          java.lang.Long.toHexString(address),
          Integer.valueOf(sizeBytes)
        )
        Right(value)
      case Left(error) =>
        DapHttpLoggers.dap.warn(
          "readMemory address=0x{} failed: {}",
          java.lang.Long.toHexString(address),
          error
        )
        Left(error)
    }

  def writeMemory(
      session: DapFramedSession,
      address: Long,
      dataBase64: String,
      timeoutMs: Int
  ): Either[String, Int] =
    session
      .sendRequest(
        command = "writeMemory",
        arguments = Some(
          Json.obj(
            "memoryReference" -> Json.fromString(f"0x$address%x"),
            "data" -> Json.fromString(dataBase64)
          )
        ),
        timeoutMs = timeoutMs
      )
      .flatMap { body =>
        body.hcursor
          .get[Int]("bytesWritten")
          .toOption
          .toRight("DAP writeMemory response did not include body.bytesWritten.")
      } match {
      case Right(written) =>
        DapHttpLoggers.dap.debug(
          "writeMemory address=0x{} succeeded bytesWritten={}",
          java.lang.Long.toHexString(address),
          Integer.valueOf(written)
        )
        Right(written)
      case Left(error) =>
        DapHttpLoggers.dap.warn(
          "writeMemory address=0x{} failed: {}",
          java.lang.Long.toHexString(address),
          error
        )
        Left(error)
    }

  def continueExecution(
      session: DapFramedSession,
      threadId: Option[Int],
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int
  ): Either[String, Json] = {
    val resolvedThreadId = threadId.getOrElse(resolveThreadId(session, dapTimeoutMs))
    session
      .sendRequest(
        command = "continue",
        arguments = Some(Json.obj("threadId" -> Json.fromInt(resolvedThreadId))),
        timeoutMs = dapContinueTimeoutMs
      )
      .map { response =>
        DapHttpLoggers.dap.info(
          "continue threadId={} succeeded",
          Integer.valueOf(resolvedThreadId)
        )
        response
      }
      .left
      .map { error =>
        DapHttpLoggers.dap.warn(
          "continue threadId={} failed: {}",
          Integer.valueOf(resolvedThreadId),
          error
        )
        error
      }
  }

  def resolveThreadId(session: DapFramedSession, dapTimeoutMs: Int): Int =
    session
      .sendRequest(
        command = "threads",
        arguments = None,
        timeoutMs = math.min(dapTimeoutMs, 2000)
      )
      .toOption
      .flatMap(json => parseThreadIds(json).headOption)
      .getOrElse {
        DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
        1
      }

  def parseThreadIds(responseBody: Json): List[Int] =
    responseBody.hcursor
      .downField("threads")
      .values
      .getOrElse(Vector.empty)
      .flatMap(_.hcursor.downField("id").as[Int].toOption)
      .toList
}
