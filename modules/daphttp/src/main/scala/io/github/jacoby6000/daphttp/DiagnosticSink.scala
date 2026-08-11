package io.github.jacoby6000.daphttp

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

/** Categories for C→IR / Smithy diagnostics shared by console, `/routes`, and Markdown reports. */
sealed trait DiagnosticCategory
object DiagnosticCategory {
  case object Section extends DiagnosticCategory
  case object Conflict extends DiagnosticCategory
  case object Symbol extends DiagnosticCategory
  case object Layout extends DiagnosticCategory
  case object ArrayBound extends DiagnosticCategory
  case object IncludeHint extends DiagnosticCategory
  case object Other extends DiagnosticCategory
}

final case class Diagnostic(category: DiagnosticCategory, message: String)

/** Collects warnings once and fans them out to the logger plus report/`/routes` buffers.
  *
  * DESNOTE(jbarber, 2026-07-21): Layout and array-bound notes used to log only and never reached
  * `IrDiagnostics` / `--report`. Route every non-fatal warning through this sink so console,
  * summary lists, and Markdown stay aligned.
  */
final class DiagnosticSink(logger: Logger, logToLogger: Boolean = true) {
  private val buffer = ListBuffer.empty[Diagnostic]

  def warn(category: DiagnosticCategory, message: String): Unit = {
    buffer += Diagnostic(category, message)
    if (logToLogger) {
      logger.warn("{}", message)
    }
  }

  def diagnostics: List[Diagnostic] = buffer.toList

  def messages: List[String] = buffer.map(_.message).toList

  def messages(category: DiagnosticCategory): List[String] =
    buffer.collect { case Diagnostic(`category`, message) => message }.toList

  /** Warnings that are not already mirrored into a dedicated `IrDiagnostics` field. */
  def reportOtherWarnings: List[String] =
    buffer.collect {
      case Diagnostic(DiagnosticCategory.Layout, message)     => message
      case Diagnostic(DiagnosticCategory.ArrayBound, message) => message
      case Diagnostic(DiagnosticCategory.Other, message)      => message
    }.toList
}

object DiagnosticSink {
  def forDoldecomp: DiagnosticSink =
    new DiagnosticSink(DapHttpLoggers.irSourceDoldecomp)

  /** Collect without logging — useful in unit tests that assert message text only. */
  def silent: DiagnosticSink =
    new DiagnosticSink(LoggerFactory.getLogger("daphttp.diagnostic.silent"), logToLogger = false)
}
