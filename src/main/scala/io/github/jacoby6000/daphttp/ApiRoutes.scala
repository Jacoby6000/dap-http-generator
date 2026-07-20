package io.github.jacoby6000.daphttp

object ApiRoutes {

  /** HTTP path prefix for all generated memory / data routes. */
  val Prefix: String = "/api"

  /** Normalize a logical route path so it is served under [[Prefix]].
    *
    * Idempotent: paths that already start with `/api/` are returned unchanged. Meta endpoints
    * (`/health`, `/routes`, `/resume`) must not be passed here.
    */
  def normalize(path: String): String = {
    val trimmed = path.trim
    if (trimmed.startsWith(s"$Prefix/") || trimmed == Prefix) trimmed
    else if (trimmed.startsWith("/")) s"$Prefix$trimmed"
    else s"$Prefix/$trimmed"
  }

  def isDataPath(path: String): Boolean =
    path == Prefix || path.startsWith(s"$Prefix/")
}
