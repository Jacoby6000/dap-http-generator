package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._

/** Path helpers and HTTP handlers for generated IR data routes under `/api/...`. */
object ApiRoutes {

  /** HTTP path prefix for all generated memory / data routes. */
  val Prefix: String = "/api"

  /** Normalize a logical route path so it is served under [[Prefix]].
    *
    * Idempotent: paths that already start with `/api/` are returned unchanged. Meta endpoints
    * (`/health`, `/routes`, `/dap-proxy/...`) must not be passed here.
    */
  def normalize(path: String): String = {
    val trimmed = path.trim
    if (trimmed.startsWith(s"$Prefix/") || trimmed == Prefix) trimmed
    else if (trimmed.startsWith("/")) s"$Prefix$trimmed"
    else s"$Prefix/$trimmed"
  }

  def isDataPath(path: String): Boolean =
    path == Prefix || path.startsWith(s"$Prefix/")

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): HttpRoutes[IO] =
    HttpRoutes.of[IO] { case request @ GET -> _ =>
      val routePath = request.uri.path.renderString
      if (!isDataPath(routePath)) {
        NotFound(Json.obj("error" -> Json.fromString(s"No route generated for $routePath")))
      } else {
        for {
          result <- plansRef.get
          response <- RoutePathResolver.resolveForHttp(routePath, result.routes) match {
            case Some(ResolvedDataPath.PointerChain(routePlan, chainSegments)) =>
              DapHttpServerMain.servePointerChainRoute(
                routePlan,
                chainSegments,
                dapClient,
                overlaysRef
              )
            case Some(ResolvedDataPath.Root(routePlan)) =>
              DapHttpServerMain.serveRoutePlan(routePlan, dapClient, overlaysRef)
            case Some(ResolvedDataPath.MemberSub(_, subRoute, index)) =>
              DapHttpServerMain.serveMemberSubRoute(
                routePath,
                subRoute,
                index,
                dapClient,
                overlaysRef
              )
            case Some(ResolvedDataPath.NestedMember(resolved)) =>
              DapHttpServerMain.serveResolvedMember(
                routePath,
                resolved,
                dapClient,
                overlaysRef
              )
            case None =>
              NotFound(
                Json.obj(
                  "error" -> Json.fromString(s"No route generated for $routePath")
                )
              )
          }
        } yield response
      }
    }
}
