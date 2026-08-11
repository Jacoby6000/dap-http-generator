package io.github.jacoby6000.daphttp

/** Unified resolution of `/api/...` paths to a read target.
  *
  * HTTP GET and realtime watches share this matcher. They differ only in how they interpret
  * [[ResolvedDataPath.MemberSub]] for pointer members: GET follows the pointee via
  * [[DapHttpServerMain.servePointerSubRoute]]; watches resolve the pointer slot through
  * [[MemberPathResolver]] ([[ResolvedDataPath.NestedMember]] with `isPointerSlot = true`).
  */
sealed trait ResolvedDataPath

object ResolvedDataPath {

  /** Exact route plan (root read). */
  final case class Root(plan: RoutePlan) extends ResolvedDataPath

  /** Pointer-chain route with concrete numeric index segments. */
  final case class PointerChain(plan: RoutePlan, segments: List[Int]) extends ResolvedDataPath

  /** Shallow `$base/member` or `$base/member/{i}` / `$base/{i}` matching a prebuilt subroute.
    *
    * Used by HTTP GET so [[MemberSubRoute.PointerSubRoute]] can follow the pointee.
    */
  final case class MemberSub(
      plan: RoutePlan,
      sub: MemberSubRoute,
      index: Option[Int]
  ) extends ResolvedDataPath

  /** Deep / nested member path resolved via [[MemberPathResolver]]. */
  final case class NestedMember(resolved: ResolvedMemberRead) extends ResolvedDataPath
}

private[daphttp] object RoutePathResolver {

  /** HTTP GET order: exact/chain → shallow member subroute → deep nested resolver. */
  def resolveForHttp(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[ResolvedDataPath] =
    matchRoute(path, routes) match {
      case Some((plan, Nil)) =>
        Some(ResolvedDataPath.Root(plan))
      case Some((plan, segments)) =>
        Some(ResolvedDataPath.PointerChain(plan, segments))
      case None =>
        matchMemberSubRoute(path, routes)
          .map { case (plan, sub, index) =>
            ResolvedDataPath.MemberSub(plan, sub, index)
          }
          .orElse(
            MemberPathResolver.resolve(path, routes).map(ResolvedDataPath.NestedMember(_))
          )
    }

  /** Watch order: exact root only; nested via [[MemberPathResolver]] (no shallow pointer follow).
    *
    * Pointer-chain paths with segments are rejected — callers must open a concrete index first.
    */
  def resolveForWatch(
      path: String,
      routes: Map[String, RoutePlan]
  ): Either[String, ResolvedDataPath] =
    matchRoute(path, routes) match {
      case Some((plan, Nil)) =>
        Right(ResolvedDataPath.Root(plan))
      case Some((_, _ :: _)) =>
        Left(
          s"Pointer-chain path $path cannot be watched directly; open a concrete index first."
        )
      case None =>
        MemberPathResolver.resolve(path, routes) match {
          case None =>
            Left(s"No route generated for $path")
          case Some(resolved) =>
            Right(ResolvedDataPath.NestedMember(resolved))
        }
    }

  def matchRoute(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, List[Int])] =
    routes.get(path).map(_ -> Nil).orElse {
      routes.collectFirst {
        case (basePath, plan)
            if plan.pointerChain.isDefined && path.startsWith(
              s"$basePath/"
            ) && path.length > basePath.length =>
          val suffix = path.stripPrefix(s"$basePath/")
          val segments = suffix.split("/").toList
          if (segments.nonEmpty && segments.forall(seg => seg.nonEmpty && seg.forall(_.isDigit))) {
            val indices = segments.flatMap(_.toIntOption)
            if (indices.length == segments.length) Some(plan -> indices)
            else None
          } else {
            None
          }
      }.flatten
    }

  def matchMemberSubRoute(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    routes.collectFirst(Function.unlift { case (basePath, plan: RoutePlan) =>
      if (!path.startsWith(s"$basePath/") || path.length <= basePath.length + 1) None
      else {
        val suffix = path.stripPrefix(s"$basePath/")
        val parts = suffix.split("/").toList
        matchRootArrayElement(plan, parts).orElse {
          parts.headOption.flatMap { memberName =>
            plan.memberSubRoutes
              .find(s => s.memberName == memberName && s.memberName.nonEmpty)
              .flatMap { sub =>
                parts.drop(1) match {
                  case Nil if !sub.isArray =>
                    Some((plan, sub, None))
                  case Nil if sub.isArray =>
                    None
                  case indexStr :: Nil if sub.isArray && indexStr.forall(_.isDigit) =>
                    indexStr.toIntOption.map(i => (plan, sub, Some(i)))
                  case _ =>
                    None
                }
              }
          }
        }
      }
    })

  private def matchRootArrayElement(
      plan: RoutePlan,
      parts: List[String]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    plan.memberSubRoutes
      .find(s => s.memberName == MemberSubRoute.RootArrayMemberName && s.isArray)
      .flatMap { sub =>
        parts match {
          case indexStr :: Nil if indexStr.forall(_.isDigit) =>
            indexStr.toIntOption.map(i => (plan, sub, Some(i)))
          case _ =>
            None
        }
      }
}
