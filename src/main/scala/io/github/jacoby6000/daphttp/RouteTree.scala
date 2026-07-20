package io.github.jacoby6000.daphttp

import io.circe.Encoder
import io.circe.Json

/** Tree of HTTP data routes for the HTML UI (1:1 with addressable API paths). */
final case class RouteTreeNode(
    path: String,
    kind: String,
    fetchable: Boolean,
    member: Option[String] = None,
    index: Option[Int] = None,
    arrayLength: Option[Int] = None,
    address: Option[Long] = None,
    children: List[RouteTreeNode] = Nil
)

object RouteTreeNode {
  implicit lazy val encoder: Encoder[RouteTreeNode] = Encoder.instance { node =>
    Json.obj(
      "path" -> Json.fromString(node.path),
      "kind" -> Json.fromString(node.kind),
      "fetchable" -> Json.fromBoolean(node.fetchable),
      "member" -> node.member.map(Json.fromString).getOrElse(Json.Null),
      "index" -> node.index.map(Json.fromInt).getOrElse(Json.Null),
      "arrayLength" -> node.arrayLength.map(Json.fromInt).getOrElse(Json.Null),
      "address" -> node.address
        .map(addr => Json.fromString(f"0x$addr%x"))
        .getOrElse(Json.Null),
      "children" -> Json.fromValues(node.children.map(encoder(_)))
    )
  }
}

object RouteTree {
  def fromPlans(routes: Map[String, RoutePlan]): List[RouteTreeNode] =
    routes.toList.sortBy(_._1).map { case (basePath, plan) =>
      val rootAddress = plan.reads.headOption.map(_.address)
      val memberChildren = plan.memberSubRoutes.sortBy(_.memberName).flatMap { sub =>
        val memberAddress = Some(sub.baseAddress + sub.memberOffsetBytes.toLong)
        if (sub.isArray) {
          val length = sub.arrayLength.getOrElse(0)
          val elements =
            if (length > 0)
              (0 until length).map { i =>
                RouteTreeNode(
                  path = s"$basePath/${sub.memberName}/$i",
                  kind = "arrayElement",
                  fetchable = true,
                  member = Some(sub.memberName),
                  index = Some(i),
                  address = memberAddress.map(_ + i.toLong * elementStride(sub))
                )
              }.toList
            else Nil
          List(
            RouteTreeNode(
              path = s"$basePath/${sub.memberName}/{index}",
              kind = "array",
              fetchable = false,
              member = Some(sub.memberName),
              arrayLength = sub.arrayLength,
              address = memberAddress,
              children = elements
            )
          )
        } else {
          List(
            RouteTreeNode(
              path = s"$basePath/${sub.memberName}",
              kind = memberKind(sub),
              fetchable = true,
              member = Some(sub.memberName),
              address = memberAddress
            )
          )
        }
      }
      val chainChildren = plan.pointerChain.toList.flatMap { chain =>
        chain.outerArrayLength match {
          case Some(length) if length > 0 =>
            (0 until length).map { i =>
              RouteTreeNode(
                path = s"$basePath/$i",
                kind = "pointerChainElement",
                fetchable = true,
                index = Some(i),
                address = Some(chain.baseAddress + i.toLong * chainOuterStride(chain))
              )
            }.toList
          case _ =>
            val suffix = (0 until PointerChainResolver.requiredSegmentCount(chain))
              .map(_ => "{index}")
              .mkString("/")
            List(
              RouteTreeNode(
                path = s"$basePath/$suffix",
                kind = "pointerChainTemplate",
                fetchable = false,
                address = Some(chain.baseAddress)
              )
            )
        }
      }
      RouteTreeNode(
        path = basePath,
        kind = if (plan.pointerChain.isDefined) "pointerChainRoot" else "root",
        fetchable = true,
        address = rootAddress.orElse(plan.pointerChain.map(_.baseAddress)),
        children = memberChildren ++ chainChildren
      )
    }

  def flatPaths(routes: Map[String, RoutePlan]): List[String] = {
    val roots = routes.keys.toList
    val extras = routes.toList.flatMap { case (basePath, plan) =>
      val chainRoutes = plan.pointerChain.toList.map { chain =>
        val suffix = (0 until PointerChainResolver.requiredSegmentCount(chain))
          .map(_ => "{index}")
          .mkString("/")
        s"$basePath/$suffix"
      }
      val memberRoutes = plan.memberSubRoutes.flatMap { sub =>
        if (sub.isArray) s"$basePath/${sub.memberName}/{index}" :: Nil
        else s"$basePath/${sub.memberName}" :: Nil
      }
      chainRoutes ++ memberRoutes
    }
    (roots ++ extras).distinct.sorted
  }

  private def memberKind(sub: MemberSubRoute): String =
    sub match {
      case _: MemberSubRoute.PointerSubRoute => "pointer"
      case _: MemberSubRoute.ValueSubRoute   => "value"
    }

  private def elementStride(sub: MemberSubRoute): Long =
    sub match {
      case v: MemberSubRoute.ValueSubRoute =>
        v.elementStrideBytes
          .orElse(v.elementSizeBytes)
          .getOrElse(v.wordSizeBits / 8)
          .toLong
      case p: MemberSubRoute.PointerSubRoute =>
        (p.wordSizeBits / 8).toLong
    }

  private def chainOuterStride(chain: PointerChainPlan): Long =
    chain.outerElementStrideBytes.getOrElse(chain.wordSizeBits / 8).toLong
}
