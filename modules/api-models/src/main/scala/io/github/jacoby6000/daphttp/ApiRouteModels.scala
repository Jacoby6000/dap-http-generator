package io.github.jacoby6000.daphttp

import io.circe.Decoder
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
        .map(addr => Json.fromString(DapAddress.format(addr)))
        .getOrElse(Json.Null),
      "children" -> Json.fromValues(node.children.map(encoder(_)))
    )
  }

  implicit lazy val decoder: Decoder[RouteTreeNode] = Decoder.instance { c =>
    for {
      path <- c.get[String]("path")
      kind <- c.get[String]("kind")
      fetchable <- c.get[Boolean]("fetchable")
      member <- c.get[Option[String]]("member")
      index <- c.get[Option[Int]]("index")
      arrayLength <- c.get[Option[Int]]("arrayLength")
      address <- c.get[Option[String]]("address").map(_.flatMap(DapAddress.parse))
      children <- c.get[List[RouteTreeNode]]("children")
    } yield RouteTreeNode(
      path,
      kind,
      fetchable,
      member,
      index,
      arrayLength,
      address,
      children
    )
  }
}

final case class RoutesResponse(
    routes: List[String],
    tree: List[RouteTreeNode],
    errors: List[String]
)

object RoutesResponse {
  implicit val decoder: Decoder[RoutesResponse] = Decoder.instance { c =>
    for {
      routes <- c.get[List[String]]("routes")
      tree <- c.get[List[RouteTreeNode]]("tree")
      errors <- c.get[List[String]]("errors")
    } yield RoutesResponse(routes, tree, errors)
  }

  implicit val encoder: Encoder[RoutesResponse] = Encoder.instance { r =>
    Json.obj(
      "routes" -> Json.fromValues(r.routes.map(Json.fromString)),
      "tree" -> Json.fromValues(r.tree.map(RouteTreeNode.encoder(_))),
      "errors" -> Json.fromValues(r.errors.map(Json.fromString))
    )
  }
}
