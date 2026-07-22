package io.github.jacoby6000.daphttp

import io.circe.Json

/** Navigate and update Circe JSON by field/index segment lists. */
object JsonPath {
  def get(json: Json, segments: List[String]): Option[Json] =
    segments.foldLeft(Option(json)) { (acc, seg) =>
      acc.flatMap { j =>
        j.asObject
          .flatMap(_.apply(seg))
          .orElse(
            j.asArray.flatMap { arr =>
              seg.toIntOption.flatMap(i => arr.lift(i))
            }
          )
      }
    }

  def replace(json: Json, segments: List[String], value: Json): Json =
    segments match {
      case Nil          => value
      case head :: tail =>
        json.asObject match {
          case Some(obj) =>
            val child = obj(head).getOrElse(Json.Null)
            Json.fromJsonObject(obj.add(head, replace(child, tail, value)))
          case None =>
            json.asArray match {
              case Some(arr) =>
                head.toIntOption match {
                  case Some(index) if index >= 0 && index < arr.size =>
                    Json.fromValues(arr.updated(index, replace(arr(index), tail, value)))
                  case _ => json
                }
              case None => json
            }
        }
    }
}
