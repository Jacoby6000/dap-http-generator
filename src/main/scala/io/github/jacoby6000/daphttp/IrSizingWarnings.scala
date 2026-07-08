package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

object IrSizingWarnings {
  def collect(services: List[IrService]): List[String] =
    services
      .flatMap(_.operations.map(_.output))
      .foldLeft((Set.empty[ShapeId], List.empty[String])) { case ((visited, warnings), struct) =>
        walkStruct(struct, visited, warnings)
      }
      ._2
      .distinct
      .sorted

  def writeToStderr(services: List[IrService]): Unit =
    collect(services).foreach(message => Console.err.println(s"warning: $message"))

  private def walkStruct(
      struct: IrType.Struct,
      visited: Set[ShapeId],
      warnings: List[String]
  ): (Set[ShapeId], List[String]) =
    if (visited.contains(struct.id)) {
      (visited, warnings)
    } else {
      struct.members.foldLeft((visited + struct.id, warnings)) { case ((seen, found), member) =>
        val withMember = memberWarning(member).fold(found)(found :+ _)
        walkType(member.target, seen, withMember)
      }
    }

  private def walkType(
      irType: IrType,
      visited: Set[ShapeId],
      warnings: List[String]
  ): (Set[ShapeId], List[String]) =
    irType match {
      case struct: IrType.Struct =>
        walkStruct(struct, visited, warnings)
      case union: IrType.Union =>
        union.members.foldLeft((visited, warnings)) { case ((seen, found), member) =>
          val withMember = memberWarning(member).fold(found)(found :+ _)
          walkType(member.target, seen, withMember)
        }
      case listType: IrType.ListType =>
        walkType(listType.element, visited, warnings)
      case mapType: IrType.MapType =>
        val (afterKey, keyWarnings) = walkType(mapType.key, visited, warnings)
        walkType(mapType.value, afterKey, keyWarnings)
      case IrType.Ref(_) | IrType.Primitive(_) =>
        (visited, warnings)
    }

  private def memberWarning(member: IrMember): Option[String] =
    if (member.isPointer) {
      None
    } else {
      effectivePrimitive(member).flatMap {
        case IrPrimitive.S32 if member.primitiveOverride.isEmpty =>
          Some(
            s"${member.id}: Integer member lacks an explicit width trait (for example @u32 or @s32)."
          )
        case IrPrimitive.LongWord if member.primitiveOverride.isEmpty =>
          Some(
            s"${member.id}: Long member lacks an explicit width trait (for example @u64 or @u128); width follows service @wordSize."
          )
        case _ =>
          None
      }
    }

  private def effectivePrimitive(member: IrMember): Option[IrPrimitive] =
    member.primitiveOverride.orElse {
      member.target match {
        case IrType.Primitive(kind) => Some(kind)
        case _                      => None
      }
    }
}
