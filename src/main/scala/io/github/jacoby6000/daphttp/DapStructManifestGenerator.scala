package io.github.jacoby6000.daphttp

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.{MemberShape, Shape, ShapeId, StructureShape}

object DapStructManifestGenerator {
  private val DapStructTrait = ShapeId.from("com.jacoby6000.daphttp#dapStruct")
  private val AlignmentTrait = ShapeId.from("com.jacoby6000.daphttp#alignment")
  private val CStringTrait = ShapeId.from("com.jacoby6000.daphttp#cString")

  private val CTypeTraits: List[(ShapeId, String)] = List(
    ShapeId.from("com.jacoby6000.daphttp#u8") -> "u8",
    ShapeId.from("com.jacoby6000.daphttp#s8") -> "s8",
    ShapeId.from("com.jacoby6000.daphttp#u16") -> "u16",
    ShapeId.from("com.jacoby6000.daphttp#s16") -> "s16",
    ShapeId.from("com.jacoby6000.daphttp#u32") -> "u32",
    ShapeId.from("com.jacoby6000.daphttp#s32") -> "s32"
  )

  def generate(model: Model): String = {
    val structs = model
      .shapes(classOf[StructureShape])
      .iterator()
      .asScala
      .filter(_.hasTrait(DapStructTrait))
      .toList
      .sortBy(_.getId.toString)

    val structJson = structs.map(structToJson(model, _)).mkString(",")
    s"{\"structs\":[$structJson]}"
  }

  private def structToJson(model: Model, structure: StructureShape): String = {
    val alignmentPart = intTraitValue(structure, AlignmentTrait)
      .map(v => s",\"alignment\":$v")
      .getOrElse("")

    val membersJson = structure.members().asScala.toList.sortBy(_.getMemberName).map { member =>
      memberToJson(model, member)
    }.mkString(",")

    s"{\"shapeId\":\"${structure.getId}\"$alignmentPart,\"members\":[$membersJson]}"
  }

  private def memberToJson(model: Model, member: MemberShape): String = {
    val cType = cTypeName(model, member)
    val cStringPart = intTraitValue(member, CStringTrait)
      .map(v => s",\"cStringBytes\":$v")
      .getOrElse("")
    val alignmentPart = intTraitValue(member, AlignmentTrait)
      .map(v => s",\"alignment\":$v")
      .getOrElse("")

    s"{\"name\":\"${member.getMemberName}\",\"type\":\"$cType\"$cStringPart$alignmentPart}"
  }

  private def cTypeName(model: Model, member: MemberShape): String = {
    CTypeTraits.collectFirst {
      case (traitId, cTypeName) if member.hasTrait(traitId) => cTypeName
    }.getOrElse {
      val target = model.expectShape(member.getTarget)
      target.getType.name().toLowerCase
    }
  }

  private def intTraitValue(shape: Shape, traitId: ShapeId): Option[Int] = {
    shape
      .findTrait(traitId)
      .toScala
      .map(_.toNode.expectNumberNode.getValue.intValue())
  }
}
