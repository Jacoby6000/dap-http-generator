package io.github.jacoby6000.daphttp

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.{
  ListShape,
  MemberShape,
  ServiceShape,
  Shape,
  ShapeId,
  ShapeType,
  StructureShape
}

object DapStructManifestGenerator {
  final case class Diagnostic(level: String, shapeId: String, message: String)
  final case class ManifestResult(json: String, diagnostics: List[Diagnostic]) {
    def errors: List[Diagnostic] = diagnostics.filter(_.level == "error")
    def warnings: List[Diagnostic] = diagnostics.filter(_.level == "warning")
  }

  private val DapStructTrait = ShapeId.from("com.jacoby6000.daphttp#dapStruct")
  private val BitmaskTrait = ShapeId.from("com.jacoby6000.daphttp#bitmask")
  private val SizeTrait = ShapeId.from("com.jacoby6000.daphttp#size")
  private val AlignmentTrait = ShapeId.from("com.jacoby6000.daphttp#alignment")
  private val PaddingTrait = ShapeId.from("com.jacoby6000.daphttp#padding")
  private val CStringTrait = ShapeId.from("com.jacoby6000.daphttp#cString")
  private val EndianTrait = ShapeId.from("com.jacoby6000.daphttp#endian")
  private val BytesShape = ShapeId.from("com.jacoby6000.daphttp#Bytes")
  private val BitsShape = ShapeId.from("com.jacoby6000.daphttp#Bits")

  private val CTypeTraits: List[(ShapeId, String)] = List(
    ShapeId.from("com.jacoby6000.daphttp#u8") -> "u8",
    ShapeId.from("com.jacoby6000.daphttp#s8") -> "s8",
    ShapeId.from("com.jacoby6000.daphttp#u16") -> "u16",
    ShapeId.from("com.jacoby6000.daphttp#s16") -> "s16",
    ShapeId.from("com.jacoby6000.daphttp#u32") -> "u32",
    ShapeId.from("com.jacoby6000.daphttp#s32") -> "s32"
  )

  def generate(model: Model): String = generateWithDiagnostics(model).json

  def generateWithDiagnostics(model: Model): ManifestResult = {
    val diagnostics = ListBuffer.empty[Diagnostic]
    val defaultEndian = model
      .shapes(classOf[ServiceShape])
      .iterator()
      .asScala
      .toList
      .sortBy(_.getId.toString)
      .flatMap(stringTraitValue(_, EndianTrait))
      .headOption

    val structs = model
      .shapes(classOf[StructureShape])
      .iterator()
      .asScala
      .filter(s => s.hasTrait(DapStructTrait) || s.hasTrait(BitmaskTrait) || s.hasTrait(SizeTrait))
      .toList
      .sortBy(_.getId.toString)

    structs.foreach(validateStructure(model, _, diagnostics))

    val structJson = structs.map(structToJson(model, _, defaultEndian, diagnostics)).mkString(",")
    val diagnosticsJson = diagnostics.toList.map(diagnosticToJson).mkString(",")
    ManifestResult(s"{\"structs\":[$structJson],\"diagnostics\":[$diagnosticsJson]}", diagnostics.toList)
  }

  private def validateStructure(model: Model, structure: StructureShape, diagnostics: ListBuffer[Diagnostic]): Unit = {
    val members = structure.members().asScala.toList
    val isBitmask = structure.hasTrait(BitmaskTrait)
    val sizeOpt = intTraitValue(structure, SizeTrait)

    if (isBitmask && sizeOpt.isEmpty) {
      diagnostics += Diagnostic("error", structure.getId.toString, "Bitmask structures must declare @size.")
    }

    if (isBitmask) {
      members.foreach { member =>
        val target = model.expectShape(member.getTarget)
        if (target.getType != ShapeType.BOOLEAN) {
          diagnostics += Diagnostic(
            "error",
            member.getId.toString,
            "Bitmask members must target boolean shapes."
          )
        }
      }

      sizeOpt.foreach { bitWidth =>
        val usedBits = members.length
        if (usedBits < bitWidth) {
          diagnostics += Diagnostic(
            "warning",
            structure.getId.toString,
            s"Bitmask defines $bitWidth bits but only $usedBits members are present."
          )
        } else if (usedBits > bitWidth) {
          diagnostics += Diagnostic(
            "error",
            structure.getId.toString,
            s"Bitmask defines $bitWidth bits but $usedBits members are present."
          )
        }
      }
    } else {
      sizeOpt.foreach { expectedBytes =>
        val expectedBits = expectedBytes * 8
        val knownBits = members.flatMap(memberBitWidth(model, _, diagnostics)).sum
        if (knownBits < expectedBits) {
          diagnostics += Diagnostic(
            "warning",
            structure.getId.toString,
            s"Structure declares @size($expectedBytes) but only $knownBits known bits are defined."
          )
        } else if (knownBits > expectedBits) {
          diagnostics += Diagnostic(
            "error",
            structure.getId.toString,
            s"Structure declares @size($expectedBytes) but requires at least $knownBits bits."
          )
        }
      }
    }
  }

  private def structToJson(
    model: Model,
    structure: StructureShape,
    defaultEndian: Option[String],
    diagnostics: ListBuffer[Diagnostic]
  ): String = {
    val alignmentPart = intTraitValue(structure, AlignmentTrait)
      .map(v => s",\"alignment\":$v")
      .getOrElse("")

    val sizePart = intTraitValue(structure, SizeTrait)
      .map(v => s",\"size\":$v")
      .getOrElse("")

    val kindPart = if (structure.hasTrait(BitmaskTrait)) ",\"kind\":\"bitmask\"" else ",\"kind\":\"struct\""

    val membersJson = structure.members().asScala.toList.sortBy(_.getMemberName).map { member =>
      memberToJson(model, member, defaultEndian, diagnostics)
    }.mkString(",")

    s"{\"shapeId\":\"${escape(structure.getId.toString)}\"$kindPart$alignmentPart$sizePart,\"members\":[$membersJson]}"
  }

  private def memberToJson(
    model: Model,
    member: MemberShape,
    defaultEndian: Option[String],
    diagnostics: ListBuffer[Diagnostic]
  ): String = {
    val cType = cTypeName(model, member)
    val cStringPart = cStringConfig(member)
      .map { cfg =>
        s",\"cStringBytes\":${cfg.bytes},\"cStringEncoding\":\"${escape(cfg.encoding)}\""
      }
      .getOrElse("")

    val alignmentPart = intTraitValue(member, AlignmentTrait)
      .map(v => s",\"alignment\":$v")
      .getOrElse("")

    val paddingPart = intTraitValue(member, PaddingTrait)
      .map(v => s",\"paddingRepeats\":$v")
      .getOrElse("")

    val endianPart = stringTraitValue(member, EndianTrait)
      .orElse(defaultEndian)
      .map(v => s",\"endian\":\"${escape(v)}\"")
      .getOrElse("")

    val widthPart = memberBitWidth(model, member, diagnostics)
      .map(v => s",\"bitWidth\":$v")
      .getOrElse("")

    s"{\"name\":\"${escape(member.getMemberName)}\",\"type\":\"${escape(cType)}\"$cStringPart$alignmentPart$paddingPart$endianPart$widthPart}"
  }

  private def memberBitWidth(model: Model, member: MemberShape, diagnostics: ListBuffer[Diagnostic]): Option[Int] = {
    val target = model.expectShape(member.getTarget)

    cStringConfig(member).map(_.bytes * 8).orElse {
      if (member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#u8")) || member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#s8"))) {
        Some(8)
      } else if (member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#u16")) || member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#s16"))) {
        Some(16)
      } else if (member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#u32")) || member.hasTrait(ShapeId.from("com.jacoby6000.daphttp#s32"))) {
        Some(32)
      } else {
        target.getType match {
          case ShapeType.BOOLEAN => Some(1)
          case ShapeType.BYTE    => Some(8)
          case ShapeType.SHORT   => Some(16)
          case ShapeType.INTEGER => Some(32)
          case ShapeType.LONG    => Some(64)
          case ShapeType.LIST =>
            val paddingOpt = intTraitValue(member, PaddingTrait)
            paddingOpt.flatMap { repeats =>
              target match {
                case listShape: ListShape if listShape.getId == BytesShape => Some(repeats * 8)
                case listShape: ListShape if listShape.getId == BitsShape  => Some(repeats)
                case _: ListShape =>
                  diagnostics += Diagnostic(
                    "error",
                    member.getId.toString,
                    "@padding can only be used with com.jacoby6000.daphttp#Bytes or #Bits list shapes."
                  )
                  None
                case _ => None
              }
            }
          case _ =>
            if (member.hasTrait(PaddingTrait)) {
              diagnostics += Diagnostic("error", member.getId.toString, "@padding can only be applied to list members.")
            }
            None
        }
      }
    }
  }

  private def cTypeName(model: Model, member: MemberShape): String = {
    CTypeTraits.collectFirst {
      case (traitId, cType) if member.hasTrait(traitId) => cType
    }.getOrElse {
      val target = model.expectShape(member.getTarget)
      if (target.getId == BytesShape) {
        "Bytes"
      } else if (target.getId == BitsShape) {
        "Bits"
      } else {
        target.getType.name().toLowerCase
      }
    }
  }

  private final case class CStringConfig(bytes: Int, encoding: String)

  private def cStringConfig(member: MemberShape): Option[CStringConfig] = {
    member.findTrait(CStringTrait).toScala.map { cStringTrait =>
      val node = cStringTrait.toNode
      if (node.isNumberNode) {
        CStringConfig(node.expectNumberNode.getValue.intValue(), "ASCII")
      } else {
        val objectNode = node.expectObjectNode
        val bytes = objectNode.expectNumberMember("bytes").getValue.intValue()
        val encoding = objectNode.getStringMember("encoding").toScala.map(_.getValue).getOrElse("ASCII")
        CStringConfig(bytes, encoding)
      }
    }
  }

  private def intTraitValue(shape: Shape, traitId: ShapeId): Option[Int] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) Some(node.expectNumberNode.getValue.intValue()) else None
    }
  }

  private def stringTraitValue(shape: Shape, traitId: ShapeId): Option[String] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isStringNode) Some(node.expectStringNode.getValue) else None
    }
  }

  private def diagnosticToJson(diagnostic: Diagnostic): String = {
    s"{\"level\":\"${escape(diagnostic.level)}\",\"shapeId\":\"${escape(diagnostic.shapeId)}\",\"message\":\"${escape(diagnostic.message)}\"}"
  }

  private def escape(value: String): String = {
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
  }
}
