package io.github.jacoby6000.daphttp

import scala.collection.mutable.ListBuffer

/** Maps source struct members to overlay members that share memory (byte-range overlap). */
private[daphttp] object OverlayFieldMapping {

  final case class MemberSpan(name: String, offset: Int, size: Int, irType: IrType)

  def memberSpans(
      struct: IrType.Struct,
      wordSize: Option[Int]
  ): Either[List[String], List[MemberSpan]] = {
    val errors = ListBuffer.empty[String]
    val packed = IrLayout.packMembers(struct.members, wordSize) match {
      case Left(errs) =>
        errors ++= errs
        struct.members
      case Right((members, _)) =>
        members
    }
    val spans = packed.flatMap { member =>
      val offset = member.offsetBytes.getOrElse(0)
      IrLayout.memberSizeBytes(member, wordSize, errors).map { size =>
        MemberSpan(member.name, offset, size, memberReadType(member))
      }
    }
    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(spans)
  }

  def overlappingOverlaySpans(
      sourceStruct: IrType.Struct,
      overlayStruct: IrType.Struct,
      sourceMemberName: String,
      wordSize: Option[Int]
  ): Either[List[String], List[MemberSpan]] =
    for {
      sourceSpans <- memberSpans(sourceStruct, wordSize)
      source <- sourceSpans
        .find(_.name == sourceMemberName)
        .toRight(List(s"Unknown source member '$sourceMemberName' for overlay mapping."))
      overlapping <- overlappingOverlaySpansInRange(
        overlayStruct,
        source.offset,
        source.size,
        wordSize
      )
    } yield overlapping

  def overlappingOverlaySpansInRange(
      overlayStruct: IrType.Struct,
      offset: Int,
      size: Int,
      wordSize: Option[Int]
  ): Either[List[String], List[MemberSpan]] =
    memberSpans(overlayStruct, wordSize).map { overlaySpans =>
      overlaySpans.filter(o => rangesOverlap(offset, size, o.offset, o.size))
    }

  def rangesOverlap(aOffset: Int, aSize: Int, bOffset: Int, bSize: Int): Boolean =
    aSize > 0 && bSize > 0 && aOffset < bOffset + bSize && bOffset < aOffset + aSize

  def unionRange(ranges: List[(Int, Int)]): Option[(Int, Int)] =
    ranges.filter { case (_, size) => size > 0 } match {
      case Nil      => None
      case nonEmpty =>
        val start = nonEmpty.map(_._1).min
        val end = nonEmpty.map { case (off, size) => off + size }.max
        Some((start, end - start))
    }

  private def memberReadType(member: IrMember): IrType =
    member.target match {
      case list: IrType.ListType if member.isArray && !member.isPointer =>
        list
      case list: IrType.ListType if member.isArray && member.isPointer =>
        list.element
      case other if member.isPointer =>
        other
      case other =>
        other
    }
}
