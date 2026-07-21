package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Codec

/** Absolute memory read resolved from a member / nested-field HTTP path. */
final case class ResolvedMemberRead(
    address: Long,
    sizeBytes: Int,
    endian: IrEndian,
    wordSizeBits: Int,
    valueType: Option[IrType],
    decodeCodec: Option[Codec[Json]],
    /** Byte offset from the parent route's primary read address (overlay expansion). */
    sourceOffsetInParent: Int,
    parentPath: String,
    isPointerSlot: Boolean = false
)

/** Resolves `$base/member`, `$base/arr/{i}`, `$base/{i}/field`, and deeper nested paths. */
private[daphttp] object MemberPathResolver {

  def resolve(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[ResolvedMemberRead] =
    routes.collectFirst(Function.unlift { case (basePath, plan) =>
      if (!path.startsWith(s"$basePath/") || path.length <= basePath.length + 1) None
      else {
        val parts = path.stripPrefix(s"$basePath/").split("/").toList.filter(_.nonEmpty)
        resolveAgainstPlan(basePath, plan, parts).toOption
      }
    })

  def resolveAgainstPlan(
      basePath: String,
      plan: RoutePlan,
      parts: List[String]
  ): Either[String, ResolvedMemberRead] = {
    if (parts.isEmpty) Left("Empty member path")
    else {
      val parentAddr = plan.reads.headOption.map(_.address).getOrElse(0L)
      if (parts.head.forall(_.isDigit))
        resolveRootArray(basePath, plan, parts, parentAddr)
      else
        resolveNamedMember(basePath, plan, parts, parentAddr)
    }
  }

  private def resolveRootArray(
      basePath: String,
      plan: RoutePlan,
      parts: List[String],
      parentAddr: Long
  ): Either[String, ResolvedMemberRead] =
    plan.memberSubRoutes
      .collectFirst {
        case v: MemberSubRoute.ValueSubRoute
            if v.memberName == MemberSubRoute.RootArrayMemberName && v.isArray =>
          v
      }
      .toRight("No root array subroute")
      .flatMap { v =>
        parts match {
          case indexStr :: rest if indexStr.forall(_.isDigit) =>
            val index = indexStr.toInt
            val stride = v.elementStrideBytes.getOrElse(v.elementSizeBytes.getOrElse(0))
            val elemAddr = v.baseAddress + index.toLong * stride.toLong
            if (rest.isEmpty)
              atSubRoute(basePath, parentAddr, v, elemAddr)
            else
              v.valueType
                .toRight(s"Root array element has no value type")
                .flatMap(t =>
                  descend(basePath, parentAddr, t, elemAddr, rest, v.endian, v.wordSizeBits)
                )
          case _ =>
            Left("Root array path requires a numeric index")
        }
      }

  private def resolveNamedMember(
      basePath: String,
      plan: RoutePlan,
      parts: List[String],
      parentAddr: Long
  ): Either[String, ResolvedMemberRead] = {
    val memberName = parts.head
    plan.memberSubRoutes.find(s => s.memberName == memberName && s.memberName.nonEmpty) match {
      case None =>
        Left(s"Unknown member '$memberName'")
      case Some(v: MemberSubRoute.ValueSubRoute) if v.isArray =>
        parts.drop(1) match {
          case indexStr :: rest if indexStr.forall(_.isDigit) =>
            val index = indexStr.toInt
            val stride = v.elementStrideBytes.getOrElse(v.elementSizeBytes.getOrElse(0))
            val elemAddr =
              v.baseAddress + v.memberOffsetBytes.toLong + index.toLong * stride.toLong
            if (rest.isEmpty)
              atSubRoute(basePath, parentAddr, v, elemAddr)
            else
              v.valueType
                .toRight(s"Array member '${v.memberName}' has no element type")
                .flatMap(t =>
                  descend(basePath, parentAddr, t, elemAddr, rest, v.endian, v.wordSizeBits)
                )
          case _ =>
            Left(s"Array member '${v.memberName}' requires a numeric index")
        }
      case Some(v: MemberSubRoute.ValueSubRoute) =>
        val addr = v.baseAddress + v.memberOffsetBytes.toLong
        val rest = parts.drop(1)
        if (rest.isEmpty)
          atSubRoute(basePath, parentAddr, v, addr)
        else
          v.valueType
            .toRight(s"Member '${v.memberName}' has no value type")
            .flatMap(t => descend(basePath, parentAddr, t, addr, rest, v.endian, v.wordSizeBits))
      case Some(p: MemberSubRoute.PointerSubRoute) =>
        val wordBytes = p.wordSizeBits / 8
        parts.drop(1) match {
          case Nil if p.isArray =>
            Left(s"Array pointer '${p.memberName}' requires a numeric index")
          case Nil =>
            val addr = p.baseAddress + p.memberOffsetBytes.toLong
            Right(
              ResolvedMemberRead(
                address = addr,
                sizeBytes = wordBytes,
                endian = p.endian,
                wordSizeBits = p.wordSizeBits,
                valueType = None,
                decodeCodec = Some(pointerSlotCodec(p.endian, p.wordSizeBits)),
                sourceOffsetInParent = (addr - parentAddr).toInt,
                parentPath = basePath,
                isPointerSlot = true
              )
            )
          case indexStr :: Nil if p.isArray && indexStr.forall(_.isDigit) =>
            val addr =
              p.baseAddress + p.memberOffsetBytes.toLong + indexStr.toInt.toLong * wordBytes
            Right(
              ResolvedMemberRead(
                address = addr,
                sizeBytes = wordBytes,
                endian = p.endian,
                wordSizeBits = p.wordSizeBits,
                valueType = None,
                decodeCodec = Some(pointerSlotCodec(p.endian, p.wordSizeBits)),
                sourceOffsetInParent = (addr - parentAddr).toInt,
                parentPath = basePath,
                isPointerSlot = true
              )
            )
          case _ =>
            Left(
              s"Cannot watch through pointer '${p.memberName}'; focus the pointee first."
            )
        }
    }
  }

  private def atSubRoute(
      basePath: String,
      parentAddr: Long,
      v: MemberSubRoute.ValueSubRoute,
      address: Long
  ): Either[String, ResolvedMemberRead] = {
    val size = v.elementSizeBytes.getOrElse(0)
    if (size <= 0) Left(s"Unable to determine size for ${v.memberName}")
    else
      Right(
        ResolvedMemberRead(
          address = address,
          sizeBytes = size,
          endian = v.endian,
          wordSizeBits = v.wordSizeBits,
          valueType = v.valueType,
          decodeCodec = v.decodeCodec,
          sourceOffsetInParent = (address - parentAddr).toInt,
          parentPath = basePath
        )
      )
  }

  private def descend(
      basePath: String,
      parentAddr: Long,
      rootType: IrType,
      baseAddress: Long,
      segments: List[String],
      endian: IrEndian,
      wordSizeBits: Int
  ): Either[String, ResolvedMemberRead] =
    JsonMemoryEncoder.resolveLeaf(rootType, segments, Some(wordSizeBits)).flatMap {
      case (leafType, member, relOffset) =>
        val address = baseAddress + relOffset.toLong
        val size =
          member.readSizeBytes
            .orElse(
              HttpRouteIrEmitter
                .sizeBytesForType(effectiveType(leafType, member), Some(wordSizeBits))
                .toOption
            )
            .getOrElse(0)
        if (size <= 0)
          Left(s"Unable to determine size for nested path ${segments.mkString("/")}")
        else {
          val tpe = effectiveType(leafType, member)
          val memberEndian = member.endianOverride.getOrElse(endian)
          Right(
            ResolvedMemberRead(
              address = address,
              sizeBytes = size,
              endian = memberEndian,
              wordSizeBits = wordSizeBits,
              valueType = Some(tpe),
              decodeCodec = HttpRouteIrEmitter.codecForType(tpe, memberEndian, Some(wordSizeBits)),
              sourceOffsetInParent = (address - parentAddr).toInt,
              parentPath = basePath
            )
          )
        }
    }

  private def effectiveType(leafType: IrType, member: IrMember): IrType =
    if (member.isPointer && !member.target.isInstanceOf[IrType.FunctionPointer])
      IrType.Primitive(IrPrimitive.LongWord)
    else
      member.primitiveOverride.map(IrType.Primitive.apply).getOrElse(leafType)

  private def pointerSlotCodec(endian: IrEndian, wordSizeBits: Int): Codec[Json] = {
    import scodec.codecs._
    val word = wordSizeBits match {
      case 64 =>
        if (endian == IrEndian.Big) int64 else int64L
      case _ =>
        if (endian == IrEndian.Big) uint32 else uint32L
    }
    word.xmap[Json](
      value => Json.fromString(DapAddress.format(value)),
      json =>
        json.asString
          .flatMap(DapAddress.parse)
          .getOrElse(0L)
    )
  }
}
