package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

import scala.collection.mutable

object IrSmithyGenerator {
  private val DapTraitsNamespace = "com.jacoby6000.daphttp"
  private val BytesShapeId = s"$DapTraitsNamespace#Bytes"
  private val BitsShapeId = s"$DapTraitsNamespace#Bits"

  /** Generates Smithy IDL text for each namespace found in the IR services. Returns a map from
    * namespace string to Smithy IDL text. The generated definitions reference traits from the
    * `com.jacoby6000.daphttp` namespace, which must be on the model assembler's classpath (e.g.
    * from `dap-http-traits.smithy`).
    */
  def generateSmithyFromIr(services: List[IrService]): Map[String, String] = {
    val byNamespace = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[IrService]]
    services.foreach { service =>
      val ns = inferNamespace(service)
      byNamespace.getOrElseUpdate(ns, mutable.ListBuffer.empty) += service
    }
    byNamespace.toMap.map { case (ns, svcs) => ns -> generateForNamespace(ns, svcs.toList) }
  }

  private def inferNamespace(service: IrService): String =
    service.operations.headOption.map(_.output.id.getNamespace).getOrElse("generated")

  private def generateForNamespace(namespace: String, services: List[IrService]): String = {
    val usedImports = mutable.LinkedHashSet.empty[String]
    val emittedShapes = mutable.LinkedHashSet.empty[ShapeId]
    val buf = new StringBuilder

    def addLine(s: String): Unit = {
      val _ = buf.append(s)
      val _ = buf.append('\n')
    }

    def useImport(fqn: String): Unit = {
      val _ = usedImports.add(fqn)
    }

    def useTrait(name: String): Unit = useImport(s"$DapTraitsNamespace#$name")

    def endianString(endian: IrEndian): String = endian match {
      case IrEndian.Big    => "big"
      case IrEndian.Little => "little"
    }

    /** Returns the daphttp trait name for a primitive kind, or None for kinds that have no
      * corresponding member-level trait.
      */
    def primitiveTraitName(kind: IrPrimitive): Option[String] = kind match {
      case IrPrimitive.U8       => Some("u8")
      case IrPrimitive.S8       => Some("s8")
      case IrPrimitive.U16      => Some("u16")
      case IrPrimitive.S16      => Some("s16")
      case IrPrimitive.U32      => Some("u32")
      case IrPrimitive.S32      => Some("s32")
      case IrPrimitive.U64      => Some("u64")
      case IrPrimitive.S64      => Some("s64")
      case IrPrimitive.U128     => Some("u128")
      case IrPrimitive.S128     => Some("s128")
      case IrPrimitive.F8       => Some("f8")
      case IrPrimitive.F16      => Some("f16")
      case IrPrimitive.F32      => Some("f32")
      case IrPrimitive.F64      => Some("f64")
      case IrPrimitive.Char     => Some("char")
      case IrPrimitive.Bool     => None
      case IrPrimitive.LongWord => None
    }

    /** Returns the trait name that must be emitted when this primitive is used as a direct
      * (non-overridden) member target, i.e. when the Smithy base shape type alone would not
      * round-trip back to the same primitive. Kinds that are already captured faithfully by their
      * natural Smithy type (Boolean→Bool, Byte→S8, Short→S16, Integer→S32, Long→LongWord,
      * Float→F32, Double→F64) return None.
      */
    def implicitPrimitiveTraitName(kind: IrPrimitive): Option[String] = kind match {
      case IrPrimitive.S8       => None
      case IrPrimitive.S16      => None
      case IrPrimitive.S32      => None
      case IrPrimitive.F32      => None
      case IrPrimitive.F64      => None
      case IrPrimitive.Bool     => None
      case IrPrimitive.LongWord => None
      case other                => primitiveTraitName(other)
    }

    /** Maps an IrPrimitive to its closest Smithy prelude shape name. */
    def primitiveSmithyType(kind: IrPrimitive): String = kind match {
      case IrPrimitive.Bool     => "Boolean"
      case IrPrimitive.S8       => "Byte"
      case IrPrimitive.U8       => "Byte"
      case IrPrimitive.S16      => "Short"
      case IrPrimitive.U16      => "Short"
      case IrPrimitive.S32      => "Integer"
      case IrPrimitive.U32      => "Integer"
      case IrPrimitive.S64      => "Long"
      case IrPrimitive.U64      => "Long"
      case IrPrimitive.U128     => "Long"
      case IrPrimitive.S128     => "Long"
      case IrPrimitive.F8       => "Byte"
      case IrPrimitive.F16      => "Short"
      case IrPrimitive.F32      => "Float"
      case IrPrimitive.F64      => "Double"
      case IrPrimitive.Char     => "Byte"
      case IrPrimitive.LongWord => "Long"
    }

    def localName(id: ShapeId): String =
      if (id.getNamespace == namespace || id.getNamespace == "smithy.api") id.getName
      else id.toString

    def memberTargetName(irType: IrType): String = irType match {
      case IrType.Primitive(kind) => primitiveSmithyType(kind)
      case struct: IrType.Struct  => localName(struct.id)
      case union: IrType.Union    => localName(union.id)
      case list: IrType.ListType  =>
        if (list.bytesAlias) { useImport(BytesShapeId); "Bytes" }
        else if (list.bitsAlias) { useImport(BitsShapeId); "Bits" }
        else localName(list.id)
      case map: IrType.MapType => localName(map.id)
      case IrType.Ref(id)      => localName(id)
    }

    def emitMember(member: IrMember, indent: String): Unit = {
      member.staticAddress.foreach { addr =>
        useTrait("staticAddress")
        addLine(s"""${indent}@staticAddress("0x${addr.toHexString}")""")
      }
      member.paddingRepeats.foreach { n =>
        useTrait("padding")
        addLine(s"${indent}@padding($n)")
      }
      if (member.isPointer) {
        useTrait("pointer")
        addLine(s"${indent}@pointer")
      }
      if (member.isArray) {
        useTrait("array")
        addLine(s"${indent}@array")
      }
      member.arrayLength.foreach { n =>
        useTrait("length")
        addLine(s"${indent}@length($n)")
      }
      member.endianOverride.foreach { endian =>
        useTrait("endian")
        addLine(s"""${indent}@endian("${endianString(endian)}")""")
      }
      // Explicit primitive override: emit the corresponding trait.
      member.primitiveOverride.foreach { prim =>
        primitiveTraitName(prim).foreach { t =>
          useTrait(t)
          addLine(s"$indent@$t")
        }
      }
      // Implicit primitive trait: emit only when the kind cannot be recovered
      // from the Smithy base type alone and there is no explicit override.
      if (member.primitiveOverride.isEmpty) {
        member.target match {
          case IrType.Primitive(kind) =>
            implicitPrimitiveTraitName(kind).foreach { t =>
              useTrait(t)
              addLine(s"$indent@$t")
            }
          case _ => ()
        }
      }
      addLine(s"$indent${member.name}: ${memberTargetName(member.target)}")
    }

    def emitTypeShape(irType: IrType): Unit = irType match {
      case struct: IrType.Struct if !emittedShapes.contains(struct.id) =>
        val _ = emittedShapes.add(struct.id)
        // Emit dependencies first so references resolve in order.
        struct.members.foreach(m => emitTypeShape(m.target))
        struct match {
          case _: IrType.Bitmask =>
            useTrait("bitmask")
            addLine("@bitmask")
          case _: IrType.MemoryMappedStruct =>
            useTrait("dapStruct")
            addLine("@dapStruct")
          case _: IrType.EnclosingStruct => ()
        }
        struct.declaredSizeBits.foreach { bits =>
          useTrait("size")
          addLine(s"@size($bits)")
        }
        addLine(s"structure ${localName(struct.id)} {")
        struct.members.foreach(m => emitMember(m, "    "))
        addLine("}")
        addLine("")

      case union: IrType.Union if !emittedShapes.contains(union.id) =>
        val _ = emittedShapes.add(union.id)
        union.members.foreach(m => emitTypeShape(m.target))
        addLine(s"union ${localName(union.id)} {")
        union.members.foreach(m => emitMember(m, "    "))
        addLine("}")
        addLine("")

      case list: IrType.ListType
          if !emittedShapes.contains(list.id) && !list.bytesAlias && !list.bitsAlias =>
        val _ = emittedShapes.add(list.id)
        emitTypeShape(list.element)
        addLine(s"list ${localName(list.id)} {")
        // Emit a trait on the list member if the element primitive needs one.
        list.element match {
          case IrType.Primitive(kind) =>
            implicitPrimitiveTraitName(kind).foreach { t =>
              useTrait(t)
              addLine(s"    @$t")
            }
          case _ => ()
        }
        addLine(s"    member: ${memberTargetName(list.element)}")
        addLine("}")
        addLine("")

      case map: IrType.MapType if !emittedShapes.contains(map.id) =>
        val _ = emittedShapes.add(map.id)
        emitTypeShape(map.key)
        emitTypeShape(map.value)
        addLine(s"map ${localName(map.id)} {")
        addLine(s"    key: ${memberTargetName(map.key)}")
        addLine(s"    value: ${memberTargetName(map.value)}")
        addLine("}")
        addLine("")

      case _ => ()
    }

    // ---- Service shapes ----
    services.foreach { service =>
      useTrait("wordSize")
      useTrait("endian")
      val wordSize = service.wordSizeBits.getOrElse(32)
      addLine(s"@wordSize($wordSize)")
      addLine(s"""@endian("${endianString(service.defaultEndian)}")""")
      addLine(s"service ${service.name} {")
      addLine("""    version: "1"""")
      if (service.operations.nonEmpty) {
        val ops = service.operations.map(_.name).mkString(", ")
        addLine(s"    operations: [$ops]")
      }
      addLine("}")
      addLine("")
    }

    // ---- Operation shapes ----
    services.foreach { service =>
      service.operations.foreach { op =>
        addLine(s"operation ${op.name} {")
        addLine(s"    output: ${localName(op.output.id)}")
        addLine("}")
        addLine("")
      }
    }

    // ---- Type shapes (output structs and all reachable types) ----
    services.foreach { service =>
      service.operations.foreach(op => emitTypeShape(op.output))
    }

    // ---- Assemble the file ----
    val header = new StringBuilder
    val _ = header.append("$version: \"2\"\n\n")
    val _ = header.append(s"namespace $namespace\n")
    if (usedImports.nonEmpty) {
      val _ = header.append("\n")
      usedImports.toList.sorted.foreach { imp =>
        val _ = header.append(s"use $imp\n")
      }
    }
    val _ = header.append("\n")
    header.toString() + buf.toString()
  }
}
