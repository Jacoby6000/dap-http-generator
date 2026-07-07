package io.github.jacoby6000.daphttp

final case class CStruct(name: String, fields: List[CField])
final case class CField(
    typeName: String,
    name: String,
    isPointer: Boolean,
    arrayLength: Option[Int]
)
