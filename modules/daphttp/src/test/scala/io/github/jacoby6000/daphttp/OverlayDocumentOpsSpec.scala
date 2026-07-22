package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

final class OverlayDocumentOpsSpec extends AnyFunSuite {
  private val member = OverlayMember("x", "u8", isArray = false, None, false)

  test("normalizeNewStructId prefixes overlay namespace") {
    assert(OverlayDocumentOps.normalizeNewStructId("Foo") == "overlay#Foo")
    assert(OverlayDocumentOps.normalizeNewStructId("ns#Foo") == "ns#Foo")
  }

  test("membersForStruct reads overlays and newStructs") {
    val doc = TypeOverlayDocument(
      structs = Map("game#S" -> OverlayStructDef(List(member))),
      newStructs = List(OverlayNewStruct("overlay#N", List(member.copy(name = "y"))))
    )
    assert(OverlayDocumentOps.membersForStruct(doc, "game#S").contains(List(member)))
    assert(
      OverlayDocumentOps
        .membersForStruct(doc, "overlay#N")
        .contains(List(member.copy(name = "y")))
    )
    assert(
      OverlayDocumentOps
        .membersForStruct(doc, "N")
        .contains(List(member.copy(name = "y")))
    )
  }

  test("membersForStruct finds unqualified newStructs via normalized id") {
    val doc = TypeOverlayDocument(
      structs = Map.empty,
      newStructs = List(OverlayNewStruct("Foo", List(member)))
    )
    assert(OverlayDocumentOps.membersForStruct(doc, "overlay#Foo").contains(List(member)))
    assert(OverlayDocumentOps.membersForStruct(doc, "Foo").contains(List(member)))
  }

  test("validateDraftMembers rejects empty names and types") {
    assert(OverlayDocumentOps.validateDraftMembers(Nil).isDefined)
    assert(OverlayDocumentOps.validateDraftMembers(List(member.copy(name = ""))).isDefined)
    assert(OverlayDocumentOps.validateDraftMembers(List(member.copy(typeId = ""))).isDefined)
    assert(OverlayDocumentOps.validateDraftMembers(List(member)).isEmpty)
  }

  test("applyStructMembers updates structs and matching newStructs") {
    val doc = TypeOverlayDocument(
      structs = Map.empty,
      newStructs = List(OverlayNewStruct("overlay#N", List(member)))
    )
    val next =
      OverlayDocumentOps.applyStructMembers(doc, "overlay#N", List(member.copy(name = "z")))
    assert(next.structs("overlay#N").members.head.name == "z")
    assert(next.newStructs.head.members.head.name == "z")
  }

  test("applyStructMembers stores under normalized key") {
    val doc = TypeOverlayDocument(
      structs = Map("Foo" -> OverlayStructDef(List(member))),
      newStructs = List(OverlayNewStruct("overlay#Foo", List(member)))
    )
    val next = OverlayDocumentOps.applyStructMembers(doc, "Foo", List(member.copy(name = "z")))
    assert(!next.structs.contains("Foo"))
    assert(next.structs("overlay#Foo").members.head.name == "z")
    assert(next.newStructs.head.members.head.name == "z")
  }

  test("addNewStruct rejects duplicates") {
    val doc = TypeOverlayDocument.empty
    OverlayDocumentOps.addNewStruct(doc, "Foo", List(member)) match {
      case Left(err) =>
        fail(s"expected Right, got Left($err)")
      case Right((created, id)) =>
        assert(id == "overlay#Foo")
        assert(OverlayDocumentOps.addNewStruct(created, "Foo", List(member)).isLeft)
        assert(OverlayDocumentOps.addNewStruct(created, "overlay#Foo", List(member)).isLeft)
    }
  }

  test("addNewStruct rejects unqualified id when overlay# form already exists") {
    val doc = TypeOverlayDocument(
      structs = Map.empty,
      newStructs = List(OverlayNewStruct("Foo", List(member)))
    )
    assert(OverlayDocumentOps.addNewStruct(doc, "overlay#Foo", List(member)).isLeft)
    assert(OverlayDocumentOps.addNewStruct(doc, "Foo", List(member)).isLeft)
  }

  test("removeStructOverlay clears structs and matching newStructs") {
    val doc = TypeOverlayDocument(
      structs = Map(
        "game#S" -> OverlayStructDef(List(member)),
        "overlay#N" -> OverlayStructDef(List(member.copy(name = "z")))
      ),
      newStructs = List(OverlayNewStruct("overlay#N", List(member.copy(name = "z"))))
    )
    val afterNew = OverlayDocumentOps.removeStructOverlay(doc, "overlay#N")
    assert(!afterNew.structs.contains("overlay#N"))
    assert(afterNew.newStructs.isEmpty)
    assert(afterNew.structs.contains("game#S"))

    val afterIr = OverlayDocumentOps.removeStructOverlay(doc, "game#S")
    assert(!afterIr.structs.contains("game#S"))
    assert(afterIr.newStructs.size == 1)
  }

  test("removeStructOverlay clears normalized structs key when reset with unqualified id") {
    val doc = TypeOverlayDocument(
      structs = Map("overlay#Foo" -> OverlayStructDef(List(member))),
      newStructs = List(OverlayNewStruct("overlay#Foo", List(member)))
    )
    val after = OverlayDocumentOps.removeStructOverlay(doc, "Foo")
    assert(after.structs.isEmpty)
    assert(after.newStructs.isEmpty)
  }
}
