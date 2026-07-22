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

  test("applyStructMembers stores newStructs under normalized key") {
    val doc = TypeOverlayDocument(
      structs = Map("Foo" -> OverlayStructDef(List(member))),
      newStructs = List(OverlayNewStruct("overlay#Foo", List(member)))
    )
    val next = OverlayDocumentOps.applyStructMembers(doc, "Foo", List(member.copy(name = "z")))
    assert(!next.structs.contains("Foo"))
    assert(next.structs("overlay#Foo").members.head.name == "z")
    assert(next.newStructs.head.members.head.name == "z")
  }

  test("applyStructMembers keeps unqualified IR overlay keys") {
    val doc = TypeOverlayDocument.empty
    val next =
      OverlayDocumentOps.applyStructMembers(doc, "Player", List(member.copy(name = "hp")))
    assert(next.structs.contains("Player"))
    assert(!next.structs.contains("overlay#Player"))
  }

  test("membersForStruct and removeStructOverlay resolve namespaced IR keys") {
    val doc = TypeOverlayDocument(
      structs = Map("game#Player" -> OverlayStructDef(List(member.copy(name = "hp")))),
      newStructs = Nil
    )
    assert(
      OverlayDocumentOps
        .membersForStruct(doc, "Player")
        .contains(List(member.copy(name = "hp")))
    )
    val after = OverlayDocumentOps.removeStructOverlay(doc, "Player")
    assert(after.structs.isEmpty)
  }

  test("applyStructMembers reuses existing namespaced IR key") {
    val doc = TypeOverlayDocument(
      structs = Map("game#Player" -> OverlayStructDef(List(member))),
      newStructs = Nil
    )
    val next =
      OverlayDocumentOps.applyStructMembers(doc, "Player", List(member.copy(name = "hp")))
    assert(next.structs.keySet == Set("game#Player"))
    assert(next.structs("game#Player").members.head.name == "hp")
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

  test("membersForStruct and remove resolve unqualified IR ids after PUT canonicalize") {
    val doc = TypeOverlayDocument(
      structs = Map("game#Player" -> OverlayStructDef(List(member.copy(name = "hp")))),
      newStructs = Nil
    )
    assert(
      OverlayDocumentOps
        .membersForStruct(doc, "Player")
        .contains(List(member.copy(name = "hp")))
    )
    val withClientKey = doc.copy(
      structs = doc.structs + ("overlay#Player" -> OverlayStructDef(List(member.copy(name = "x"))))
    )
    assert(
      OverlayDocumentOps
        .membersForStruct(withClientKey, "Player")
        .contains(List(member.copy(name = "hp")))
    )
    val after = OverlayDocumentOps.removeStructOverlay(withClientKey, "Player")
    assert(!after.structs.contains("game#Player"))
    assert(after.structs.contains("overlay#Player"))
  }

  test("ambiguous short IR names do not wipe multiple namespaced keys") {
    val doc = TypeOverlayDocument(
      structs = Map(
        "game#Player" -> OverlayStructDef(List(member.copy(name = "a"))),
        "other#Player" -> OverlayStructDef(List(member.copy(name = "b")))
      ),
      newStructs = Nil
    )
    assert(
      OverlayDocumentOps.matchingStructKeys(doc, "Player") == List("game#Player", "other#Player")
    )
    assert(OverlayDocumentOps.uniqueMatchingStructKey(doc, "Player").isEmpty)
    assert(OverlayDocumentOps.membersForStruct(doc, "Player").isEmpty)
    assert(OverlayDocumentOps.removeStructOverlay(doc, "Player") == doc)
  }
}
