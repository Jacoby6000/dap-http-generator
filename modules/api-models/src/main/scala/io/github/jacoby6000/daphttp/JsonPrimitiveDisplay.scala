package io.github.jacoby6000.daphttp

import io.circe.Json

object JsonPrimitiveDisplay {
  def cssClass(json: Json): String =
    if (json.isNull) "jv-null"
    else if (json.isBoolean) "jv-bool"
    else if (json.isNumber) "jv-num"
    else if (json.isString) "jv-str"
    else "jv-punct"

  def text(json: Json): String =
    json.fold(
      jsonNull = "null",
      jsonBoolean = _.toString,
      jsonNumber = _.toString,
      jsonString = s => Json.fromString(s).noSpaces,
      jsonArray = _ => "[]",
      jsonObject = _ => "{}"
    )
}
