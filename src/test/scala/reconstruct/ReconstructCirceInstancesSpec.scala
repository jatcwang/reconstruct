package reconstruct

import io.circe.{Json, JsonNumber}
import io.circe.parser.parse
import io.circe.testing.instances._

class ReconstructCirceInstancesSpec extends ReconstructSuite {

  "Reconstruct[Json]" should {
    "Property: Produce correct reconstruction code" in {
      implicit val gen = generatorDrivenConfig.copy(minSuccessful = 100000)
      forAll { input: Json =>
        evalAndAssertEqual(showCode(input), input, Seq("io.circe.parser.parse", "io.circe.Json"))
      }(gen, implicitly, implicitly, implicitly, implicitly, implicitly)
    }

    //TODOO: failed test case:
    /*
   "\r" : [
    51137323010114023643,
    false,
    false,
    -17.09085112173847102252137352566311,
    false,
    true,
    false,
    true,
    null,
    true
  ]
     */

    "Json object is reconstructed with circe parser and string JSON representation" in {
      val input: Json = parse("""
      {
        "key": ["value", 12] ,
        "12": 12,
        "la": null
      }
    """).right.get
      showCode(input) shouldBe
        "parse(s\"\"\"" + """{"key":["value",12],"12":12,"la":null}""" + "\"\"\").right.get"
    }

    "Json array is reconstructed with circe parser and string JSON representation" in {

      val input: Json = parse("""
      [
        {
          "key": "v"
        },
        12,
        "x"
      ]
    """).right.get
      showCode(input) shouldBe
        "parse(s\"\"\"" + """[{"key":"v"},12,"x"]""" + "\"\"\").right.get"
    }

    "Single JSON string is reconstructed as Json.fromString" in {
      showCode(Json.fromString("asdf")) shouldBe """Json.fromString("asdf")"""
    }

    "Single Json null is reconstructed as Json.Null" in {
      showCode(Json.Null) shouldBe "Json.Null"
    }

    "Single Json boolean should be reconstructed as Json.True/False" in {
      showCode(Json.True) shouldBe "Json.True"
      showCode(Json.False) shouldBe "Json.False"
    }

    "Single Json number is reconstructed as Json.fromInt if it is an integer" in {
      showCode(Json.fromInt(125)) shouldBe "Json.fromInt(125)"
    }

    "Single Json number: Handles negative zeroes" in {
      showCode(Json.fromJsonNumber(JsonNumber.fromString("-0").get)) shouldBe "parse(s\"\"\"-0\"\"\").right.get"
      showCode(Json.fromJsonNumber(JsonNumber.fromString("-0.0").get)) shouldBe "parse(s\"\"\"-0\"\"\").right.get"
      showCode(Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe("-0E+0"))) shouldBe "parse(s\"\"\"-0E+0\"\"\").right.get"
    }

    "Single Json number negative zero is reconstructed as Json.fromInt if it is an integer" in {
      showCode(Json.fromJsonNumber(JsonNumber.fromString("-0").get)) shouldBe """Json.fromString("-0")"""
      showCode(Json.fromJsonNumber(JsonNumber.fromString("-0.0").get)) shouldBe """Json.fromString("-0")"""
    }

    "Single Json number is reconstructed as Json.fromLong if it is a long" in {
      showCode(Json.fromLong(125322222222222382L)) shouldBe "Json.fromLong(125322222222222382l)"
    }

    "Single Json number is reconstructed with parse and string JSON representation if it's neither Int or Long" in {
      showCode(Json.fromDouble(12.12).get) shouldBe "parse(s\"\"\"12.12\"\"\").right.get"
      showCode(Json.fromBigInt(BigInt("18446744078005044483"))) shouldBe "parse(s\"\"\"18446744078005044483\"\"\").right.get"
      showCode(Json.fromBigDecimal(BigDecimal("18446744078005044483.00212012101"))) shouldBe "parse(s\"\"\"18446744078005044483.00212012101\"\"\").right.get"
    }
  }

  private def showCode(json: Json)(implicit reconstruct: Reconstruct[Json]): String = {
    reconstruct.showCode(json)
  }

}
