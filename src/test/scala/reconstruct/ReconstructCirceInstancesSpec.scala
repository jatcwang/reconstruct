package reconstruct

import io.circe.Json
import io.circe.parser.parse
import org.scalacheck.{Arbitrary, Gen}

class ReconstructCirceInstancesSpec extends ReconstructSuite {

  private val expected: Json = parse(
    """
      {
        "rofl": ["maojk", 12] ,
        "12": 12,
        "la": null
      }
    """).right.get
  //TODOO: use actual arbitrary
  implicit val arbJson: Arbitrary[Json] = Arbitrary(Gen.const(expected))

  "Reconstruct[Json]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Json =>
        evalAndAssertEqual(Reconstruct[Json].showCode(input), input, Seq("io.circe.parser.parse"))
      }
    }
  }

}
