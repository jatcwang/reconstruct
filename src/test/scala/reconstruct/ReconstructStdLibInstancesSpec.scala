package reconstruct
import java.net.URI
import java.util.UUID

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReconstructStdLibInstancesSpec extends ReconstructSuite {

  "Reconstruct[String]" should {
    """"Encode a tab character as \t""" in {
      Reconstruct[String].showCode("ab\tcd") shouldBe """"ab\tcd""""
    }

    """Encode a newline as \n""" in {
      Reconstruct[String].showCode("ab\ncd") shouldBe """"ab\ncd""""
    }

    """Encode a carriage return as \r""" in {
      Reconstruct[String].showCode("ab\rcd") shouldBe """"ab\rcd""""
    }

    """Encode a double quote as """" in {
      Reconstruct[String].showCode(""" "" """) shouldBe """" \"\" """"
    }

    """Encode a backslash as \"""" in {
      Reconstruct[String].showCode("""\""") shouldBe """"\\""""
    }

    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { input: String =>
        evalAndAssertEqual(Reconstruct[String].showCode(input), input)
      }
    }

  }

  "Reconstruct[Int]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: Int =>
        evalAndAssertEqual(Reconstruct[Int].showCode(num), num)
      }
    }
  }

  "Reconstruct[Float]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: Float =>
        evalAndAssertEqual(Reconstruct[Float].showCode(num), num)
      }
    }
  }

  "Reconstruct[Double]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: Double =>
        evalAndAssertEqual(Reconstruct[Double].showCode(num), num)
      }
    }
  }

  "Reconstruct[Long]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: Long =>
        evalAndAssertEqual(Reconstruct[Long].showCode(num), num)
      }
    }
  }

  //TODOO: test failure
  /*

[info] - should Property: Produce correct reconstruction code *** FAILED ***
[info]   TestFailedException was thrown during property evaluation.
[info]     Message: NonEmptyList(-2147483648, -1372081638, 1089760091, 0, -2147483648, 916421874) did not equal NonEmptyList(-1, -2147483648, -1372081638, 1089760091, 0, -2147483648, 916421874)
[info]     Location: (ReconstructSuite.scala:18)
[info]     Occurred when passed generated values (
[info]       arg0 = NonEmptyList(-1, -2147483648, -1372081638, 1089760091, 0, -2147483648, 916421874)
[info]     )
   */

  "Reconstruct[BigDecimal]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: BigDecimal =>
        evalAndAssertEqual(Reconstruct[BigDecimal].showCode(num), num)
      }
    }
  }

  "Reconstruct[BigInt]" should {
    "Property: Produces string which when evaluated will yield the same value" in {
      forAll { num: BigInt =>
        evalAndAssertEqual(Reconstruct[BigInt].showCode(num), num)
      }
    }
  }

  "Reconstruct[Option[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Option[Double] =>
        evalAndAssertEqual(Reconstruct[Option[Double]].showCode(input), input)
      }
    }
  }

  "Reconstruct[Either[A, B]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Either[String, Double] =>
        evalAndAssertEqual(Reconstruct[Either[String, Double]].showCode(input), input)
      }
    }
  }

  "Reconstruct[Seq[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Seq[Double] =>
        evalAndAssertEqual(Reconstruct[Seq[Double]].showCode(input), input)
      }
    }

    "Empty collection outputs .empty" in {
      assert(Reconstruct[Seq[Double]].showCode(Seq.empty[Double]) == "Seq.empty[Double]")
    }
  }

  "Reconstruct[List[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: List[Double] =>
        evalAndAssertEqual(Reconstruct[List[Double]].showCode(input), input)
      }
    }

    "Empty collection outputs .empty" in {
      assert(Reconstruct[List[Double]].showCode(List.empty[Double]) == "List.empty[Double]")
    }
  }

  "Reconstruct[Vector[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Vector[Double] =>
        evalAndAssertEqual(Reconstruct[Vector[Double]].showCode(input), input)
      }
    }

    "Empty collection outputs .empty" in {
      assert(Reconstruct[Vector[Double]].showCode(Vector.empty[Double]) == "Vector.empty[Double]")
    }
  }

  "Reconstruct[Set[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Set[Double] =>
        evalAndAssertEqual(Reconstruct[Set[Double]].showCode(input), input)
      }
    }

    "Empty collection outputs .empty[A]" in {
      assert(Reconstruct[Set[Double]].showCode(Set.empty[Double]) == "Set.empty[Double]")
    }
  }

  "Reconstruct[Map[K, V]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Map[String, String] =>
        evalAndAssertEqual(Reconstruct[Map[String, String]].showCode(input), input)
      }
    }

    "Empty collection outputs .empty" in {
      assert(Reconstruct[Map[String, Int]].showCode(Map.empty[String, Int]) == "Map.empty[String, Int]")
    }
  }

  "Reconstruct[UUID]" should {
    "Reconstruct using UUID.fromString" in {
      val input = UUID.fromString("16a8aaf6-8741-461c-897f-d36fa244a3fd")
      assert(Reconstruct[UUID].showCode(input) == """UUID.fromString("16a8aaf6-8741-461c-897f-d36fa244a3fd")""")
    }

    "Property: Produce correct reconstruction code" in {
      forAll { input: UUID =>
        evalAndAssertEqual(Reconstruct[UUID].showCode(input), input, imports = Seq("java.util.UUID"))
      }
    }
  }

  "Reconstruct[URI]" should {
    "Reconstruct using constructor" in {
      val input = new URI("https://www.exmaple.com/path?query1=1&query2=two#123")
      evalAndAssertEqual(Reconstruct[URI].showCode(input), input, imports = Seq("java.net.URI"))
    }
  }

}
