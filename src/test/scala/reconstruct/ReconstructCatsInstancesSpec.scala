package reconstruct

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.implicits._

class ReconstructCatsInstancesSpec extends ReconstructSuite {

  "Reconstruct[NonEmptyList[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: NonEmptyList[Int] =>
        evalAndAssertEqual(Reconstruct[NonEmptyList[Int]].showCode(input), input, Seq("cats.data.NonEmptyList"))
      }
    }
  }

  "Reconstruct[NonEmptyVector[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: NonEmptyVector[Int] =>
        evalAndAssertEqual(Reconstruct[NonEmptyVector[Int]].showCode(input), input, Seq("cats.data.NonEmptyVector"))
      }
    }
  }

  "Reconstruct[NonEmptySet[A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: NonEmptySet[Int] =>
        evalAndAssertEqual(
          Reconstruct[NonEmptySet[Int]].showCode(input),
          input,
          Seq("cats.data.NonEmptySet", "cats.implicits._")
        )
      }
    }
  }

  "Reconstruct[NonEmptyMap[K, V]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: NonEmptyMap[Double, String] =>
        evalAndAssertEqual(
          Reconstruct[NonEmptyMap[Double, String]].showCode(input),
          input,
          Seq("cats.data.NonEmptyMap", "cats.implicits._")
        )
      }
    }
  }

  "Reconstruct[Validated[E, A]]" should {
    "Property: Produce correct reconstruction code" in {
      forAll { input: Validated[String, Int] =>
        evalAndAssertEqual(Reconstruct[Validated[String, Int]].showCode(input), input, Seq("cats.data.Validated._"))
      }
    }
  }

}
