package reconstruct

import org.scalatest.{Matchers, WordSpec}

class TypeNameSpec extends WordSpec with Matchers {
  "Default instance using TypeTag" should {
    "provide unqualified type name" in {
      implicitly[TypeName[Seq[Int]]].value shouldBe "Seq[Int]"
      implicitly[TypeName[String]].value shouldBe "String"
    }

    "Provide (non-concrete) name for type parameters" in {
      def func[A](vec: Vector[A]): String = {
        implicitly[TypeName[Vector[A]]].value
      }

      func[Int](Vector.empty) shouldBe "Vector[A]"
    }

    "Provide (non-concrete) name for type members" in {
      def func(hasTypeMember: HasTypeMember): String = {
        implicitly[TypeName[hasTypeMember.T]].value
      }

      func(new HasTypeMember { type T = String} ) shouldBe "hasTypeMember.T"

    }

  }

  class HasTypeMember {
    type T
  }
}
