package reconstruct
import org.scalatest.FreeSpec
import DerivedReconstruct._
import ReconstructDerivationSpec._
import org.scalatest.Matchers

class ReconstructDerivationSpec extends FreeSpec with Matchers {


  "Derived Reconstruct instance" - {
    "Case class" - {
      val childCaseClass = Child(a = 10, b = "hello")
      val expectedChildCaseClass = """Child(a = 10, b = "hello")"""

      "works when derived for itself" in { 
        implicitly[Reconstruct[Parent]].showCode(childCaseClass) shouldBe  expectedChildCaseClass
      }

      "works when derived from parent" in {
        implicitly[Reconstruct[Child]].showCode(childCaseClass) shouldBe expectedChildCaseClass
      }
    }

    "Case object" - {
      val expectedCode = "ChildObject"
      "works when derived for itself" in {
        implicitly[Reconstruct[ChildObject.type]].showCode(ChildObject) shouldBe expectedCode
      }

      "works for parent sealed trait derivation" in {
        implicitly[Reconstruct[Parent]].showCode(ChildObject) shouldBe expectedCode
      }
    }
  }
}

object ReconstructDerivationSpec {
  sealed trait Parent
  final case class Child(a: Int, b: String) extends Parent
  case object ChildObject extends Parent
}   
