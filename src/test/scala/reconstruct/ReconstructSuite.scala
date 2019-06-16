package reconstruct

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReconstructSuite extends WordSpec with Matchers with ScalaCheckPropertyChecks {

  import scala.reflect.runtime.universe
  import scala.tools.reflect.ToolBox
  private val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  def evalAndAssertEqual[A](strToEval: String, expected: A, imports: Seq[String] = Seq.empty) = {
    val allCode = imports.map("import " + _).mkString("\n") + "\n" + strToEval
    //TODOO: delme
//    println(allCode)
    val result = tb.eval(tb.parse(allCode)) == expected
    if (!result) println(allCode)
    assert(tb.eval(tb.parse(allCode)) == expected)
  }

}
