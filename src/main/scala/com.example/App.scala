package com.example
import reconstruct.Reconstruct
import reconstruct.DerivedReconstruct._

object App {

  final case class Boo(i: Int, s: String)

  sealed trait Parent
  sealed trait Mid extends Parent
  sealed trait Mid2 extends Parent

  final case class Child1(s: Int) extends Mid
  case object Child2 extends Mid

  final case class Child2(s: Int) extends Mid2

  def main(args: Array[String]): Unit = {
    // println(implicitly[Reconstruct[Boo]].showCode(Boo(1, "as")))
    println(implicitly[Reconstruct[Parent]].showCode(Child2))
  }

}
