package com.example
import DerivedReconstruct._

object App {

  final case class Boo(i: Int, s: String)

  def main(args: Array[String]): Unit = {
    println(implicitly[Reconstruct[Boo]].showCode(Boo(1, "as")))
  }

}
