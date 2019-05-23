package com.example

trait Reconstruct[T] {
  def showCode(t: T): String
}

object Reconstruct {
  implicit val reconstructInt: Reconstruct[Int] = new Reconstruct[Int] {
    override def showCode(t: Int): String = {
      t.toString
    }
  }

  implicit val reconstructDouble: Reconstruct[Double] = new Reconstruct[Double] {
    override def showCode(t: Double): String = {
      t.toString
    }
  }

  // TODOO: replace special chars like \, \r, \n, "
  implicit val reconstructString: Reconstruct[String] = new Reconstruct[String] {
    override def showCode(t: String): String = {
      s""""${t}""""
    }
  }
}
