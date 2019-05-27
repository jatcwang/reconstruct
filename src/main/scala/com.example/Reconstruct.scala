package com.example

trait Reconstruct[T] {
  def showCode(t: T): String
}

trait ClassReconstruct[T] extends Reconstruct[T] { self =>
  def body(t: T): String
  def constructor(t: T): String

  final def showCode(t: T): String = {
    s"""${constructor(t)}${body(t)}"""
  }

  final def withConstructor(constructorStr: String): ClassReconstruct[T] = new ClassReconstruct[T] {
    override def body(t: T): String = self.body(t)
    override def constructor(t: T): String = constructorStr
  }

  final def withConstructor(mkConstructor: T => String): ClassReconstruct[T] = new ClassReconstruct[T] {
    override def body(t: T): String = self.body(t)
    override def constructor(t: T): String = mkConstructor(t)
  }
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
