package reconstruct
import scala.reflect.runtime.universe._

/** Typeclass providing the name of type */
trait TypeName[A] {
  def value: String
}

object TypeName {
  implicit def typeNameFromTypeTag[A](implicit tag: WeakTypeTag[A]): TypeName[A] = new TypeName[A] {
    override def value: String = {
      tag.tpe.toString
    }
  }
}
