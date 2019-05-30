package reconstruct
import magnolia._
import language.experimental.macros

object DerivedReconstruct {

  type Typeclass[T] = Reconstruct[T]

  def combine[T](ctx: CaseClass[Reconstruct, T]): Reconstruct[T] = new Reconstruct[T] {
    override def showCode(value: T): String = {
      val contents = if (ctx.parameters.nonEmpty) {
        val fields = ctx.parameters
          .map { p =>
            p.label + " = " + p.typeclass.showCode(p.dereference(value))
          }
            .mkString(", ")
            s"(${fields})"
      }
      else ""
      s"""${ctx.typeName.short}${contents}"""
    }
  }

  def dispatch[T](ctx: SealedTrait[Reconstruct, T]): Reconstruct[T] =
    new Reconstruct[T] {
      override def showCode(t: T): String = {
        ctx.dispatch(t) { subtype =>
          subtype.typeclass.showCode(subtype.cast(t))
        }
      }
    }

  implicit def gen[T]: Reconstruct[T] = macro Magnolia.gen[T]

}
