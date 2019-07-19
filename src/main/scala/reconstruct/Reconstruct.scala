package reconstruct

import java.net.URI
import java.util.UUID

import cats.data._
import io.circe.{Json, JsonNumber, JsonObject}

trait Reconstruct[T] {
  def showCode(input: T): String
}

// TDOOO: make instances optional
object Reconstruct {
  def apply[T](implicit r: Reconstruct[T]): Reconstruct[T] = r

  implicit val reconstructInt: Reconstruct[Int] = new Reconstruct[Int] {
    override def showCode(t: Int): String = {
      t.toString
    }
  }

  implicit val reconstructFloat: Reconstruct[Float] = new Reconstruct[Float] {
    override def showCode(t: Float): String = {
      s"""${t.toString}f"""
    }
  }

  implicit val reconstructDouble: Reconstruct[Double] = new Reconstruct[Double] {
    override def showCode(t: Double): String = {
      t.toString
    }
  }

  implicit val reconstructLong: Reconstruct[Long] = new Reconstruct[Long] {
    override def showCode(t: Long): String = {
      t.toString + "l"
    }
  }

  implicit val reconstructBigDecimal: Reconstruct[BigDecimal] = new Reconstruct[BigDecimal] {
    override def showCode(t: BigDecimal): String = {
      s"""BigDecimal("${t.toString}")"""
    }
  }

  implicit val reconstructBigInt: Reconstruct[BigInt] = new Reconstruct[BigInt] {
    override def showCode(t: BigInt): String = {
      s"""BigInt("${t.toString}")"""
    }
  }

  implicit val reconstructString: Reconstruct[String] = new Reconstruct[String] {
    override def showCode(str: String): String = {
      val builder = new StringBuilder()
      builder.append('"')
      str.foreach {
        case '\n' => builder.append("""\n""")
        case '\r' => builder.append("""\r""")
        case '\t' => builder.append("""\t""")
        case '\\' => builder.append("""\\""")
        case '"' => builder.append("""\"""")
        case c => builder.append(c)
      }
      builder.append('"').result()
    }
  }

  def reconstructIterable[F[X] <: Iterable[X], A](className: String, typeName: TypeName[A])(
    implicit reconA: Reconstruct[A]
  ): Reconstruct[F[A]] = new Reconstruct[F[A]] {
    override def showCode(values: F[A]): String = {
      if (values.isEmpty) className + s".empty[$typeName]"
      else {
        val builder = new StringBuilder
        builder
          .append(className)
          .append("(")
        appendForEach(values)(builder, a => builder.append(reconA.showCode(a)))
        builder.append(")").result
      }
    }
  }

  implicit def reconstructOption[A](implicit reconA: Reconstruct[A]): Reconstruct[Option[A]] =
    new Reconstruct[Option[A]] {
      override def showCode(input: Option[A]): String = input match {
        case Some(value) => s"""Some(${reconA.showCode(value)})"""
        case None => "None"
      }
    }

  implicit def reconstructEither[A, B](
    implicit reconA: Reconstruct[A],
    reconB: Reconstruct[B]
  ): Reconstruct[Either[A, B]] = new Reconstruct[Either[A, B]] {
    override def showCode(input: Either[A, B]): String = {
      input match {
        case Left(l) => s"""Left(${reconA.showCode(l)})"""
        case Right(r) => s"""Right(${reconB.showCode(r)})"""
      }
    }
  }

  implicit def reconstructSeq[A](implicit recon: Reconstruct[A], typeName: TypeName[A]): Reconstruct[Seq[A]] =
    reconstructIterable("Seq", typeName)

  implicit def reconstructList[A](implicit recon: Reconstruct[A], typeName: TypeName[A]): Reconstruct[List[A]] =
    reconstructIterable("List", typeName)

  implicit def reconstructVector[A](implicit recon: Reconstruct[A], typeName: TypeName[A]): Reconstruct[Vector[A]] =
    reconstructIterable("Vector", typeName)

  implicit def reconstructSet[A](implicit recon: Reconstruct[A], typeName: TypeName[A]): Reconstruct[Set[A]] =
    reconstructIterable("Set", typeName)

  implicit def reconstructMap[K, V](
    implicit reconK: Reconstruct[K],
    reconV: Reconstruct[V],
    typeNameK: TypeName[K],
    typeNameV: TypeName[V]
  ): Reconstruct[Map[K, V]] =
    new Reconstruct[Map[K, V]] {
      override def showCode(input: Map[K, V]): String = {
        if (input.isEmpty) s"Map.empty[${typeNameK.value}, ${typeNameV.value}]"
        else {
          val builder = new StringBuilder
          builder.append("Map(")
          appendForEach(input)(builder, keyVal => appendReconstructKeyValuePair(builder, keyVal))
          builder.append(")").result
        }
      }
    }

  implicit val reconstructUuid: Reconstruct[UUID] = new Reconstruct[UUID] {
    override def showCode(value: UUID): String = {
      s"""UUID.fromString("${value.toString}")"""
    }
  }

  implicit val reconstructUri: Reconstruct[URI] = new Reconstruct[URI] {
    override def showCode(input: URI): String = {
      s"""new URI("${input.toString}")"""
    }
  }

  implicit def reconstructNonEmptyList[A](implicit reconA: Reconstruct[A]): Reconstruct[NonEmptyList[A]] =
    new Reconstruct[NonEmptyList[A]] {
      override def showCode(input: NonEmptyList[A]): String = {
        val builder = new StringBuilder
        builder.append("NonEmptyList.of(")
        appendForEach(input.toList)(builder, a => builder.append(reconA.showCode(a)))
        builder.append(')').result
      }
    }

  implicit def reconstructNonEmptyVector[A](implicit reconA: Reconstruct[A]): Reconstruct[NonEmptyVector[A]] =
    new Reconstruct[NonEmptyVector[A]] {
      override def showCode(input: NonEmptyVector[A]): String = {
        val builder = new StringBuilder
        builder.append("NonEmptyVector.of(")
        appendForEach(input.toVector)(builder, a => builder.append(reconA.showCode(a)))
        builder.append(')').result
      }
    }

  implicit def reconstructNonEmptySet[A](implicit reconA: Reconstruct[A]): Reconstruct[NonEmptySet[A]] =
    new Reconstruct[NonEmptySet[A]] {
      override def showCode(input: NonEmptySet[A]): String = {
        val builder = new StringBuilder
        builder.append("NonEmptySet.of(")
        appendForEach(input.toSortedSet)(builder, a => builder.append(reconA.showCode(a)))
        builder.append(')').result
      }
    }

//TODOO: test
  implicit def reconstructNonEmptyMap[K, V](implicit reconK: Reconstruct[K], reconV: Reconstruct[V]) =
    new Reconstruct[NonEmptyMap[K, V]] {
      override def showCode(input: NonEmptyMap[K, V]): String = {
        val builder = new StringBuilder
        builder.append("NonEmptyMap.of(")
        appendForEach(input.toSortedMap)(
          builder,
          keyVal => appendReconstructKeyValuePair(builder, keyVal)
        )
        builder.append(')').result
      }
    }

  implicit def reconstructValidated[E, A](
    implicit reconE: Reconstruct[E],
    reconA: Reconstruct[A],
    typeNameE: TypeName[E],
    typeNameA: TypeName[A]
  ): Reconstruct[Validated[E, A]] = new Reconstruct[Validated[E, A]] {
    override def showCode(input: Validated[E, A]): String = {
      input match {
        case Validated.Valid(a) => s"""Validated.(${reconA.showCode(a)})"""
        case Validated.Invalid(e) => s"""Invalid(${reconE.showCode(e)})"""
      }
    }
  }

  implicit val reconstructJson: Reconstruct[Json] = {
    def addPrePostFixToJsonString(jsonString: String): String = {
      val prefix = "parse(s" + "\"\"\""
      val postfix = "\"\"\").right.get"
      s"$prefix$jsonString$postfix"
    }

    val circeParserReconstructor: Reconstruct[Json] = new Reconstruct[Json] {
      override def showCode(input: Json): String = {
        addPrePostFixToJsonString(input.noSpaces)
      }
    }

    val folder = new Json.Folder[String] {
      override def onNull: String = "Json.Null"

      override def onBoolean(value: Boolean): String =
        if (value) {
          "Json.True"
        } else "Json.False"

      override def onNumber(value: JsonNumber): String = {
        // Handle negative zeros
        val strValue = value.toString
        if (strValue == "-0" || strValue == "-0E+0")
          addPrePostFixToJsonString(strValue)
        else {
          value.toInt
            .map(n => s"Json.fromInt(${reconstructInt.showCode(n)})")
            .orElse(
              value.toLong.map(n => s"Json.fromLong(${reconstructLong.showCode(n)})")
            )
            .getOrElse(
              addPrePostFixToJsonString(strValue)
            )

        }
      }

      override def onString(value: String): String = {
        s"Json.fromString(${reconstructString.showCode(value)})"
      }

      override def onArray(value: Vector[Json]): String = circeParserReconstructor.showCode(Json.fromValues(value))

      override def onObject(value: JsonObject): String = circeParserReconstructor.showCode(Json.fromJsonObject(value))
    }

    new Reconstruct[Json] {
      override def showCode(input: Json): String = input.foldWith(folder)
    }
  }

  private def appendForEach[A](
    input: Iterable[A]
  )(builder: StringBuilder, appendOneFunc: A => StringBuilder): StringBuilder = {
    input.foreach { a =>
      appendOneFunc(a)
      builder.append(',')
    }
    builder.deleteCharAt(builder.length - 1)
  }

  private def appendReconstructKeyValuePair[K, V](
    builder: StringBuilder,
    keyVal: (K, V)
  )(implicit reconK: Reconstruct[K], reconV: Reconstruct[V]): StringBuilder = {
    builder
      .append(reconK.showCode(keyVal._1))
      .append(" -> ")
      .append(reconV.showCode(keyVal._2))
  }

  // TODOO: EitherT?
  // TODOO tuple instances
}
