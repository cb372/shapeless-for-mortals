// Copyright (C) 2015 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
/**
 * TypeClass (api/impl/syntax) for marshalling objects into
 * `java.util.HashMap<String,Object>` (yay, big data!).
 */
package s4m.smbd

import java.util

import shapeless._, labelled.{ field, FieldType }

/**
 * This exercise involves writing tests, only a skeleton is provided.
 *
 * - Exercise 1.1: derive =BigDataFormat= for sealed traits.
 * - Exercise 1.2: define identity constraints using singleton types.
 */
package object api {
  type StringyMap = java.util.HashMap[String, AnyRef]
  type BigResult[T] = Either[String, T] // aggregating errors doesn't add much
}

package api {
  trait BigDataFormat[T] {
    def label: String
    def toProperties(t: T): StringyMap
    def fromProperties(m: StringyMap): BigResult[T]
  }

  trait SPrimitive[V] {
    def toValue(v: V): AnyRef
    def fromValue(v: AnyRef): V
  }

  // EXERCISE 1.2
  trait BigDataFormatId[T, V] {
    def key: String
    def value(t: T): V
  }
}

package object impl {
  import api._

  // EXERCISE 1.1 goes here
  implicit object HNilBigDataFormat extends BigDataFormat[HNil] {
    def label: String = "don't care"
    def toProperties(t: HNil): StringyMap = new StringyMap()
    def fromProperties(m: StringyMap): BigResult[HNil] = Right(HNil)
  }

  implicit def hListBigDataFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
    key: Witness.Aux[Key],
    headSPrimitive: SPrimitive[Value],
    lazyTailFormat: Lazy[BigDataFormat[Remaining]]
  ): BigDataFormat[FieldType[Key, Value] :: Remaining] = new BigDataFormat[FieldType[Key, Value] :: Remaining] {
    def label: String = "don't care"
    def toProperties(t: FieldType[Key, Value] :: Remaining): StringyMap = {
      val map = lazyTailFormat.value.toProperties(t.tail)
      val primitive = headSPrimitive.toValue(t.head)
      map.put(key.value.name, primitive)
      map
    }
    def fromProperties(map: StringyMap): BigResult[FieldType[Key, Value] :: Remaining] = {
      for {
        head <- Option(map.get(key.value.name)).map(headSPrimitive.fromValue).toRight(s"Missing field: ${key.value}").right
        tail <- lazyTailFormat.value.fromProperties(map).right
      } yield field[Key](head) :: tail
    }
  }

  implicit object StringSPrimitive extends SPrimitive[String] {
    def toValue(v: String): AnyRef = v
    def fromValue(v: AnyRef): String = v.asInstanceOf[String]
  }

  implicit object IntSPrimitive extends SPrimitive[Int] {
    def toValue(v: Int): AnyRef = Integer.valueOf(v)
    def fromValue(v: AnyRef): Int = v.asInstanceOf[Integer].toInt
  }

  implicit object BooleanSPrimitive extends SPrimitive[Boolean] {
    def toValue(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)
    def fromValue(v: AnyRef): Boolean = v.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  implicit object CNilBigDataFormat extends BigDataFormat[CNil] {
    def label: String = "don't care"
    def toProperties(t: CNil): StringyMap = throw new RuntimeException("Oops!")
    def fromProperties(m: StringyMap): BigResult[CNil] = Left("Shouldn't be here!")
  }

  implicit def coproductBigDataFormat[Name <: Symbol, Head, Tail <: Coproduct](
    implicit
    key: Witness.Aux[Name],
    lazyHeadFormat: Lazy[BigDataFormat[Head]],
    lazyTailFormat: Lazy[BigDataFormat[Tail]]
  ): BigDataFormat[FieldType[Name, Head] :+: Tail] = new BigDataFormat[FieldType[Name, Head] :+: Tail] {
    def label: String = key.value.name
    def toProperties(t: FieldType[Name, Head] :+: Tail): StringyMap = t match {
      case Inl(head) =>
        val map = lazyHeadFormat.value.toProperties(head)
        map.put("_typeHint", key.value.name)
        map
      case Inr(tail) =>
        lazyTailFormat.value.toProperties(tail)
    }
    def fromProperties(m: StringyMap): BigResult[FieldType[Name, Head] :+: Tail] = {
      if (m.get("_typeHint").asInstanceOf[String] == label) {
        lazyHeadFormat.value.fromProperties(m).right map { headResult =>
          Inl(field[Name](headResult))
        }
      } else {
        lazyTailFormat.value.fromProperties(m).right map { tailResult =>
          Inr(tailResult)
        }
      }
    }
  }

  implicit def familyBigDataFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    lazyBigDataFormat: Lazy[BigDataFormat[Repr]],
    tpe: Typeable[T]
  ): BigDataFormat[T] = new BigDataFormat[T] {
    def label: String = tpe.describe
    def fromProperties(m: StringyMap): BigResult[T] = lazyBigDataFormat.value.fromProperties(m).right.map(hlist => gen.from(hlist))
    def toProperties(t: T): StringyMap = lazyBigDataFormat.value.toProperties(gen.to(t))
  }
}

package impl {
  import api._

  // EXERCISE 1.2 goes here
}

package object syntax {
  import api._

  implicit class RichBigResult[R](val e: BigResult[R]) extends AnyVal {
    def getOrThrowError: R = e match {
      case Left(error) => throw new IllegalArgumentException(error.mkString(","))
      case Right(r) => r
    }
  }

  /** Syntactic helper for serialisables. */
  implicit class RichBigDataFormat[T](val t: T) extends AnyVal {
    def label(implicit s: BigDataFormat[T]): String = s.label
    def toProperties(implicit s: BigDataFormat[T]): StringyMap = s.toProperties(t)
    def idKey[P](implicit lens: Lens[T, P]): String = ???
    def idValue[P](implicit lens: Lens[T, P]): P = lens.get(t)
  }

  implicit class RichProperties(val props: StringyMap) extends AnyVal {
    def as[T](implicit s: BigDataFormat[T]): T = s.fromProperties(props).getOrThrowError
  }
}
