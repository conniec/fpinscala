package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A=>A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    val zero = (a: A) => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)( (b, a) => m.op(b, f(a)) )
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")


  // def ordered(ints: IndexedSeq[Int]): Boolean = {
  //   // Our monoid tracks the minimum and maximum element seen so far
  //   // as well as whether the elements are so far ordered.
  //   val mon = new Monoid[Option[(Int, Int, Boolean)]] {
  //     def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
  //       (o1, o2) match {
  //         // The ranges should not overlap if the sequence is ordered.
  //         case (Some((x1, y1, p)), Some((x2, y2, q))) =>
  //           Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
  //         case (x, None) => x
  //         case (None, x) => x
  //       }
  //     val zero = None
  //   }

  //   foldMapV(ints, mon)(i => Option((i, i, true))).map(_._3).getOrElse(true)

  // }


  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val split = as.splitAt(as.length / 2)
      val a1 = foldMapV(split._1, m)(f)
      val b1 = foldMapV(split._2, m)(f)
      m.op(a1, b1)
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(w1: WC, w2: WC) = {
      (w1, w2) match {
        case (x: Stub, y: Stub) => Stub(x.chars + y.chars)
        case (x: Stub, y: Part) => Part(x + y.lStub, y.words, y.rStub)
        case (x: Part, y: Stub) => Part(x.lStub, x.words, x.rStub + y)
        case (x: Part, y: Part) => {
          val newWords = if (x.rStub.isEmpty && y.lStub.isEmpty) y.words + x.words
            else y.words + x.words + 1
          Part(x.lStub, newWords, y.rStub)
        }
        case _ => Stub("")
      }
    }
    val zero = Stub("")
  }

  def count(s: String): Int = {

    // def helper(s: String, monoid: Monoid[WC]): Monoid[WC] = {
    //   if (s.split("\\s+").size == 1) Stub(s)
    //   else {
    //     val split = s.splitAt(s.length / 2)

    //   }
    // }
    1
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = 
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)) = (A.op(x._1, y._1), B.op(x._2, y._2))
      val zero = (A.zero, B.zero)
    }
    
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero = a => B.zero
      def op(x: A => B, y: A => B) = a => B.op(x(a), y(a))
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K,V]] {
      def zero = Map[K,V]()
      def op(a: Map[K,V], b: Map[K,V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    as.foldLeft(Map[A, Int]())( (b, a) => (
      m.op(Map[A, Int](a -> 1), b)
    ))
  }

}


trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z) // can't be in terms of foldLeft

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = sys.error("todo")
    //foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => f(a))
    // foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List[A]())((b, a) => a :: b)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(x) => f(x)
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case None => z
      case Some(x) => f(z, x)
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case None => z
      case Some(x) => f(x, z)
    }
}

