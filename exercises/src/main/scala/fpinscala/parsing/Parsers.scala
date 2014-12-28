 package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A] (p: Parser[A])(input: String): Either[ParseError, A]


  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  implicit def string(s: String): Parser[String]

  // PRIMATIVES
  def char(c: Char): Parser[Char] =
    map(string(c.toString))(_.charAt(0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = 
    map(string(""))(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    flatMap(p)(x => map(p2)(a => (x, a)) )
  }

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
    flatMap(a)(a => succeed(f(a)))
    // flatMap(a)(f andThen succeed)
  }
  // END PRIMATIVES


  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List()) )


  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(x => map(p2)(a => f(x, a)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

//   def context[A](input: Parser[A]): Int = {
//     // val reg = "[0-9]+".r
//     // val n = regex(reg).toInt
//     // flatMap(n, listOfN(n, char("a")))
//     for {
//   digit <- "[0-9]+".r
//   val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
//   _ <- listOfN(n, char('a'))
// } yield n
//   }
    

  case class ParserOps[A](p: Parser[A]) {


  }

  object Laws {
    // def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = 
    //   equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}