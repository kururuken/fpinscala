package fpinscala.parsing

import language.higherKinds
import java.util.regex._
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  // Here we use implicit conversion (also implicit parameters)
  // So for implicit conversion, if some where requries type B 
  // and we give it type A, the compiler will automatically look for 
  // implicit conversion
  // implicit def f(A): B
  // Similar for implicit parameters
  //
  // Here is an example
  // string will be automatically convert to Parser[String] and ParserOps[String]
  // if needed
  // so "a" | "b" will be convert to parser through the following path
  // String => Parser[String] => ParserOps[String] => ParserOps.| => Parser[String]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  
  // ParserOps contains the operation we can perform on parsers
  // since we have implicit conversion for String and Parser[String] 
  // to be converted to ParserOps, it is very convenient to perform 
  // these ops on parser and even string
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) 
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def attempt: Parser[A] = self.attempt(p)
    def scope(msg: String) = self.scope(msg)(p)
    def label(msg: String) = self.label(msg)(p)
  }

  // slice will return the portion of the string found
  // many will simply return a Parser[List[A]]
  // so when we run the parse we get a list
  // slice is used to convert the list to string
  // even if the parser p.many.map(_.size) will generate an intermediate list when run, slice(p.many).map(_.size) will not.
  def slice[A](p: Parser[A]): Parser[String]

  // non strict
  // in our implementation we want to evaluate the first argument first,
  // if it fails we will not evaluate the second argument

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // the parser that recognizes zero or more repetitions
  // simple parser like string("abc") will return "abc" only once if found
  // many will return 0 or more times
  // latter part of or means 0 times
  //
  // This original version will cause stack overflow
  // To see this problem, consider the scenerio in the test
  //    val p1 = attempt(string("a")).many.slice
  //    val p2 = string("a").many.slice
  //    MyParsers.run(p)("aaac") should be (Right("aaa"))
  // p2 will simply fail because p2 is not in attempt mode so 
  // when it encounters "c" it just commits the failure and 
  // there is no room for or to run the second parser
  // p1 will cause a stack overflow problem
  // when p fails, map2 will still tries to run many(p)
  // and since the first p failure never gets commited
  // it will become an infinite recursion
  // to solve this we introduce an override version of many
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(f andThen succeed) // equals x => succeed(f(x)), f andThen g = g(f(_)), f compose g = f(g(_))

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for (a <- p; b <- p2) yield (a, b)

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for (a <- p; b <- p2) yield f(a, b)
  // p.product(p2).map(f.tupled) // tupled wraps its argument to a tuple

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // This implementation is not OK
  // map and succeed will call each other
  // so one of them must be primitive
  def succeed[A](a: A): Parser[A] // = string("") map (_ => a)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)

  implicit def regex(r: Regex): Parser[String]

  // used to tag certain parser
  // if p fails, its ParseError will somehow incorporate msg.
  // instead of pushing onto the error stack, it replaces what’s already there
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // higher level message attached to p
  // Unlike label, scope doesn’t throw away the label(s) attached to p—it merely adds additional information in the event that p fails.
  // will push one level to the stack
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // all parsers commit by default if they examine at least one character to produce a result. 12 We then introduce a combinator, attempt, which delays committing to a parse
  def attempt[A](p: Parser[A]): Parser[A]

  object Laws {
  }
}

// Location and Parse Error are sort of detailed implementation of error handling
// should we just make some trait here and do the implementation elsewhere?
case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError = 
    copy(stack = (loc,msg) :: stack)
  
  def latestLoc: Option[Location] = stack.lastOption.map(_._1)

  def label[A](s: String): ParseError = 
    ParseError(latestLoc.map((_,s)).toList)
}