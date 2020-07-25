package fpinscala.parsing

import language.higherKinds
import MyParser._
import scala.util.matching.Regex

object MyParser {
    type Parser[+A] = Location => Result[A]

    trait Result[+A] {
        def mapError(f: ParseError => ParseError): Result[A] = 
            this match { 
                case Failure(e, isCommitted) => Failure(f(e), isCommitted) 
                case _ => this 
            }
        
        def uncommit: Result[A] = this match { 
            case Failure(e,true) => Failure(e,false) 
            case _ => this 
        }

        def addCommit(isCommitted: Boolean): Result[A] = this match { 
            case Failure(e,c) => Failure(e, c || isCommitted) 
            case _ => this 
        }

        def advanceSuccess(n: Int): Result[A] = this match { 
            case Success(a,m) => Success(a,n+m) 
            case _ => this 
        }
    }
    case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
    case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

object MyParsers extends Parsers[Parser] {
    def run[A](p: Parser[A])(input: String): Either[ParseError,A] = {
        val l = Location(input)
        p(l) match {
            case Failure(get, _) => Left(get)
            case Success(get, charsConsumed) => Right(get)
        }
    }

    // note when facing parsing errors we should return the first character 
    // that does not match
    // we do this only for the sake of simplicity
    // note we can delete implicit here, and the method is no longer implicit
    implicit def string(s: String): Parser[String] = l => {
        if (l.input.indexOf(s, l.offset) == l.offset)
            Success(s, s.length)
        else
            Failure(l.toError("string"), true)
    }

    implicit def regex(r: Regex): Parser[String] = l => {
        val s = r.findPrefixOf(l.input.drop(l.offset))
        s match {
            case None => Failure(l.toError("regex"), true)
            case Some(value) => Success(value, value.length)
        }
    }

    def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    def slice[A](p: Parser[A]): Parser[String] = l => {
        p(l) match {
            case f @ Failure(_, _) => f
            case Success(_, n) => Success(l.input.substring(l.offset, l.offset + n), n)
        }
    }

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = 
        s => p(s).mapError(_.push(s, msg))
    
    def label[A](msg: String)(p: Parser[A]): Parser[A] = 
        s => p(s).mapError(_.label(msg))
    
    def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

    // currently isCommitted is solely used by or
    def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match { 
        case Failure(e,false) => y(s) 
        case r => r 
    }

    // ultimately we will use flatMap to do concat on different parsers
    // so only flatMap would advance 
    def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = l => f(l) match {
        case Success(get, n) => g(get)(l.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case e @ Failure(_, _) => e
    }

    override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }
}