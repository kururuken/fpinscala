package fpinscala.parsing

import language.higherKinds

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
  //   import P._
  //   val spaces = char(' ').many.slice

  // }

  // I cannot do it now
  // too hard for me
}