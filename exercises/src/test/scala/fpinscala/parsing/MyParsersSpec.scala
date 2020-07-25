package fpinscala.parsing

import language.higherKinds
import org.scalatest.{run => _, _}
import MyParsers._

class MyParsersSpec extends FlatSpec with Matchers {
    "Parser" should "parse simple string" in {
        // val p1 = string("abc")
        // val p2 = string("abd")

        MyParsers.run("abc")("abcd") should be (Right("abc"))
        MyParsers.run("abd")("abcd") shouldBe a [Left[_, _]]
    }

    it should "parse many strings" in {
        // val p1 = attempt(string("a")).many.slice
        // val p2 = string("a").many.slice

        MyParsers.run("a".attempt.many.slice)("aaac") should be (Right("aaa"))
        MyParsers.run("a".many.slice)("aaac") shouldBe a [Left[_, _]]
    }

    it should "concat 2 parsers" in {
        MyParsers.run("abc" ** "def" slice)("abcdef") should be (Right("abcdef"))
    }

    it should "commit or uncommit if first parse fails" in {
        MyParsers.run("abc" | "def")("def") shouldBe a [Left[_, _]]
        MyParsers.run("abc".attempt | "def")("def") should be (Right("def"))
    }
    
    it should "marks label and scope" in {

    }
}