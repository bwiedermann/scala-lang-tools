package langtools

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait LangParseMatchers[IR] extends Matchers {
  
  val parser: String ⇒ Parsers#ParseResult[IR]
    
  class ParsedProgram(source: String) {
    lazy val parseResult = parser(source)
    lazy val successful = parseResult.successful
  }
  
  def program(source: String) = new ParsedProgram(source)
  def file(filename: String) = 
    new ParsedProgram(io.Source.fromFile(filename).mkString)
  
  val parse = new Matcher[ParsedProgram] {
    def apply(left: ParsedProgram) = 
      MatchResult( left.successful,
        "the program did not parse",
        "the program parsed")
  }
  
  def parseAs(prog: IR) = new Matcher[ParsedProgram] {
    def apply(left: ParsedProgram) = 
      MatchResult( left.parseResult.get == prog,
        "should NOT parse as " + left.parseResult.get,
        "parsed correctly")
  }
}

trait LangInterpretMatchers[IR, SD] extends LangParseMatchers[IR] { 
  
  val interpreter: langtools.Interpreter[IR, SD]
  
  class InterpretedProgram(source: String) extends ParsedProgram(source) {
    lazy val result: SD = interpreter.eval(parseResult.get) 
  }
  
  override def program(source: String) = new InterpretedProgram(source)
  override def file(filename: String) = 
    new InterpretedProgram(io.Source.fromFile(filename).mkString)
  
  def compute(r: SD) = new Matcher[InterpretedProgram] {
    def apply(left: InterpretedProgram) = 
      MatchResult( left.result == r,
        "should NOT evaluate to " + left.result,
        "evaluated correctly")
  }
  
  def raiseError[ET](implicit manifest: Manifest[ET]) =  
    new Matcher[InterpretedProgram] {
      def apply(left: InterpretedProgram) = {
        // we're just wrapping this matcher:
        an [ET] should be thrownBy{left.result}
        MatchResult(true,
            "no one should ever see this",
            "raised the expected error") 
      }
    }
}

trait StoreMatchers[SD,XD,VD] extends Matchers {
  this: LangInterpretMatchers[_,SD] ⇒
 
  def lookup(x: XD, r: SD): Option[VD]

  def give(bindings: (XD, VD)*) = new Matcher[InterpretedProgram] {
    // This isn't the right way to do it. All the tests should be composed
    // into one MatchResult
    def apply(left: InterpretedProgram) = {
      val result: SD = left.result
      bindings foreach { binding ⇒
        val (x, value) = binding
        val foundValue = lookup(x, result)
        assert( foundValue.isDefined )
        assert( foundValue.get === value ) 
      }
      MatchResult( true,
        "didn't check out",  
        "checked out")
    }
  }

}
