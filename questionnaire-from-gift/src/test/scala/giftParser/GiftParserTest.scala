package giftParser

import java.io.File

import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner



/**
 * Created by alvaro on 21/09/2014.
 */
@RunWith(classOf[JUnitRunner])
class GiftParserTest extends FlatSpec {

  "Empty file" should "have no questions" in {
    val s = ""
    val ret = GiftParser.parse(s)
    assert( ret.successful )
    val giftFile = ret.questions
    assert( giftFile.size == 0 )
  }

  val singleOpenQuestion =
    """hola
      |{
      |
      |}""".stripMargin

  "Single open question" should "parse" in {
    val ret = GiftParser.parse(singleOpenQuestion)
    assert( ret.successful )
    val giftFile = ret.questions
    assert( giftFile(0).isInstanceOf[OpenQuestion] )
  }

  val singleOpenQuestionNoBlanks = """hola{ }"""

  "Single open question with no extra new lines" should "parse" in {
    val ret = GiftParser.parse(singleOpenQuestionNoBlanks)
    assert( ret.successful )
    val giftFile = ret.questions
    assert( giftFile(0).isInstanceOf[OpenQuestion] )
  }

  val singleClosedQuestion =
    """hola como estás yo bien gracias{
      |=bien
      |~mal
      |}""".stripMargin

  "Single closed question" should "parse" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    assert( ret.successful )
    val giftFile = ret.questions
    assert( giftFile(0).isInstanceOf[QuestionnaireQuestion] )
  }

  "Single closed question" should "parse with correct answers" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    assert( ret.successful )
    val giftFile = ret.questions
    assert( giftFile(0) == new QuestionnaireQuestion( "hola como estás yo bien gracias", List( new Answer("bien", true), new Answer("mal", false)) ) )
  }

  val singleClosedQuestionNoBlanks =
    """hola como estás yo bien gracias{ =bien ~mal }"""

  "Single closed question with no extra blanks" should "parse with correct answers" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    val retnoblanks = GiftParser.parse(singleClosedQuestionNoBlanks)
    assert( ret.questions == retnoblanks.questions )
  }

  "A malformed file" should "not parse" in {
    val error =" this is an error "
    val ret = GiftParser.parse(error)
    assert( !ret.successful )
  }

  "Some questions" should "parse" in {
    val questions = List(singleClosedQuestion, singleClosedQuestionNoBlanks, singleOpenQuestion, singleOpenQuestionNoBlanks )
    val s = questions.mkString("\n")

    val ret = GiftParser.parse(s)
    val giftFile = ret.questions
    assert( giftFile.size == 4 )
  }

  "A malformed question" should "not parse" in {
    val s = "Esto está mal{ {= }"
    val ret = GiftParser.parse(s)
    assert(!ret.successful)

    ret match {
      case GiftError(msg,line,column,lineContents) =>
        assert( line == 1 )
        assert(lineContents == s)
    }
  }

  "A big file" should "parse" in {
    val file = new File("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/seguridad-informatica/examenes/SI-Extraordinaria-Junio.gift")
    val ret = GiftParser.parse(file)

    ret match {
      case GiftError(msg,line,column,lineContents) =>

      case g : GiftFile =>
        println( GiftToLatex.generateLatexForQuestions(g) )
        println( GiftToLatex.generateLatexSolutionForSolution(g) )
    }


  }

}
