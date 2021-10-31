package giftParser

import java.io.File

import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._
import giftParser.TestGiftGenerator._
import giftParser.Util._
import org.junit.runner.RunWith
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatestplus.junit.JUnitRunner

import scala.util.Try


/**
 * Created by alvaro on 21/09/2014.
 */
@RunWith(classOf[JUnitRunner])
class GiftParserTest extends FlatSpec {


  "Empty file" should "have no questions" in {
    val s = ""
    val ret = GiftParser.parse(s)
    assert(ret.successful)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile.size == 0)
  }

  val singleOpenQuestion =
    """hola
      |{
      |
      |}""".stripMargin

  "Single open question" should "parse" in {
    val ret = GiftParser.parse(singleOpenQuestion)
    assert(ret.successful)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile(0).isInstanceOf[OpenQuestion])
  }

  val singleOpenQuestionNoBlanks = """hola{ }"""

  "Single open question with no extra new lines" should "parse" in {
    val ret = GiftParser.parse(singleOpenQuestionNoBlanks)
    assert(ret.successful)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile(0).isInstanceOf[OpenQuestion])
  }

  "A question with more than one correct answer" should "not parse" in {

    val s = "Incorrect{ =good =also good ~bad }"

    val t = Try(GiftParser.parse(s))
    assert(t.isFailure)
  }

  val singleClosedQuestion =
    """hola como estás yo bien gracias{
      |=bien
      |~mal
      |}""".stripMargin

  "Single closed question" should "parse" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    assert(ret.successful)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile(0).isInstanceOf[QuestionnaireQuestion])
  }

  "Single closed question" should "parse with correct answers" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    assert(ret.successful)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile(0) == new QuestionnaireQuestion("hola como estás yo bien gracias", List(new Answer("bien", true), new Answer("mal", false))))
  }

  val singleClosedQuestionNoBlanks =
    """hola como estás yo bien gracias{ =bien ~mal }"""

  "Single closed question with no extra blanks" should "parse with correct answers" in {
    val ret = GiftParser.parse(singleClosedQuestion)
    val retnoblanks = GiftParser.parse(singleClosedQuestionNoBlanks)
    assert(ret.asInstanceOf[GiftFile].questions == retnoblanks.asInstanceOf[GiftFile].questions)
  }

  "A malformed file" should "not parse" in {
    val error = " this is an error "
    val ret = GiftParser.parse(error)
    assert(!ret.successful)
  }

  "Some questions" should "parse" in {
    val questions = List(singleClosedQuestion, singleClosedQuestionNoBlanks, singleOpenQuestion, singleOpenQuestionNoBlanks)
    val s = questions.mkString("\n")

    val ret = GiftParser.parse(s)
    val giftFile = ret.asInstanceOf[GiftFile].questions
    assert(giftFile.size == 4)
  }

  "A malformed question" should "not parse" in {
    val s = "Esto está mal{ {= }"
    val ret = GiftParser.parse(s)
    assert(!ret.successful)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        assert(line == 1)
        assert(lineContents == s)
    }
  }

  def testFile(name: String) = new File(new File("./build/gift-parser/"), name)

  "A generated file" should "parse" in {

    val s = TestGiftGenerator.generateGift(40, 4)
    val f = createFile(renderGift(s), testFile("generated.gift"))

    val ret = GiftParser.parse(f)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        assert(g.openQuestions.size == 4)
        assert(g.questionnaireQuestions.size == 40)

    }
  }


  "A multiline question" should "parse" in {
    val s = """Una pregunta
               multilínea
               que debería funcionar{
            }"""

    val ret = GiftParser.parse(s)
    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg + "-" + line + "-" + column + "-" + lineContents)
      case GiftFile(questions) =>
        assert(questions.size == 1)
        assert(questions(0).text contains "Una pregunta")
        assert(questions(0).text contains "multilínea")
        assert(questions(0).text contains "que debería funcionar")
    }
  }

  "= and ~" should "parse if escaped" in {
    val s =
      """Una pregunta{
        |  = La respuesta \=
        |  ~ La respuesta \~
        |  }
      """.stripMargin

    val ret = GiftParser.parse(s)
    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg + "-" + line + "-" + column + "-" + lineContents)
      case GiftFile(questions) =>
        assert(questions.size == 1)
        val question = questions(0).asInstanceOf[QuestionnaireQuestion]
        assert(question.answers.size == 2)
    }

  }

  "backslash alone (\\)" should "be parsed" in {
    val s =
      """Una pregunta{
        |  =La respuesta correcta \
        |  ~La que no \ es
        |}
      """.stripMargin

    val ret = GiftParser.parse(s)
    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg + "-" + line + "-" + column + "-" + lineContents)
      case GiftFile(questions) =>
        assert(questions.size == 1)
        val question = questions(0).asInstanceOf[QuestionnaireQuestion]
        assert(question.answers.size == 2)
    }
  }

  "A question with table" should "be a full page question" in {
    val s = "Diseña la siguiente red para que los ordenadores blancos tengan un dominio de broadcast distinto de los ordenadores oscuros, pero que todos puedan comunicarse a nivel de red.\n<img width=\"8cm\" src=\"ejercicio-vlan-1.png\">\n<font size=\"+1\">\n<table columns=\"6\">\n<tr>\n\t<td>Equipo</td>\t<td>Interfaz</td>\t<td>Vlan (access/trunk)</td>\t<td>Puerta de enlace</td>\t<td>Dirección IP y Máscara</td>\n</tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n<tr><td></td><td></td><td></td><td></td><td></td></tr>\n</table>\n</font>\n{\n}"
    GiftParser.parse(s) match{
      case GiftFile(questions) =>
        assert( questions(0).asInstanceOf[OpenQuestion].fullPageQuestion )
    }
  }



}
