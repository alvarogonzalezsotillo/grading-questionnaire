package giftParser

/**
 * Created by alvaro on 16/06/15.
 */

import scala.util.parsing.combinator._

class GiftParser extends JavaTokenParsers {

  import giftParser.GiftParser._

  // ANY NUMBER OF BLANKS
  def indent: Parser[String] = """\s*""".r

  // ONLY ~ OR =
  def startOfAnswer: Parser[String] = "~" | "="

  // ANY CHARACTERS, NOT INCLUDING THE FIRST }, ~ OR =
  def answerBody: Parser[String] = """[^\}~=]*""".r

  // ANY CHARACTERS, NOT INCLUDING THE FIRST {
  def questionText: Parser[String] = """[^\{]*""".r


  // AN ANSWER CAN BE INDENTED, HAS A START AND A BODY
  def answer: Parser[Answer] = indent ~> startOfAnswer ~ answerBody ^^ {
    case "=" ~ b => Answer(b.trim, true)
    case "~" ~ b => Answer(b.trim, false)
  }

  // A QUESTION HAS A TEXT, AND MAYBE SOME ANSWERS
  def question: Parser[Question] = (questionText <~  "{" ) ~ ( rep(answer) <~ "}" )  ^^ {
    case t ~ List()  => OpenQuestion(t.trim)
    case t ~  a => QuestionnaireQuestion(t.trim, a)
  }

  // A QUESTIONNAIRE IS COMPOSED OF SEVERAL QUESTIONS
  def questionnaire: Parser[GiftFile] = rep(question)
}

object GiftParser{

  case class Answer(text: String, correct: Boolean)

  trait Question {
    val text: String
  }

  case class OpenQuestion(text: String) extends Question
  case class QuestionnaireQuestion(text: String, answers: List[Answer]) extends Question
  type GiftFile = List[Question]

  def parse( s: String ) = {
    val parser = new GiftParser
    parser.parseAll( parser.questionnaire, s )
  }
}
