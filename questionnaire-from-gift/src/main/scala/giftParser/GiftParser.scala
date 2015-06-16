package giftParser

/**
 * Created by alvaro on 16/06/15.
 */

import scala.util.parsing.combinator._

class GiftParser extends JavaTokenParsers {

  import giftParser.GiftParser._


  def indent: Parser[String] = """\s*""".r

  def startOfAnswer: Parser[String] = "~" | "="

  def answerBody: Parser[String] = """(?m)[^\}~=]*""".r

  def answer: Parser[Answer] = indent ~> startOfAnswer ~ answerBody ^^ {
    case "=" ~ b => Answer(b.trim, true)
    case "~" ~ b => Answer(b.trim, false)
  }

  def questionText: Parser[String] = """(?m)[^\{]*""".r

  def question: Parser[Question] = questionText ~ "{" ~ rep(answer) ~ "}" ^^ {
    case t ~ "{" ~ List() ~ "}" => OpenQuestion(t.trim)
    case t ~ "{" ~ a ~ "}" => QuestionnaireQuestion(t.trim, a)
  }

  def questionnaire: Parser[GiftFile] = rep(question)

}

object GiftParser{

  case class Answer(text: String, correct: Boolean)

  trait Question {
    val text: String
    val answers = List()
  }

  case class OpenQuestion(text: String) extends Question
  case class QuestionnaireQuestion(text: String, override val answers: List[Answer]) extends Question
  type GiftFile = List[Question]
}
