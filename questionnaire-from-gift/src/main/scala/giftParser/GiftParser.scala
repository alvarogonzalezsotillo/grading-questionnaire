package giftParser

/**
 * Created by alvaro on 16/06/15.
 */

import java.io._

import com.typesafe.scalalogging.LazyLogging
import giftParser.GiftParser.{GiftError, GiftFile}
import giftToLatex.GiftToLatex

import scala.util.Random
import scala.util.parsing.combinator._

// SEE https://docs.moodle.org/23/en/GIFT_format
// NOT SUPPORTED: comments, missing word, matching, title, feedback, numerics
class GiftParser extends RegexParsers {

  import GiftParser._
  import GiftParser.GiftFile._

  def unescapeGiftText(s: String) = {
    val escaped = Map("\\{" -> "{", "\\}" -> "}", "=" -> "=", ":" -> ":", "~" -> "~")
    escaped.foldLeft(s)((ret, esc) => ret.replaceAll( """\\""" + esc._1, esc._2))
  }

  def lineComment: Parser[String] = """//.*""".r

  // ANY NUMBER OF BLANKS
  def indent: Parser[String] = """\s*""".r

  // ONLY ~ OR =
  def startOfAnswer: Parser[String] = "~" | "=" | failure("~ or = expected")

  // ANY CHARACTERS, NOT INCLUDING THE FIRST }, ~ OR =
  // Note: }, ~ and = can be escaped as \}, \~ and \=
  def answerBody: Parser[String] = """(\\=|\\\}|\\~|\\=|[^\}~=])*""".r | failure("}, = or ~ expected")

  // ANY CHARACTERS, NOT INCLUDING THE FIRST {
  def questionText: Parser[String] = """[^\{]*""".r | failure("{ expected")

  // AN ANSWER CAN BE INDENTED, HAS A START AND A BODY
  def answer: Parser[Answer] = rep(lineComment) ~> indent ~> startOfAnswer ~ answerBody ^^ {
    case "=" ~ b => Answer(unescapeGiftText(b.trim), true)
    case "~" ~ b => Answer(unescapeGiftText(b.trim), false)
    case _ => ???
  } | failure("An answer starts with = or ~")

  // A QUESTION HAS A TEXT, AND MAYBE SOME ANSWERS
  def question: Parser[Question] = rep(lineComment) ~> (questionText <~ "{") ~ (rep(answer) <~ "}") ^^ {
    case t ~ Nil => OpenQuestion(unescapeGiftText(t.trim))
    case t ~ a => QuestionnaireQuestion(unescapeGiftText(t.trim), a)
  } | failure("A question has some text, and maybe some answers surrounded by {}")

  // A QUESTIONNAIRE IS COMPOSED OF SEVERAL QUESTIONS
  def questionnaire: Parser[Questions] = rep(question)
}

object GiftParser extends LazyLogging{

  type Questions = List[GiftFile.Question]

  trait GiftParserResult {
    val successful = false
    def get : GiftFile = this match{
      case GiftError(msg, line, column, lineContents) =>
        throw new IllegalArgumentException(s"Error:$msg, at $line,$column\n$lineContents")

      case g: GiftFile =>
        g
    }
  }

  object GiftFile {

    case class Answer(text: String, correct: Boolean)

    trait Question {
      val text: String

      def shuffle = this
    }

    case class OpenQuestion(text: String) extends Question{
      private val fullPageHTMLTags = Seq( "table", "li" ).map( "</" + _ + ">")
      def fullPageQuestion = fullPageHTMLTags.exists( text.contains )
      logger.error("OpenQuestion:" + text )
    }

    case class QuestionnaireQuestion(text: String, answers: List[Answer]) extends Question {
      override def shuffle = QuestionnaireQuestion(text, Random.shuffle(answers))

      override def equals(o: Any) = o match {
        case QuestionnaireQuestion(t, a) =>
          t == text && answers.toSet == a.toSet

        case _ =>
          super.equals(o)
      }
      assert(answers.count(_.correct) == 1, s"Incorrect number of correct answers:$this")
      logger.error("QuestionnaireQuestion:" + text )
    }

    def apply(questions: Questions, file: Option[File]) = new GiftFile(questions, file)

    def unapply(gf: GiftFile): Option[Questions] = Some(gf.questions)
  }

  class GiftFile(val questions: Questions, val file: Option[File]) extends GiftParserResult {

    import GiftFile._

    override val successful = true

    val openQuestions = questions.filter(_.isInstanceOf[OpenQuestion])
    val questionnaireQuestions = questions.filter(_.isInstanceOf[QuestionnaireQuestion])

    def reorder(reorderAnswer: Boolean = true, reorderQuestions: Boolean = false) = {

      var oQuestions = openQuestions
      var qQuestions = questionnaireQuestions

      if (reorderQuestions) {
        oQuestions = Random.shuffle(oQuestions)
      }
      if (reorderQuestions) {
        qQuestions = Random.shuffle(qQuestions)
      }

      if (reorderAnswer) {
        qQuestions = qQuestions.map(_.shuffle)
      }

      GiftFile(qQuestions ++ oQuestions, file)
    }

    def reduce( questionnaireQuestionsNumber: Int = Int.MaxValue, openQuestionsNumber: Int = Int.MaxValue ) = {

      def reduceNumber( number: Int, q: Questions ): Questions ={
        if( q.size > number ){
          val i = Random.nextInt(q.size )
          reduceNumber( number, q.take(i-1) ++ q.drop(i) )
        }
        else{
          q
        }
      }

      val newQQ = reduceNumber(questionnaireQuestionsNumber, questionnaireQuestions)
      val newOQ = reduceNumber(openQuestionsNumber, openQuestions)
      GiftFile(newQQ ++ newOQ, file )
    }

  }

  case class GiftError(msg: String, line: Int, column: Int, lineContents: String) extends GiftParserResult

  private def processResult(parser: GiftParser, ret: GiftParser#ParseResult[Questions], file: Option[File]): GiftParserResult = ret match {
    case parser.Success(_, _) =>
      GiftFile(ret.get, file)
    case parser.Error(msg, next) =>
      logger.error( "parser.Error:" + msg + " " + next )
      GiftError(msg, next.pos.line, next.pos.column, next.pos.longString.takeWhile(_ != '\n'))
    case parser.Failure(msg, next) =>
      logger.error( "parser.Failure:" + msg + " " + next )
      GiftError(msg, next.pos.line, next.pos.column, next.pos.longString.takeWhile(_ != '\n'))
    case _ => throw new IllegalStateException()
  }

  def parse(s: String) = {
    val parser = new GiftParser
    val ret = parser.parseAll(parser.questionnaire, s)
    processResult(parser, ret, None)
  }

  def parse(reader: Reader) = {
    val parser = new GiftParser
    val ret = parser.parseAll(parser.questionnaire, reader)
    processResult(parser, ret, None)
  }

  def parse(f: File) = {
    val parser = new GiftParser
    val reader = if( f == new File(".") ) new InputStreamReader(System.in) else new FileReader(f)
    val ret = parser.parseAll(parser.questionnaire, reader)
    reader.close()
    processResult(parser, ret, Some(f))
  }

}
