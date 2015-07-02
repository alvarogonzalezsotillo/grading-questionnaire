package giftParser

/**
 * Created by alvaro on 16/06/15.
 */

import java.io.{FileReader, File, Reader}

import scala.util.{Random}
import scala.util.parsing.combinator._

// SEE https://docs.moodle.org/23/en/GIFT_format
// NOT SUPPORTED: missing word, matching, title, feedback, numerics
class GiftParser extends JavaTokenParsers {

  import GiftParser._
  import GiftParser.GiftFile._

  // ANY NUMBER OF BLANKS
  def indent: Parser[String] = """\s*""".r

  // ONLY ~ OR =
  def startOfAnswer: Parser[String] = "~" | "=" | failure( "~ or = expected")

  // ANY CHARACTERS, NOT INCLUDING THE FIRST }, ~ OR =
  def answerBody: Parser[String] = """[^\}~=]*""".r | failure( "}, = or ~ expected" )

  // ANY CHARACTERS, NOT INCLUDING THE FIRST {
  def questionText: Parser[String] = """[^\{]*""".r | failure ( "{ expected" )

  // AN ANSWER CAN BE INDENTED, HAS A START AND A BODY
  def answer: Parser[Answer] = indent ~> startOfAnswer ~ answerBody  ^^ {
    case "=" ~ b => Answer(b.trim, true)
    case "~" ~ b => Answer(b.trim, false)
  } | failure( "An answer starts with = or ~" )

  // A QUESTION HAS A TEXT, AND MAYBE SOME ANSWERS
  def question: Parser[Question] = (questionText <~  "{" ) ~ ( rep(answer) <~ "}" )  ^^ {
    case t ~ Nil  => OpenQuestion(t.trim)
    case t ~ a    => QuestionnaireQuestion(t.trim, a)
  } | failure( "A question has some text, and maybe some answers surrounded by {}");

  // A QUESTIONNAIRE IS COMPOSED OF SEVERAL QUESTIONS
  def questionnaire: Parser[Questions] = rep(question)
}

object GiftParser{


  type Questions = List[GiftFile.Question]

  trait GiftResult{
    def questions: Questions
    val successful = true
  }

  object GiftFile{
    case class Answer(text: String, correct: Boolean)

    trait Question {
      val text: String

      def shuffle = this
    }

    case class OpenQuestion(text: String) extends Question

    case class QuestionnaireQuestion(text: String, answers: List[Answer]) extends Question {
      override def shuffle = QuestionnaireQuestion(text, Random.shuffle(answers))
    }

    def apply( questions: Questions ) = new GiftFile(questions)
    def unapply( gf: GiftFile) : Option[Questions]= Some(gf.questions)
  }

  class GiftFile( val questions: Questions ) extends GiftResult{

    import GiftFile._

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

      GiftFile(qQuestions ++ oQuestions)
    }

  }
  case class GiftError( msg: String, line: Int, column: Int, lineContents: String ) extends GiftResult{
    override def questions = throw new NoSuchElementException()
    override val successful = false
  }

  private def processResult(parser: GiftParser, ret: GiftParser#ParseResult[Questions]): GiftResult = ret match {
    case parser.Success(_, _) => GiftFile(ret.get).reorder()
    case parser.Error(msg, next) => GiftError(msg, next.pos.line, next.pos.column, next.pos.longString.takeWhile(_ != '\n'))
    case parser.Failure(msg, next) => GiftError(msg, next.pos.line, next.pos.column, next.pos.longString.takeWhile(_ != '\n'))
  }

  def parse( s: String ) = {
    val parser = new GiftParser
    val ret = parser.parseAll(parser.questionnaire, s)
    processResult( parser, ret )
  }

  def parse( reader: Reader ) = {
    val parser = new GiftParser
    val ret = parser.parseAll(parser.questionnaire, reader)
    processResult( parser, ret )
  }

  def parse( f: File ) = {
    val parser = new GiftParser
    val reader = new FileReader(f)
    val ret = parser.parseAll(parser.questionnaire, reader)
    reader.close()
    processResult( parser, ret )
  }

}
