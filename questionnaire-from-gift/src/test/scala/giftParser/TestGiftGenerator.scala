package giftParser

import java.io.File

import giftParser.GiftParser.GiftFile
import giftParser.GiftParser.GiftFile.{Answer, QuestionnaireQuestion, OpenQuestion}

import scala.io.Source
import scala.util.Random

/**
 * Generates random GIFT files
 */
object TestGiftGenerator {

  import giftParser.Util._

  private val randomLines = withResource(getClass.getResourceAsStream("lorem.txt")) { in =>
    Source.fromInputStream(in).getLines().filter(_.trim != "").toList
  }


  private def shuffle = Random.shuffle(randomLines)

  private def randomLine = shuffle.head


  def generateGift(questionnaireQuestions: Int, openQuestions: Int) = {
    import GiftParser.GiftFile._

    def randomAnswers(n: Int = 4) = {
      shuffle.take(n - 1).map(Answer(_, false)) ++ shuffle.take(1).map(Answer(_, true))
    }

    val qq = (1 to questionnaireQuestions).map(_ => QuestionnaireQuestion(randomLine, randomAnswers())).toList
    val oq = (1 to openQuestions).map(_ => OpenQuestion(randomLine)).toList

    GiftFile(qq ++ oq)
  }

  def renderGift(g: GiftFile) = {

    def answerToString(a: Answer) = {
      val solution = if (a.correct) "=" else "~"
      s"  $solution${a.text}\n"
    }

    val ret = g.questions.map(_ match {
      case OpenQuestion(text) =>
        s"$text\n{\n}\n"
      case QuestionnaireQuestion(text, answers) =>
        s"$text\n{\n${answers.map(answerToString).mkString}}\n"
    })

    ret.mkString("\n")
  }
}
