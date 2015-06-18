package giftParser

import java.io.File

import giftParser.GiftParser._

import scala.util.Random

/**
 * Created by alvaro on 18/06/15.
 */
object GiftQuestionnaire {

  def parseFile(f: File, reorderQuestions: Boolean = false, reorderAnswer: Boolean = false) = {
    reorder( GiftParser.parse(f), reorderQuestions, reorderAnswer )
  }

  def parseString(s: String, reorderQuestions: Boolean = false, reorderAnswer: Boolean = false) = {
    reorder( GiftParser.parse(s), reorderQuestions, reorderAnswer )
  }


  private def reorder(giftResult: GiftResult, reorderQuestions: Boolean, reorderAnswer: Boolean) = giftResult match {

    case error: GiftError => error

    case GiftFile(questions) =>
      var oQuestions = questions.filter(_.isInstanceOf[OpenQuestion])
      var qQuestions = questions.filter(_.isInstanceOf[QuestionnaireQuestion])

      if (reorderQuestions) {
        oQuestions = Random.shuffle(oQuestions)
      }
      if (reorderQuestions) {
        qQuestions = Random.shuffle(qQuestions)
      }

      if (reorderAnswer) {
        qQuestions = qQuestions.map(_.shuffle)
      }

      GiftFile( oQuestions ++ qQuestions )
  }

  private def generateQuestionLatex( q : Question ): String = q match {
    case OpenQuestion(text) =>
      s"""\\begin{OpenQuestion}
         |  $text
         |\\end{OpenQuestion}""".stripMargin

    case QuestionnaireQuestion(text, answers) =>
      val sa = answers.map {   a =>
        s"""  \\begin{Answer}
           |    ${a.text}
           |  \\end{Answer}
           |""".stripMargin
      }.mkString("\n")

      val begin = s"""\\begin{QuestionnaireQuestion}
         |  $text
         |""".stripMargin
      val end = "\\end{QuestionnaireQuestion}"

      begin + sa + end
  }

  def generateLatex( g: GiftFile ) = {
    g.questions.map( GiftQuestionnaire.generateQuestionLatex ).mkString("\n")
  }

  def generateSolution( g: GiftFile ) = {
    val indexes = g.questions.filter( _.isInstanceOf[QuestionnaireQuestion] ).map{ case QuestionnaireQuestion(_,answers) =>
        answers.indexWhere( _.correct )
    }

    val options = indexes.map( i => (i.toChar + 'a').toChar )

    "\\begin{Solution}\n" + options.mkString("\t") + "\n\\end{Solution}"
  }



}
