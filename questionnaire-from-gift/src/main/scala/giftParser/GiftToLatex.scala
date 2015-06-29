package giftParser

import java.io.{InputStream, ByteArrayOutputStream, File}

import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._

import scala.util.Random

/**
 * Created by alvaro on 18/06/15.
 */
object GiftToLatex {


  private def generateQuestionLatex( q : Question ): String = q match {
    case OpenQuestion(text) =>
      s"\\begin{OpenQuestion}$text\\end{OpenQuestion}"

    case QuestionnaireQuestion(text, answers) =>
      val sa = answers.map {   a =>  s"\\item ${a.text}"  }.mkString("\n")

      val begin = s"\\begin{QuestionnaireQuestion}[$text]"
      val end = "\n\\end{QuestionnaireQuestion}"

      begin + sa + end
  }

  def generateLatexForQuestions( g: GiftFile ) = {
    g.questions.map( GiftToLatex.generateQuestionLatex ).mkString("\n")
  }

  def generateLatexSolutionForSolution( g: GiftFile ) = {
    val indexes = g.questions.filter( _.isInstanceOf[QuestionnaireQuestion] ).map{ case QuestionnaireQuestion(_,answers) =>
        answers.indexWhere( _.correct )
    }

    val options = indexes.map( i => (i.toChar + 'a').toChar )

    "\\begin{Solution}\n" + options.mkString("\t") + "\n\\end{Solution}"
  }

  def loadLatexTemplate  = {
    def streamToString( in: InputStream ) ={
      val bos = new ByteArrayOutputStream()
      val buffer = new Array[Byte](1024)
      var bytesRead = in.read(buffer)
      while( bytesRead > 0 ){
        bos.write(buffer,0,bytesRead )
        bytesRead = in.read(buffer)
      }
      bos.close()
      bos.toString
    }

    val in = getClass.getResourceAsStream("QuestionnaireGradingTest.tex")
    try{
      streamToString(in)
    }
    finally{
      in.close()
    }
  }



}
