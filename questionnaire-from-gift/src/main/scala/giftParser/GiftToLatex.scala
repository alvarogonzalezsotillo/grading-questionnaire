package giftParser

import java.io.{InputStream, ByteArrayOutputStream, File}

import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._

import scala.util.Random

/**
 * Created by alvaro on 18/06/15.
 */
object GiftToLatex {


  private object charsToLatex extends ((Char) => String){

    private val charsToLatexMap = Map(
      '\n' -> """\\""",
      '_' ->  """\_""",
      '#' -> """\#""",
      '%' -> """\%""",
      '&' -> """\&""",
      '$' -> """\$""",
      '\\' -> """\textbackslash""",
      '{' -> """\{""",
      '}' -> """\}"""
    ).withDefault( _.toString )

    var state = "``"

    private def toggle = state = state match{
      case "``" => "''"
      case "''" => "``"
    }

    override def apply( c: Char): String = c match{
      case '"' =>
        val ret = state
        toggle
        ret

      case c => charsToLatexMap(c)

    }
  }

  private def escapeLatex( s: String )  =  s.map( charsToLatex ).mkString

  private def generateQuestionLatex( q : Question ): String = q match {
    case OpenQuestion(text) =>
      s"\\begin{OpenQuestion}\n  ${escapeLatex(text)}\n\\end{OpenQuestion}"

    case QuestionnaireQuestion(text, answers) =>
      val sa = answers.map {   a =>  s"    \\item ${escapeLatex(a.text)}"  }.mkString("\n")

      val begin = s"  \\begin{QuestionnaireQuestion}[${escapeLatex(text)}]\n"
      val end = "\n  \\end{QuestionnaireQuestion}"

      begin + sa + end
  }

  private def generateLatexForQuestions( g: GiftFile ) = {
    val qq = g.questionnaireQuestions.map( GiftToLatex.generateQuestionLatex ).mkString("\n")
    val oq = g.openQuestions.map( GiftToLatex.generateQuestionLatex ).mkString("\n")

    s"\\begin{QuestionnaireQuestions}\n$qq\n\\end{QuestionnaireQuestions}\n$oq"
  }

  private def generateLatexSolutionForSolution( g: GiftFile ) = {
    val indexes = g.questions.filter( _.isInstanceOf[QuestionnaireQuestion] ).map{ case QuestionnaireQuestion(_,answers) =>
        answers.indexWhere( _.correct )
    }

    val options = indexes.map( i => (i.toChar + 'a').toChar )

    "\\Solution{" + options.mkString(",") + "}"
  }

  private lazy val latexTemplate  = {
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

    val in = getClass.getResourceAsStream("/QuestionnaireGradingTest.template.tex")
    try{
      streamToString(in)
    }
    finally{
      in.close()
    }
  }

  def generateLatex(f: GiftFile, questionnaireQuestionsWeight : Int = 60, openQuestionsWeight: Int = 40 ): String ={
    val instructions = s"\\Instructions{$questionnaireQuestionsWeight}{$openQuestionsWeight}"
    val answerTable = s"\\AnswerTable{${f.questionnaireQuestions.size}}"
    val questions = generateLatexForQuestions(f)
    val solutions = generateLatexSolutionForSolution(f)
    val generatedContent = List(instructions,answerTable,questions,solutions).mkString("\n")


    latexTemplate.replace("${GeneratedContent}",generatedContent)
  }

  def apply( f: GiftFile, questionnaireQuestionsWeight : Int = 60, openQuestionsWeight: Int = 40 ) = {
    generateLatex(f,questionnaireQuestionsWeight,openQuestionsWeight)
  }

}