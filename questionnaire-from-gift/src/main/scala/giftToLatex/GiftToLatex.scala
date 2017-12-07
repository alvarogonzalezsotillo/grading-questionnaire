package giftToLatex

import java.io.{ByteArrayOutputStream, File, InputStream, OutputStream}

import com.typesafe.scalalogging.slf4j.LazyLogging
import common.{BinaryConverter, QuestionnaireVersion}
import giftParser.GiftParser
import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._
import giftParser.Util._
import giftToLatex.Main.Config

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alvaro on 18/06/15.
 */
object GiftToLatex extends LazyLogging{


  object GiftHTMLParser extends RegexParsers {

    trait Chunk{
      val text: String
      val toLatex : String
    }

    case class TextChunk( text: String ) extends Chunk{
      lazy val toLatex = charToLatex.escapeLatex(text)
    }
    case class HTMLChunk( text: String ) extends Chunk{
      lazy val toLatex = htmlToLatex(text)
    }

    def textChunk : Parser[TextChunk] = "[^<]+".r ^^ {
      case s => TextChunk(s)
    }

    def htmlChunk : Parser[HTMLChunk] = "<(.*?)>".r ^^ {
      case s => HTMLChunk(s)
    }

    def chunk : Parser[Chunk] = htmlChunk | textChunk

    // THE TEXT HAS SOME TEXTS IN PLAIN UTF8 AND SOME HTML TEXT
    def chunks: Parser[List[Chunk]] = rep(chunk)
  }


  private object charToLatex extends ((Char) => String) {

    private val charsToLatexMap = Map(
      '\n' -> """\\""",
      '_' -> """\_""",
      '#' -> """\#""",
      '%' -> """\%""",
      '&' -> """\&""",
      '$' -> """\$""",
      '\\' -> """{\textbackslash}""",
      '{' -> """\{""",
      '}' -> """\}""",
      '^' -> """\string^"""
    ).withDefault(_.toString)

    var state = "``"

    private def toggle = state = state match {
      case "``" => "''"
      case "''" => "``"
    }

    override def apply(c: Char): String = c match {
      case '"' =>
        val ret = state
        toggle
        ret

      case c => charsToLatexMap(c)
    }

    def escapeLatex(s: String) = s.map(charToLatex).mkString
  }

  object htmlToLatex {
    private val tagsRexs = Map(
      // IMAGES
      """(?i)<img src="(.*?)">""" -> """\\\\ \\begin{center}\\includegraphics{$1}\\end{center}""",
      """(?i)<img src=(.*?)>""" -> """\\\\ \\begin{center}\\includegraphics{$1}\\end{center}""",
      """(?i)<img width="(.*?)" src="(.*?)">""" -> """\\\\ \\begin{center}\\includegraphics[width=$1]{$2}\\end{center}""",

      // LISTS
      "<ul>" -> """\\begin{itemize} """,
      "</ul>" -> """\\end{itemize} """,
      "<li>" -> """\\item """,
      "</li>" -> "",

      // TABLES
      """<table columns="(.*?)">""" ->  """\\begin{center} \\begin{tabular}{| *{$1}{c|} }
        """,
      "<tr>" ->  "\\\\hline \n",
      "</tr>" ->  "\\\\\\\\",
      "<td>" -> "",
      "</td>" -> " & ",
      "</table>" ->  "\\\\hline\n \\\\end{tabular} \\\\end{center}",

      // SIZES
      """<font size="\+1">""" -> "\\\\begin{large}\n",
      """</font>""" -> "\\\\end{large}\n",

      // FORMAT
      "<i>" -> """\\textit{""",
      "</i>" -> "} ",
      "<b>" -> """\\textbf{""",
      "</b>" -> "} "


    )


    def translateHTMLTagsToLatex(s: String) = {
      tagsRexs.foldLeft(s)( (accum:String,regex:(String,String)) => accum.replaceAll(regex._1,regex._2 ) )
    }
    def apply(s:String) = translateHTMLTagsToLatex(s)
  }

  private def translateHtmlToTex(s: String) = {

    val chunks = GiftHTMLParser.parseAll(GiftHTMLParser.chunks, s).getOrElse( List(GiftHTMLParser.TextChunk(s)) )
    val ret = chunks.map(_.toLatex).mkString

    ret
  }

  private def generateQuestionLatex(q: Question, reduceOpenQuestions: Boolean ): String = q match {
    case question:OpenQuestion =>

      val typeOfQuestion = {
        if( !reduceOpenQuestions ) {
          if (question.fullPageQuestion) "FullPageOpenQuestion" else "HalfPageOpenQuestion"
        }
        else{
          if (question.fullPageQuestion) "HalfPageOpenQuestion" else "QuarterPageOpenQuestion"
        }
      }
      s"\\begin{${
        typeOfQuestion
      }}\n  ${
        translateHtmlToTex(question.text)
      }\n\\end{${
        typeOfQuestion
      }}"

    case QuestionnaireQuestion(text, answers) =>
      val sa = answers.map {
        a => s"    \\item ${
          translateHtmlToTex(a.text)
        }"
      }.mkString("\n")

      val begin = s"  \\begin{QuestionnaireQuestion}[${
        translateHtmlToTex(text)
      }]\n"
      val end = "\n  \\end{QuestionnaireQuestion}"

      begin + sa + end
  }

  private def generateLatexForQuestions(g: GiftFile, reduceOpenQuestions: Boolean ) = {
    val qq = g.questionnaireQuestions.map(GiftToLatex.generateQuestionLatex(_,reduceOpenQuestions)).mkString("\n")
    val oq = g.openQuestions.map(GiftToLatex.generateQuestionLatex(_,reduceOpenQuestions)).mkString("\n")

    s"\\begin{QuestionnaireQuestions}\n$qq\n\\end{QuestionnaireQuestions}\n$oq"
  }

  private def generateSolutionIndexes(g: GiftFile) = {
    g.questions.filter(_.isInstanceOf[QuestionnaireQuestion]).map {
      case QuestionnaireQuestion(_, answers) =>
        answers.indexWhere(_.correct)
    }

  }

  private val latexTemplate = resourceToString("giftToLatex/QuestionnaireGradingTest.template.tex")



  def generateLatex(f: GiftFile, headerText: String = "", questionnaireQuestionsWeight: Int = 60, horizontal: Boolean = true, ticked: Boolean = false, imagePath: Seq[String] = Seq(), reduceOpenQuestions: Boolean=false ): String = {

    val version = QuestionnaireVersion.version(horizontal,ticked)

    val openQuestionsWeight = 100 - questionnaireQuestionsWeight
    val firstPage = s"\\FirstPage{$questionnaireQuestionsWeight}{$openQuestionsWeight}{${f.questionnaireQuestions.size}}{$horizontal}{$ticked}"
    val questions = generateLatexForQuestions(f,reduceOpenQuestions)
    val solutionIndexes = generateSolutionIndexes(f)
    val solutions = solutionIndexes.map(i => (i.toChar + 'a').toChar).mkString(",")
    val qrCodeData = BinaryConverter.toBase64( BinaryConverter.toBinarySolutions(solutionIndexes,version) )
    val generatedContent = List( firstPage, questions ).mkString("\n")
    
    def toImagePath(s: String ) = {
      s"{${if( s.last == '/' ) s else s + '/'}}"
    }


    val substitutions = Map(
      "$GeneratedContent$" -> generatedContent,
      "$ImagePath$" -> imagePath.map(toImagePath).mkString(","),
      "$HeaderText$"-> headerText,
      "$Solutions$"-> solutions,
      "$QRCodeData$"-> qrCodeData,
      "$GiftFile$" -> ("\"" + f.file.map(_.getAbsolutePath).getOrElse("") + "\"")
    )

    substitutions.foldLeft(latexTemplate){ case (latex,(k,v)) => latex.replace(k,v) }
  }

  trait GiftToLatexConfig {
    val giftFile: File
    val headerText: String = "headerText"
    val horizontalTable: Boolean = true
    val tickedTable: Boolean = false
    val reduceOpenQuestions: Boolean = false
    val questionnaireQuestionsWeight: Int = 60
    val imagePath : Seq[String] = Seq()
  }



  def apply(giftParsedFile: GiftFile)(implicit c: GiftToLatexConfig ) : String = {

    val headerText = c.headerText
    val questionnaireQuestionsWeight = c.questionnaireQuestionsWeight
    val horizontal = c.horizontalTable
    val reduceOpenQuestions = c.reduceOpenQuestions
    val ticked = c.tickedTable
    val imagePath = c.imagePath


    val additionalImagePath = c.giftFile.getAbsoluteFile.getParent
    val ip = additionalImagePath +: imagePath
    logger.debug( ip.toString )
    generateLatex( giftParsedFile, headerText, questionnaireQuestionsWeight, horizontal, ticked, ip, reduceOpenQuestions )


  }

}
