package giftToLatex

import java.io.{ByteArrayOutputStream, File, InputStream}

import com.typesafe.scalalogging.slf4j.LazyLogging
import giftParser.GiftParser
import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._

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
      '\\' -> """\textbackslash""",
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
      """(?i)<img src="(.*?)">""" -> """\\\\ \\includegraphics{$1}""",
      """(?i)<img src=(.*?)>""" -> """\\\\ \\includegraphics{$1}""",

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
      """</font>""" -> "\\\\end{large}\n"
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

  private def generateQuestionLatex(q: Question): String = q match {
    case question:OpenQuestion =>
      val typeOfQuestion = if (question.fullPageQuestion) "FullPageOpenQuestion" else "HalfPageOpenQuestion"
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

  private def generateLatexForQuestions(g: GiftFile) = {
    val qq = g.questionnaireQuestions.map(GiftToLatex.generateQuestionLatex).mkString("\n")
    val oq = g.openQuestions.map(GiftToLatex.generateQuestionLatex).mkString("\n")

    s"\\begin{QuestionnaireQuestions}\n$qq\n\\end{QuestionnaireQuestions}\n$oq"
  }

  private def generateLatexSolutionForSolution(g: GiftFile) = {
    val indexes = g.questions.filter(_.isInstanceOf[QuestionnaireQuestion]).map {
      case QuestionnaireQuestion(_, answers) =>
        answers.indexWhere(_.correct)
    }

    val options = indexes.map(i => (i.toChar + 'a').toChar)

    "\\Solution{" + options.mkString(",") + "}"
  }

  private lazy val latexTemplate = {
    def streamToString(in: InputStream) = {
      val bos = new ByteArrayOutputStream()
      val buffer = new Array[Byte](1024)
      var bytesRead = in.read(buffer)
      while (bytesRead > 0) {
        bos.write(buffer, 0, bytesRead)
        bytesRead = in.read(buffer)
      }
      bos.close()
      bos.toString
    }

    val in = getClass.getResourceAsStream("/QuestionnaireGradingTest.template.tex")
    try {
      streamToString(in)
    }
    finally {
      in.close()
    }
  }

  def generateLatex(f: GiftFile, headerText: String = "", questionnaireQuestionsWeight: Int = 60, imagePath: Seq[String] = Seq() ): String = {

    val openQuestionsWeight = 100 - questionnaireQuestionsWeight
    val firstPage = s"\\FirstPage{$questionnaireQuestionsWeight}{$openQuestionsWeight}{${f.questionnaireQuestions.size}}"
    val questions = generateLatexForQuestions(f)
    val solutions = generateLatexSolutionForSolution(f)
    val generatedContent = List( firstPage, questions, solutions).mkString("\n")
    
    def toImagePath(s: String ) = {
      s"{${if( s.last == '/' ) s else s + '/'}}"
    }

    latexTemplate
      .replace("${GeneratedContent}", generatedContent)
      .replace("${ImagePath}", imagePath.map(toImagePath).mkString(","))
      .replace("${HeaderText}", headerText )
      .replace("${GiftFile}", f.file.map(_.getAbsolutePath).getOrElse(""))

  }


  def apply(f: File,  headerText: String = "", questionnaireQuestionsWeight: Int = 60, imagePath: Seq[String] = Seq()): String = {
    GiftParser.parse(f) match {
      case GiftError(msg, line, column, lineContents) =>
        throw new IllegalArgumentException(s"Error:$msg, at $line,$column\n$lineContents")

      case g: GiftFile =>
        val additionalImagePath = f.getAbsoluteFile.getParent
        val ip = additionalImagePath +: imagePath
        logger.debug( ip.toString )
        generateLatex(g, headerText, questionnaireQuestionsWeight, ip )
    }

  }

}
