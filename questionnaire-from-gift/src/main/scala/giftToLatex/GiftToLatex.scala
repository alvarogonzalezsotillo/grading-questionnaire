package giftToLatex

import java.io.{ByteArrayOutputStream, File, InputStream}

import giftParser.GiftParser
import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by alvaro on 18/06/15.
 */
object GiftToLatex {


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
      '}' -> """\}"""
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
    private val htmlImgRexs = Seq( """(?i)<img src="(.*?)">""", """(?i)<img src=(.*?)>""")

    def hasHtmlImages(s: String) = htmlImgRexs.contains(s.matches(_))

    def translateImagesToLatex(s: String) = htmlImgRexs.fold(s)((ret, rex) => ret.replaceAll(rex, """\\\\ \\includegraphics{$1}"""))
    def apply(s:String) = translateImagesToLatex(s)
  }

  private def translateHtmlToTex(s: String) = {

    val chunks = GiftHTMLParser.parseAll(GiftHTMLParser.chunks, s).getOrElse( List(GiftHTMLParser.TextChunk("Error parsing:" + s)) )
    val ret = chunks.map(_.toLatex).mkString

    ret
  }

  private def generateQuestionLatex(q: Question): String = q match {
    case OpenQuestion(text) =>
      val typeOfQuestion = if (q.hasImages) "FullPageOpenQuestion" else "HalfPageOpenQuestion"
      s"\\begin{${
        typeOfQuestion
      }}\n  ${
        translateHtmlToTex(text)
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

  def generateLatex(f: GiftFile, imagePath: Seq[String] = Seq(), questionnaireQuestionsWeight: Int = 60, openQuestionsWeight: Int = 40): String = {

    val instructions = s"\\Instructions{$questionnaireQuestionsWeight}{$openQuestionsWeight}"
    val answerTable = s"\\AnswerTable{${
      f.questionnaireQuestions.size
    }}"
    val questions = generateLatexForQuestions(f)
    val solutions = generateLatexSolutionForSolution(f)
    val generatedContent = List(instructions, answerTable, questions, solutions).mkString("\n")

    def toImagePath(s: String ) = {
      s"{${if( s.last == '/' ) s else s + '/'}}"
    }

    latexTemplate
      .replace("${GeneratedContent}", generatedContent)
      .replace("${ImagePath}", imagePath.map(toImagePath).mkString(","))

  }


  def apply(f: File, imagePath: Seq[String] = Seq(), questionnaireQuestionsWeight: Int = 60, openQuestionsWeight: Int = 40): String = {
    GiftParser.parse(f) match {
      case GiftError(msg, line, column, lineContents) =>
        throw new IllegalArgumentException(s"Error:$msg, at $line,$column\n$lineContents")

      case g: GiftFile =>
        val aditionalImagePath = f.getAbsoluteFile.getParent
        val ip = aditionalImagePath +: imagePath
        println( ip )
        generateLatex(g, ip, questionnaireQuestionsWeight, openQuestionsWeight)
    }

  }

}
