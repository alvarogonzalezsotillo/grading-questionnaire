package giftToLatex

import java.io.{InputStream, FileOutputStream, File}
import java.nio.channels.Channels

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.util.Random


/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App with LazyLogging {

  val invalidFile = new File(".")

  case class Config(giftFile: File = invalidFile,
                    keepTexFile: Boolean = false,
                    help :Boolean = false,
                    headerText: String = "",
                    numberOfVariations: Int = 2,
                    horizontalTable: Boolean = true,
                    tickedTable: Boolean = false,
                    questionnaireQuestionsWeight: Int = 60)
  realMain

  def realMain = {
    val parser = new scopt.OptionParser[Config]("gifttolatex") {
      head("gifttolatex", "0.1")

      arg[File]("<gift file>") minOccurs(0) maxOccurs(1) text ("The input GIFT file (https://docs.moodle.org/23/en/GIFT_format). If omitted, standard input is used.") action { (gf, c) =>
        c.copy(giftFile = gf)
      }


      opt[Unit]('k', "keep-tex-file") text ("Keep the intermediate TEX file.") action { (_, c) =>
        c.copy(keepTexFile = true)
      }

      opt[Int]('q', "questionnaire-weight") text ("Percentage of questionnaire questions. Defaults to 60%.") action { (q, c) =>
        c.copy(questionnaireQuestionsWeight = q)
      }

      opt[Int]('n', "number-of-variations") text ("Number of variations of the questionnarie, with permuted answers. Defaults to 2.") action { (n, c) =>
        c.copy(numberOfVariations = n)
      }

      opt[String]('t',"header-text") text ("Header text") required() action{ (t,c) =>
        c.copy(headerText = t)
      }

      opt[Boolean]('x', "ticked-table") text ("Check box answer table instead of letters. Defaults to letters.") action{ (x,c) =>
        c.copy( tickedTable = x )
      }

      opt[Boolean]('h', "horizontal-table") text ("Horizontal answer table. Defaults to horizontal") action{ (h,c) =>
        c.copy( horizontalTable = h )
      }

      opt[Unit]('h',"help") text("Shows this help") action { (_,c) =>
        c.copy(help = true)
      }

    }

    def createTempFileFrom( in: InputStream ) = {
      val file = File.createTempFile("","gitftolatex", new File("."))
      file.deleteOnExit()
      val out = new FileOutputStream(file)
      out.getChannel().transferFrom(Channels.newChannel(in), 0, Long.MaxValue )
      out.close()
    }

    def generateQuestionnarieVersion(c: Config, version: Option[String]) = {
      val latex = GiftToLatex(c.giftFile,  c.headerText, c.questionnaireQuestionsWeight)
      def computeOutFile: File = {
        val name = if( c.giftFile == invalidFile ){
          "stdin"
        }
        else {
          val gift = c.giftFile.toString
          val i = gift.lastIndexOf('.')
          if (i > 0) gift.take(i) else gift
        }
        val f = name + version.map("-"+_).getOrElse("") + ".pdf"
        new File(f)
      }
      val outFile = computeOutFile
      LatexCompiler(latex, outFile, c.keepTexFile)
    }

    parser.parse(args, Config()) match {
      case Some(c) if c.help =>
        parser.showUsage

      case Some(c) =>

        logger.error(c.toString)

        (0 until c.numberOfVariations).map(v => (v + 'A').toChar).foreach( v => generateQuestionnarieVersion(c, Some(v.toString)))

      case None =>
    }
  }

  def test = {

    implicit def toFile(s: String) = new File(s)

    val latex = GiftToLatex("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/aplicaciones-web-ampliada/Examenes/AW-A-EvaluacionExtraordinaria.gift", "Aplicaciones Web - Evaluacion Extraordinaria", 50)
    LatexCompiler(latex, "AW-A-EvaluacionExtraordinaria.pdf", true)
  }
}
