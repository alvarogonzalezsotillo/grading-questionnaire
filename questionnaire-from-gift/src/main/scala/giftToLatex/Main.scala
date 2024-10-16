package giftToLatex

import java.io.{File, FileOutputStream, InputStream}
import java.nio.channels.Channels

import giftParser.GiftParser
import giftParser.GiftParser.{GiftError, GiftFile}
import giftToLatex.GiftToLatex.{GiftToLatexConfig, logger}
import com.typesafe.scalalogging.LazyLogging

import scala.util.Random


/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App with LazyLogging {

  val invalidFile = new File(".")

  case class Config(override val giftFile: File = invalidFile,
                    keepTexFile: Boolean = false,
                    help: Boolean = false,
                    override val headerText: String = "",
                    maxQuestionnaireQuestions: Int = Integer.MAX_VALUE,
                    numberOfVariations: Int = 2,
                    override val horizontalTable: Boolean = true,
                    override val tickedTable: Boolean = false,
                    override val reduceOpenQuestions: Boolean = false,
                    override val questionnaireQuestionsWeight: Int = 60) extends GiftToLatex.GiftToLatexConfig

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

      opt[Int]('m', "max-questionnaire-questions") text ("maximum number of questionnaire questions") action { (m,c) =>
        c.copy(maxQuestionnaireQuestions = m)
      }

      opt[Int]('n', "number-of-variations") text ("Number of variations of the questionnarie, with permuted answers. Defaults to 2.") action { (n, c) =>
        c.copy(numberOfVariations = n)
      }

      opt[String]('t',"header-text").text("Header text").required().action{ (t,c) =>
        c.copy(headerText = t)
      }

      opt[Boolean]('x', "ticked-table") text ("Check box answer table instead of letters. Defaults to letters.") action{ (x,c) =>
        c.copy( tickedTable = x )
      }

      opt[Boolean]('v', "vertical-table") text ("Horizontal answer table. Defaults to horizontal") action{ (v,c) =>
        c.copy( horizontalTable = !v )
      }

      opt[Boolean]('r', "reduce-open-questions") text ("Reserved half the space for open questions") action{ (r,c) =>
        c.copy( reduceOpenQuestions = r )
      }


      opt[Unit]('h',"help") text("Shows this help") action { (_,c) =>
        c.copy(help = true)
      }

      help('h',"help") text("Shows this help")

    }

    def createTempFileFrom( in: InputStream ) = {
      val file = File.createTempFile("","gitftolatex", new File("."))
      file.deleteOnExit()
      val out = new FileOutputStream(file)
      out.getChannel().transferFrom(Channels.newChannel(in), 0, Long.MaxValue )
      out.close()
      file.getAbsolutePath
    }

    def generateQuestionnarieVersion(giftParsedFile: GiftFile, version: Option[String])(implicit c: Config ) = {
      val latex = GiftToLatex(giftParsedFile)(c)
      def computeOutFile: File = {
        val name = if( c.giftFile == invalidFile ){
          createTempFileFrom(System.in)
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
      LatexCompiler.apply(latex, outFile, c.keepTexFile)
    }

    parser.parse(args, Config()) match {
      case Some(c) =>

        logger.error(c.toString)

        val g = GiftParser.parse(c.giftFile).get
        val giftParsedFile = g.reduce(c.maxQuestionnaireQuestions)
        (0 until c.numberOfVariations).map(v => (v + 'A').toChar).foreach{ v =>
          val version = giftParsedFile.reorder()
          generateQuestionnarieVersion(version, Some(v.toString))(c)
        }

      case None =>
    }
  }

  def test = {

    implicit def toFile(s: String) = new File(s)

    val c = new GiftToLatexConfig {
      override val giftFile  : File = "/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/aplicaciones-web-ampliada/Examenes/AW-A-EvaluacionExtraordinaria.gift"
      override val headerText = "Aplicaciones Web - Evaluacion Extraordinaria"
    }

    val giftParsedFile = GiftParser.parse(c.giftFile).get
    val latex = GiftToLatex(giftParsedFile)(c)
    LatexCompiler(latex, "AW-A-EvaluacionExtraordinaria.pdf", true)

  }
}
