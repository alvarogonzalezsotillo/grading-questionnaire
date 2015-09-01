package giftToLatex

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging


/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App with LazyLogging {

  val invalidFile = new File(".")

  case class Config(giftFile: File = invalidFile, outFile: File = invalidFile, keepTexFile: Boolean = false, questionnaireQuestionsWeight: Int = 60) {
    val openQuestionsWeight = 100 - questionnaireQuestionsWeight
  }

  test

  def realMain = {
    val parser = new scopt.OptionParser[Config]("gifttolatex") {
      head("gifttolatex", "0.1")

      arg[File]("<gift file>") text("The input GIFT file (https://docs.moodle.org/23/en/GIFT_format).") required() action { (gf, c) =>
        c.copy(giftFile = gf)
      }

      arg[File]("<pdf file>") text("The output PDF file. Defaults to the GIFT file, with pdf extension.") optional() action { (gf, c) =>
        c.copy(outFile = gf)
      }

      opt[Unit]('k', "keep-tex-file") text("Keep the intermediate TEX file.") action { (_, c) =>
        c.copy(keepTexFile = true)
      }

      opt[Int]('q', "questionnaire-weight") text("Percentage of questionnaire questions. Defaults to 60%.") action { (q, c) =>
        c.copy(questionnaireQuestionsWeight = q)
      }

    }

    parser.parse(args, Config()) match {
      case Some(c) =>

        logger.error(c.toString)

        val latex = GiftToLatex(c.giftFile, c.questionnaireQuestionsWeight, c.openQuestionsWeight)
        def computeOutFile: File = {
          if (c.outFile.getAbsolutePath != new File(".").getAbsolutePath) {
            c.outFile
          }
          else {
            val gift = c.giftFile.toString
            val i = gift.lastIndexOf('.')
            val f = (if (i > 0) gift.take(i) else gift) + ".pdf"
            new File(f)
          }
        }
        val outFile = computeOutFile
        LatexCompiler(latex, outFile, c.keepTexFile)

      case None =>
    }
  }

  def test = {

    implicit def toFile(s: String) = new File(s)

    val latex = GiftToLatex("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/aplicaciones-web-ampliada/Examenes/AW-A-EvaluacionExtraordinaria.gift", 50, 50)
    LatexCompiler(latex, "AW-A-EvaluacionExtraordinaria.pdf", true)
  }
}
