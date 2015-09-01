package giftToLatex

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging


/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App with LazyLogging {

  val invalidFile = new File(".")

  case class Config(giftFile: File = invalidFile,
                    keepTexFile: Boolean = false,
                    headerText: String = "",
                    numberOfVersions: Int = 2,
                    questionnaireQuestionsWeight: Int = 60)
  realMain

  def realMain = {
    val parser = new scopt.OptionParser[Config]("gifttolatex") {
      head("gifttolatex", "0.1")

      arg[File]("<gift file>") text ("The input GIFT file (https://docs.moodle.org/23/en/GIFT_format).") required() action { (gf, c) =>
        c.copy(giftFile = gf)
      }


      opt[Unit]('k', "keep-tex-file") text ("Keep the intermediate TEX file.") action { (_, c) =>
        c.copy(keepTexFile = true)
      }

      opt[Int]('q', "questionnaire-weight") text ("Percentage of questionnaire questions. Defaults to 60%.") action { (q, c) =>
        c.copy(questionnaireQuestionsWeight = q)
      }

      opt[Int]('v', "number-of-versions") text ("Number of versions of the questionnarie, with permuted answers. Defaults to 2.") action { (v, c) =>
        c.copy(numberOfVersions = v)
      }

      opt[String]('h',"header-text") text ("Header text") action{ (t,c) =>
        c.copy(headerText = t)
      }

    }

    def generateQuestionnarieVersion(c: Config, version: Option[String]) = {
      val latex = GiftToLatex(c.giftFile, c.headerText, c.questionnaireQuestionsWeight)
      def computeOutFile: File = {
        val gift = c.giftFile.toString
        val i = gift.lastIndexOf('.')
        val name = if (i > 0) gift.take(i) else gift
        val f = name + version.map("-"+_).getOrElse("") + ".pdf"
        new File(f)
      }
      val outFile = computeOutFile
      LatexCompiler(latex, outFile, c.keepTexFile)
    }

    parser.parse(args, Config()) match {
      case Some(c) =>

        logger.error(c.toString)

        (0 until c.numberOfVersions).map(v => (v + 'A').toChar).foreach( v => generateQuestionnarieVersion(c, Some(v.toString)))

      case None =>
    }
  }

  def test = {

    implicit def toFile(s: String) = new File(s)

    val latex = GiftToLatex("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/aplicaciones-web-ampliada/Examenes/AW-A-EvaluacionExtraordinaria.gift", "Aplicaciones Web - Evaluacion Extraordinaria", 50)
    LatexCompiler(latex, "AW-A-EvaluacionExtraordinaria.pdf", true)
  }
}
