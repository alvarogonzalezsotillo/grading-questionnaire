package giftToLatex

import java.io.File

/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App {

  val invalidFile = new File(".")

  case class Config(giftFile: File = invalidFile, outFile: File = invalidFile, keepTexFile: Boolean = false, questionnaireQuestionsWeight: Int = 60) {
    val openQuestionsWeight = 100 - questionnaireQuestionsWeight
  }

  /*
  val parser = scopt.OptionParser[Config]("gifttolatex") {
    head("gifttolatex", "0.1")

    arg[File]("<gift file>") required() action { (gf, c) =>
      c.copy(giftFile = gf)
    }

    arg[File]("<pdf file>") optional() action { (gf, c) =>
      c.copy(outFile = gf)
    }

    opt[Unit]('k', "keep-tex-file") action { (_, c) =>
      c.copy(keepTexFile = true)
    }

    opt[Int]('q', "questionnaire-weight") action { (q, c) =>
      c.copy(questionnaireQuestionsWeight = q)
    }

  }

  parser.parse(args, Config()) match {
    case Some(c) =>
      val latex = GiftToLatex(c.giftFile, c.questionnaireQuestionsWeight, c.openQuestionsWeight)
      LatexCompiler(latex, c.outFile, c.keepTexFile)

    case None =>
  }
  */
}
