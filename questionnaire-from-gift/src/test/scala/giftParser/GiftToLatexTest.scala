package giftParser

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File

import giftParser.GiftParser.{GiftFile, GiftError}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import giftParser.TestGiftGenerator._
import giftParser.Util._


@RunWith(classOf[JUnitRunner])
class GiftToLatexTest extends FlatSpec {

  val bigGiftFile = new File("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/seguridad-informatica/examenes/SI-Extraordinaria-Junio.gift")

  "latex compile" should "work with an example file" in {
    val ret = GiftParser.parse(bigGiftFile)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        val latex = GiftToLatex(g)
        LatexCompiler.compile( latex, new File( "example.pdf") )
    }

  }

  "latex compile" should "work with a generated file" in {

    val s = generateGift(300,10)
    val f = Util.createFile(renderGift(s), new File("generated.gift"))

    val ret = GiftParser.parse(f)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        val latex = GiftToLatex(g)
        LatexCompiler.compile( latex, new File( "generated.pdf") )
    }

  }

  "A big file" should "generate latex" in {
    val ret = GiftParser.parse(bigGiftFile)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        val latex = GiftToLatex(g)
        println( latex )
    }
  }


  "A big file" should "parse" in {
    val ret = GiftParser.parse(bigGiftFile)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
    }
  }

  "A generated file" should "parse" in {

    val s = TestGiftGenerator.generateGift(40,4)
    val f = createFile(renderGift(s), new File("generated.gift"))

    val ret = GiftParser.parse(f)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        assert(g.openQuestions.size == 4)
        assert(g.questionnaireQuestions.size == 40)

    }
  }

}

