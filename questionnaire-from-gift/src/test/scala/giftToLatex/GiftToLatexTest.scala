package giftToLatex

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File

import giftParser.GiftParser.{GiftError, GiftFile}
import giftParser.TestGiftGenerator._
import giftParser.{GiftParser, Util}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class GiftToLatexTest extends FlatSpec {

  val bigGiftFile = new File("/home/alvaro/SincronizadoCloud/copy/2014-2015-Alonso de Avellaneda/seguridad-informatica/examenes/SI-Extraordinaria-Junio.gift")

  "latex compile" should "work with an example file" in {
    val latex = GiftToLatex(bigGiftFile)
    LatexCompiler.compile( latex, new File( "example.pdf") )
  }

  "latex compile" should "work with a generated file" in {

    val s = generateGift(38,5)
    val f = Util.createFile(renderGift(s), new File("generated.gift"))

    val latex = GiftToLatex(f, imagePath = Seq("src/test/resources/giftParser/") )
    LatexCompiler.compile( latex, new File( "generated.pdf") )

  }

  "A big file" should "generate latex" in {
    val ret = GiftParser.parse(bigGiftFile)

    ret match {
      case GiftError(msg, line, column, lineContents) =>
        fail(msg)

      case g: GiftFile =>
        val latex = GiftToLatex.generateLatex(g)
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

  "An image in html" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \includegraphics{image.jpg} and some text """ )
  }

  "Some images in html" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \includegraphics{image.jpg} and some text \\ \includegraphics{image2.jpg} """ )
  }

  "Some images in html (quoted or not)" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> and another image <img src=image3.jpg> more """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \includegraphics{image.jpg} and some text \\ \includegraphics{image2.jpg} and another image \\ \includegraphics{image3.jpg} more """ )
  }

  "Some images in (answers) html (quoted or not)" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> and another image <img src=image3.jpg> more """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \includegraphics{image.jpg} and some text \\ \includegraphics{image2.jpg} and another image \\ \includegraphics{image3.jpg} more """ )
  }

  "A question with html list" should "parse" in{
    val html =
      """una pregunta con lista
        |<ul>
        | <li>un item</li>
        | <li>Otro item</li>
        |</ul>""".stripMargin
    val tex = GiftToLatex.htmlToLatex(html)
    val expected = "una pregunta con lista\n\\begin{itemize} \n \\item un item\n \\item Otro item\n\\end{itemize} "
    //println(tex.replaceAll("\n","\\\\n").replaceAll("\b","·") )
    //println(expected.replaceAll("\n","\\\\n").replaceAll("\b","·"))
    assert( tex ==expected)
  }


}

