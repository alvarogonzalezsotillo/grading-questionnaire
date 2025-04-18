package giftToLatex

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File

import common.BinaryConverter
import giftParser.GiftParser.{GiftError, GiftFile}
import giftParser.TestGiftGenerator._
import giftParser.{GiftParser, Util}
import giftToLatex.GiftToLatex.GiftToLatexConfig
import org.junit.runner.RunWith
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatestplus.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class GiftToLatexTest extends FlatSpec {

  def testFile( name: String ) = new File( new File("./build/gift-to-latex/"), name )

  "latex compile" should "work with a generated file" in {

    val s = generateGift(40,5)
    val f = Util.createFile(renderGift(s), testFile("generated.gift"))

    val c  = new GiftToLatexConfig {
      override val giftFile = f
      override val imagePath = Seq("src/test/resources/giftParser/")
    }

    val g = GiftParser.parse(c.giftFile).get
    val latex = GiftToLatex(g)(c)
    LatexCompiler.compile( latex, testFile( "generated.pdf"), true, 2, true )

  }

  "An image in html" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \begin{center}\includegraphics{image.jpg}\end{center} and some text """ )
  }

  "Some images in html" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \begin{center}\includegraphics{image.jpg}\end{center} and some text \\ \begin{center}\includegraphics{image2.jpg}\end{center} """ )
  }

  "Some images in html (quoted or not)" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> and another image <img src=image3.jpg> more """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \begin{center}\includegraphics{image.jpg}\end{center} and some text \\ \begin{center}\includegraphics{image2.jpg}\end{center} and another image \\ \begin{center}\includegraphics{image3.jpg}\end{center} more """ )
  }

  "Some images in (answers) html (quoted or not)" should "translate to latex" in {
    val html = """a question <img src="image.jpg"> and some text <img src="image2.jpg"> and another image <img src=image3.jpg> more """
    val tex = GiftToLatex.htmlToLatex(html)
    assert( tex == """a question \\ \begin{center}\includegraphics{image.jpg}\end{center} and some text \\ \begin{center}\includegraphics{image2.jpg}\end{center} and another image \\ \begin{center}\includegraphics{image3.jpg}\end{center} more """ )
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

  "A list of solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3)
    val version = 0.toByte
    val expected = Array[Byte]( version, 2, solutions.size.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions,version)
    assert( expected.toSeq == bin.toSeq )
  }
    
  "A list of odd solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3,3)
    val version = 0.toByte
    val expected = Array[Byte]( version, 2, solutions.size.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte, (3 << 6).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions,version)
    assert( expected.toSeq == bin.toSeq )
  }


  "A binary solution" should "convert to indexes" in {
    val version = 0.toByte

    val solutions = Array[Byte](version, 2, 4, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte )
    val expected = Seq(0,1,2,3)
    val (sol,v) = BinaryConverter.fromBinarySolutions(solutions)
    assert( expected == sol )
    assert(version == v )
  }
    
  "A binary solution with odd number of indexes" should "convert to indexes" in {
    val version = 0.toByte
    val solutions = Array[Byte]( version, 2, 5, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte, (3 << 6).toByte )
    val expected = Seq(0,1,2,3,3)
    val (sol,v) = BinaryConverter.fromBinarySolutions(solutions)
    assert( expected == sol )
    assert(version == v )
  }



  "Another list of solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3,3,2,1,0)
    val version = 0.toByte
    val expected = Array[Byte]( version, 2, 8.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3).toByte, ((((3)<<2 | 2)<<2 | 1)<<2 | 0).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions,version)
    assert( expected.toSeq == bin.toSeq )
  }



  "A list of solutions" should "convert to binary and convert again to indexes" in {
    val version = 0.toByte
    val solutions = Seq(0,1,2,3,3,2,1,0,3,2,1,0,0,1,2,3)
    val bin = BinaryConverter.toBinarySolutions(solutions,version)
    val (sol,v) = BinaryConverter.fromBinarySolutions(bin)
    assert( solutions == sol )
    assert(version == v )
  }
    
  "A list of odd solutions" should "convert to binary and convert again to indexes" in {
    val solutions = Seq(0,1,2,3,3,2,1,0,3,2,1,0,0,1,2,3,1)
    assert( solutions.size % 2 == 1 )
    val version = 0.toByte

    val bin = BinaryConverter.toBinarySolutions(solutions,version)
    val (sol,v) = BinaryConverter.fromBinarySolutions(bin)
    assert( solutions == sol )
    assert(version == v )
  }


}

