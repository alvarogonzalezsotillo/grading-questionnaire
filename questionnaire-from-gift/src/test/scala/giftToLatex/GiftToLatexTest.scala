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


  "latex compile" should "work with a generated file" in {

    val s = generateGift(40,5)
    val f = Util.createFile(renderGift(s), new File("generated.gift"))

    val latex = GiftToLatex(f, imagePath = Seq("src/test/resources/giftParser/") )
    LatexCompiler.compile( latex, new File( "generated.pdf") )

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

  "A list of solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3)
    val expected = Array[Byte]( BinaryConverter.version, 2, solutions.size.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions)
    assert( expected.toSeq == bin.toSeq )
  }
    
  "A list of odd solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3,3)
    val expected = Array[Byte]( BinaryConverter.version, 2, solutions.size.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte, (3 << 6).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions)
    assert( expected.toSeq == bin.toSeq )
  }


  "A binary solution" should "convert to indexes" in {
    val solutions = Array[Byte]( BinaryConverter.version, 2, 4, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte )
    val expected = Seq(0,1,2,3)
    val sol = BinaryConverter.fromBinarySolutions(solutions)
    assert( expected.toSeq == sol.toSeq )
  }
    
  "A binary solution with odd number of indexes" should "convert to indexes" in {
    val solutions = Array[Byte]( BinaryConverter.version, 2, 5, ((((0)<<2 | 1)<<2 | 2)<<2 | 3 ).toByte, (3 << 6).toByte )
    val expected = Seq(0,1,2,3,3)
    val sol = BinaryConverter.fromBinarySolutions(solutions)
    assert( expected.toSeq == sol.toSeq )
  }



  "Another list of solutions" should "convert to binary" in {
    val solutions = Seq(0,1,2,3,3,2,1,0)
    val expected = Array[Byte]( BinaryConverter.version, 2, 8.toByte, ((((0)<<2 | 1)<<2 | 2)<<2 | 3).toByte, ((((3)<<2 | 2)<<2 | 1)<<2 | 0).toByte )
    val bin = BinaryConverter.toBinarySolutions(solutions)
    assert( expected.toSeq == bin.toSeq )
  }



  "A list of solutions" should "convert to binary and convert again to indexes" in {
    val solutions = Seq(0,1,2,3,3,2,1,0,3,2,1,0,0,1,2,3)
    val bin = BinaryConverter.toBinarySolutions(solutions)
    val sol = BinaryConverter.fromBinarySolutions(bin)
    assert( solutions.toSeq == sol.toSeq )
  }
    
  "A list of odd solutions" should "convert to binary and convert again to indexes" in {
    val solutions = Seq(0,1,2,3,3,2,1,0,3,2,1,0,0,1,2,3,1)
    assert( solutions.size % 2 == 1 )
    val bin = BinaryConverter.toBinarySolutions(solutions)
    val sol = BinaryConverter.fromBinarySolutions(bin)
    assert( solutions.toSeq == sol.toSeq )
  }


}

