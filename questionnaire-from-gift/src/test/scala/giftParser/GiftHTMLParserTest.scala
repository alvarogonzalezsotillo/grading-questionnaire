package giftParser

import java.io.File

import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._
import giftToLatex.GiftToLatex
import GiftToLatex.GiftHTMLParser
import giftParser.TestGiftGenerator._
import giftParser.Util._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Try


/**
 * Created by alvaro on 21/09/2014.
 */
@RunWith(classOf[JUnitRunner])
class GiftHTMLParserTest extends FlatSpec {

  import GiftHTMLParser._

  "If there are no HTML" should "be a text chunk" in {
    val ret = parseAll(textChunk, "only text")
    assert(!ret.isEmpty)
    assert(ret.get.isInstanceOf[TextChunk] )
  }


  "If there are no HTML" should "parse to only one text chunk" in {
    val ret = parseAll(chunks, "only text in chunks")
    assert(!ret.isEmpty)
    assert(ret.get.size == 1)
    assert(ret.get.head.isInstanceOf[TextChunk] )
  }

  "If there is one HTML" should "parse to only one HTML chunk" in {
    val ret = parseAll(chunks, "<only text=a>")
    assert(!ret.isEmpty)
    assert(ret.get.size == 1)
    assert(ret.get.head.isInstanceOf[HTMLChunk] )
  }

  "If there is some HTML" should "parse to some HTML chunk" in {
    val ret = parseAll(chunks, "<only text=a><img src=\"hola\">")
    assert(!ret.isEmpty)
    assert(ret.get.size == 2)
    assert(ret.get.forall( _.isInstanceOf[HTMLChunk] ) )
  }

  "If there is some HTML with text" should "parse to some HTML chunks and text chunks" in {
    val ret = parseAll(chunks, "text <only text=a> text <img src=\"hola\"> text ")
    assert(!ret.isEmpty)
    assert(ret.get.size == 5)
  }

  "A list" should "parse to a list" in {
    val html = "<ul>\n<li>\nhola\n</li>\n</ul>"
    val ret = parseAll(chunks,html)
    assert(!ret.isEmpty)
    println(ret)
  }

}
