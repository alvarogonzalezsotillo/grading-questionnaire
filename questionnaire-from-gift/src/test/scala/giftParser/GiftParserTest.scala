package giftParser

import giftParser.GiftParser._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
/**
 * Created by alvaro on 21/09/2014.
 */
@RunWith(classOf[JUnitRunner])
class GiftParserTest extends FlatSpec {

  "Empty file" should "have no questions" in {
    val s = ""
    val ret = GiftParser.parse(s)
    assert( ret.successful )
    val giftFile = ret.get
    assert( giftFile.size == 0 )
  }

  "Single open question" should "parse" in {
    val singleOpenQuestion =
      """hola
        |{
        |
        |}""".stripMargin
    val ret = GiftParser.parse(singleOpenQuestion)
    assert( ret.successful )
    val giftFile = ret.get
    assert( giftFile(0).isInstanceOf[OpenQuestion] )
  }

  "Single open question with no extra new lines" should "parse" in {
    val singleOpenQuestion = """hola{ }"""
    val ret = GiftParser.parse(singleOpenQuestion)
    assert( ret.successful )
    val giftFile = ret.get
    assert( giftFile(0).isInstanceOf[OpenQuestion] )
  }

  "Single closed question" should "parse" in {
    val singleOpenQuestion =
      """hola como estás yo bien gracias{
        |=bien
        |~mal
        |}""".stripMargin
    val ret = GiftParser.parse(singleOpenQuestion)
    assert( ret.successful )
    val giftFile = ret.get
    assert( giftFile(0).isInstanceOf[QuestionnaireQuestion] )
  }

  "Single closed question" should "parse with correct answers" in {
    val singleOpenQuestion =
      """hola como estás yo bien gracias{
        |=bien
        |~mal
        |}""".stripMargin
    val ret = GiftParser.parse(singleOpenQuestion)
    assert( ret.successful )
    val giftFile = ret.get
    val q = giftFile(0).asInstanceOf[QuestionnaireQuestion]
    assert( q.answers.size == 2 )
    assert( q.answers(0) == Answer( "bien", true) )
    assert( q.answers(1) == Answer( "mal", false) )
  }

  "A malformed file" should "not parse" in {
    val error =" this is an error "
    val ret = GiftParser.parse(error)
    assert( !ret.successful )
  }

}
