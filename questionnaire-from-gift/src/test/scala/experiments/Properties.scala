package giftParser

import java.io.File

import experiments.Properties.RWProp
import giftParser.GiftParser.GiftFile._
import giftParser.GiftParser._
import giftToLatex.GiftToLatex
import GiftToLatex.GiftHTMLParser
import giftParser.TestGiftGenerator._
import giftParser.Util._
import giftToLatex.GiftToLatex.GiftHTMLParser._
import org.junit.runner.RunWith
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatestplus.junit.JUnitRunner

import scala.util.Try


/**
 * Created by alvaro on 21/09/2014.
 */
@RunWith(classOf[JUnitRunner])
class Properties extends FlatSpec {


  "A prop" should "be converted to its type" in {
    import experiments.Properties.implicits._
    val p = RWProp(5)
    val v : Int = p
    assert( v == 5 )
  }

  "A RW prop" should "be updatable" in{
    val p = RWProp("Hola")
    p() = "adios"
    assert( p() == "adios" )
  }

  "A RW prop" should "notify a change" in{
    val p = RWProp("Hola")
    var modified = false
    p.addListener{
      modified = true
    }
    p() = "adios"
    assert( p() == "adios" )
  }

  "A derived prop" should "update when primary updates" in{
    val p = RWProp(5)
    val dp = p.derive{ v =>
      v.toString
    }
    assert( dp() == "5" )
    p() = 8
    assert( dp() == "8" )
  }

  "A two level derived prop" should "update when primary updates" in{
    val p = RWProp(5)
    val dp = p.derive{ v =>
      v.toString
    }
    val ddp = dp.derive{ v =>
      v + v
    }
    assert( ddp() == "55" )
    p() = 8
    assert( ddp() == "88" )
  }

}
