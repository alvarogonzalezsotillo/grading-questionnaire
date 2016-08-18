package common

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

/**
  * Created by alvaro on 18/08/16.
  */


/**
  * Created by alvaro on 7/06/16.
  */
@RunWith(classOf[JUnitRunner])
class QuestionnaireVersionTest extends FlatSpec {

  import QuestionnaireVersion._

  it should "work if vertical ticked" in{
    val v = version(false,true)
    assert( isVerticalVersion(v) )
    assert( isTickedVersion(v))
  }

  it should "work if vertical letter" in{
    val v = version(false,false)
    assert( isVerticalVersion(v) )
    assert( isLetterVersion(v))
  }

  it should "work if horizontal ticked" in{
    val v = version(true,true)
    assert( isHorizontalVersion(v) )
    assert( isTickedVersion(v))
  }

  it should "work if horizontal letter" in{
    val v = version(true,false)
    assert( isHorizontalVersion(v) )
    assert( isLetterVersion(v))
  }

}
