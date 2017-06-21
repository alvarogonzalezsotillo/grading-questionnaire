package imgproc

import java.io.File
import javax.imageio.ImageIO

import common.TestUtil
import imgproc.ImageProcessing._
import imgproc.Implicits._
import TestUtil._
import imgproc.ocr.OneLetterOCR._
import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr._
import imgproc.steps.AnswersInfo.cells
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._
import org.junit.runner.RunWith
import org.opencv.core._
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Random


/**
  * Created by alvaro on 1/02/17.
  */
@RunWith(classOf[JUnitRunner])
class EmptyRecognizerTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()


  "Empty recognizer" should "recognize an empty pattern" in{
    val m = readImageFromResources(s"to-recognize-#.png")
    assert( DefaultEmptyRecognizer.isEmpty(m) )
  }


  "Empty recognizer" should "recognize a non-empty pattern" in{
    val all = for( c <- Seq('X', 'O', 'a','b','c','d') ) yield {
      val m = readImageFromResources(s"to-recognize-$c.png")
      c -> !DefaultEmptyRecognizer.isEmpty(m)
    }
    println( all )
    assert( all.forall( _._2 ) )

  }

  "Inspecting histograms" should "dump data" in{
    DefaultEmptyRecognizer.inspect()
  }


}
