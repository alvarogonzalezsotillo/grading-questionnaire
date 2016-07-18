package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import imgproc.ImageProcessing._
import imgproc.TestUtil._
import imgproc.ocr.OneLetterOCR._
import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr.{Pattern, TrainedOneLetterOCR}
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import java.io.File
import javax.imageio.ImageIO

import org.opencv.core._
import Implicits._
import imgproc.steps.AnswersInfo.cells

import scala.util.Random


@RunWith(classOf[JUnitRunner])
class OCRTest extends FlatSpec {


  nu.pattern.OpenCV.loadLibrary()

  private def cellsOfTestImage(f: String) = {
    val img = readImageFromResources(f)
    val info = ProcessingStep.cellsOfAnswerMatrix_matrixBased.process(img)
    info(cells).get
  }


  ignored{
    behavior of "bounding boxes"

    it should "find overlap if contained" in {
      val r1 = new Rect(0, 0, 10, 10)
      val r2 = new Rect(1, 1, 1, 1)
      assert(r1 overlaps r2)
      assert(r2 overlaps r1)
    }

    it should "find overlap if one corner inside" in {
      val r1 = new Rect(0, 0, 10, 10)
      val r2 = new Rect(-1, -1, 5, 5)
      assert(r1 overlaps r2)
      assert(r2 overlaps r1)
    }

    it should "find overlap if two corners inside" in {
      val r1 = new Rect(0, 0, 10, 10)
      val r2 = new Rect(-1, 2, 5, 5)
      assert(r1 overlaps r2)
      assert(r2 overlaps r1)
    }

    it should "find overlap if no corners inside" in {
      val r1 = new Rect(0, 0, 10, 10)
      val r2 = new Rect(-1, 2, 20, 20)
      assert(r1 overlaps r2)
      assert(r2 overlaps r1)
    }

    it should "find no overlap if there is no overlap" in {
      val r1 = new Rect(0, 0, 10, 10)
      val r2 = new Rect(-4, -4, 2, 2)
      assert(!(r1 overlaps r2))
      assert(!(r2 overlaps r1))
    }


  }

  val fs = Seq("ocr-1.png", "ocr-2.png")
  ignored {


    behavior of "Cell"

    it should "be thresholded to find single letters" in {
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        saveTestImage(s"12-${index + 1}-$f", c)
        saveTestImage(s"12-${index + 1}-lettersContours-$f", thresholdLettersImage(c))
      }
    }

    it should "find contours from thresholded image" in {
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        val contours = findContoursOfLetterFragment(thresholdLettersImage(c))
        drawContours(c, contours, new Scalar(255, 0, 255), 1)
        saveTestImage(s"13-${index + 1}-$f", c)
      }
    }

    it should "find letter candidate contours from thresholded image" in {
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        val contours = extractPossibleLettersBBox(c)
        drawContours(c, contours.map(_.asShape), new Scalar(255, 0, 255), 1)
        saveTestImage(s"14-${index + 1}-$f", c)
      }
    }

    it should "find candidate letters from image" in {
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        val candidates = extractPossibleLettersImage(c).map(Pattern.resizeToPatterSize)
        for ((candidate, candidateIndex) <- candidates.zipWithIndex) {
          saveTestImage(s"15-${index + 1}-$candidateIndex-$f", candidate)
        }
      }
    }
  }

  {
    behavior of "Candidate letter"

    it should "be normalized" in {
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        val candidates = extractPossibleLettersImage(c)
        for ((candidate, candidateIndex) <- candidates.zipWithIndex) {
          saveTestImage(s"16-${index + 1}-$candidateIndex-$f", normalizeLetter(candidate))
        }
      }
    }

    it should "improve bounding box if extracted again" in{
      for (f <- fs; cells = cellsOfTestImage(f); (c, index) <- cells.zipWithIndex) {
        val candidates = extractPossibleLettersImage(c).map(Pattern.resizeToPatterSize)
        for ((candidate, candidateIndex) <- candidates.zipWithIndex ; again <- extractPossibleLettersImage(candidate) ) {
          saveTestImage(s"17-${index + 1}-$candidateIndex-$f", normalizeLetter(again))
        }
      }

    }
  }



  ignored{

    def recognize(letter: Char) = {
      val ocr = new TrainedOneLetterOCR()
      val m = readImageFromResources(s"to-recognize-$letter.png")
      val prediction = ocr.predict(m)
      println( s"Reconociendo $letter: $prediction")
      assert( prediction.significative )
      assert(prediction.prediction.get.toLower == letter)
    }

    behavior of "A trained ocr"

    it should "not launch any exception" in {
      new TrainedOneLetterOCR()
    }

    it should "recognize sample a" in {
      recognize('a')
    }

    it should "recognize sample b" in {
      recognize('b')
    }

    it should "recognize sample c" in {
      recognize('c')
    }

    it should "recognize sample d" in {
      recognize('d')
    }

    it should "try to clasify all pending images" in{
      val baseDirs = Seq(
        new File("blob-detect/src/main/resources/training-models/to-clasify"),
        new File("src/main/resources/training-models/to-clasify")
      )


      val files = baseDirs.map(_.listFiles).filter(_ != null ).flatten
      val ocr = new TrainedOneLetterOCR()
      
      for(f <- files ){
        val m : Mat = ImageIO.read(f)

        {
          val mats = extractPossibleLettersImage(m)
          for ((m, i) <- mats.zipWithIndex) {
            saveTestImage(s"$i-${f.getName}", m)
          }
        }

        val prediction = ocr.predict(m)
        saveTestImage("prediction-" + prediction.prediction.getOrElse("#") + "-" + prediction.description + "-" + f.getName(), m)
      }
    }

    it should "have a high accuracy with all the patterns" in {

      val trainingPatterns = Pattern.trainingPatterns
      val ocr = new TrainedOneLetterOCR(trainingPatterns)

      println( "All")
      for( (l,ps) <- trainingPatterns ){
        val status = runSomeTestAndFailIfSoMuchFailures(ps,1){ p =>
          val prediction = ocr.predict(p)
          assert( prediction.prediction.get == l )
        }
        println( s"$l -> $status" )
      }
    }
    it should "have a high accuracy with some of the training patterns" in{

      def split( patterns : TrainingPatterns, trainingRatio: Double = 0.5 ) : (TrainingPatterns,TrainingPatterns) = {
        val splitted = for( (l,ps) <- patterns ) yield {
          val cut = (ps.size*trainingRatio).toInt
          assert( cut > 0 && cut < ps.size )
          val (training,test) = Random.shuffle(ps).splitAt(cut)
          ( l-> training, l -> test )
        }

        (splitted.map(_._1).toMap, splitted.map(_._2).toMap)
      }

      val (trainingPatterns, testPatterns) = split(Pattern.trainingPatterns)

      val ocr = new TrainedOneLetterOCR(trainingPatterns)

      println( "Same as training")
      for( (l,ps) <- trainingPatterns ){
        val status = runSomeTestAndFailIfSoMuchFailures(ps,1){ p =>
          val prediction = ocr.predict(p)
          assert( prediction.prediction.get == l )
        }
        println( s"$l -> $status" )
      }

      println( "Other letters")
      for( (l,ps) <- testPatterns ){
        val status = runSomeTestAndFailIfSoMuchFailures(ps,1){ p =>
          val prediction = ocr.predict(p)
          assert( prediction.prediction.get == l )
        }
        println( s"$l -> $status" )
      }
    }

  }
}
