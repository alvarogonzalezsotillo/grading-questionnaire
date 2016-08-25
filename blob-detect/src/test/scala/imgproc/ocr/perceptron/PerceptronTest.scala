package imgproc.ocr.perceptron

import imgproc.ImageProcessing
import imgproc.ocr.Pattern
import org.junit.runner.RunWith
import org.opencv.core.Mat
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PerceptronTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.Implicits._

  def dump( m: Array[Array[Int]]) = {
    println( "--------")
    for( r <- 0 until m.length ){
      println( m(r).mkString("\t") )
    }
    println( "--------")
  }



  "A pattern" should "be converted to a bidimensional array" in {
    val m = ImageProcessing.readImageFromResources("/imgproc/to-recognize-a.png", getClass() )
    val array = m.toIntArray
    dump( array )
  }

  "Simple forms" should "be inferred" in {
    val patternSize = Pattern.patternSize

    def noise( matrix: Array[Array[Int]], times: Int = patternSize*4 ) = {
      for( _ <- 1 to times ) {
        val f = Random.nextInt(matrix.length)
        val c = Random.nextInt(matrix(0).length)
        val v = if (Random.nextBoolean) 255 else 0
        matrix(f)(c) = v
      }
      matrix
    }

    def a = Array.tabulate[Int](patternSize, patternSize) { (f, c) =>
      if (f < c) 255 else 0
    }
    def b = Array.tabulate[Int](patternSize, patternSize) { (f, c) =>
      if (f > c) 255 else 0
    }

    def c = Array.tabulate[Int](patternSize, patternSize) { (f, c) =>
      if (f == 0) 255 else 0
    }

    def d = Array.tabulate[Int](patternSize, patternSize) { (f, c) =>
      if (c == 0) 255 else 0
    }

    def unrecognizable = Array.tabulate[Int](patternSize, patternSize) { (f, c) =>
      0
    }


    val samples = 1000

    val letters = Map( ' ' -> unrecognizable _,  'A' -> a _, 'B' -> b _ , 'C'-> c _ , 'D' -> d _ )

    val data = letters.map{ case(l,f) => l -> Iterator.continually[Mat]( noise(f()) ).take(samples).toSeq}


    val p = new LetterPerceptron(patternSize, 2, patternSize)
    val iterations = p.train(data)
    println(s"iterations:$iterations")


    for( (l,f) <- letters ){

      val prediction = p.predict(noise(f()) )
      println(s"prediction:$prediction")
      assert(prediction.significative)
      assert( prediction.prediction.get == l )
    }


    dump(noise(a))
  }


}
