package imgproc.ocr.perceptron

import org.junit.runner.RunWith
import org.opencv.core.Mat
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PerceptronTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.Implicits._


  "Simple forms" should "be inferred" in {
    val patternSize = 5

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

    def noise( matrix: Array[Array[Int]], times: Int = patternSize ) = {
      for( _ <- 1 to times ) {
        val f = Random.nextInt(matrix.length)
        val c = Random.nextInt(matrix(0).length)
        val v = if (Random.nextBoolean) 255 else 0
        matrix(f)(c) = v
      }
      matrix
    }

    def dump( m: Array[Array[Int]]) = {
      println( "--------")
      for( r <- 0 until m.length ){
        println( m(r).mkString("\t") )
      }
      println( "--------")
    }

    val samples = 10
    val A = Iterator.continually[Mat]( noise(a) ).take(samples).toSeq
    val B = Iterator.continually[Mat]( noise(b) ).take(samples).toSeq
    val C = Iterator.continually[Mat]( noise(c) ).take(samples).toSeq
    val D = Iterator.continually[Mat]( noise(d) ).take(samples).toSeq


    val data = Map('A' -> A, 'B' -> B, 'C' -> C, 'D' -> D)


    val p = new Perceptron(patternSize, 2, patternSize)
    val iterations = p.train(data)
    println(s"iterations:$iterations")


    val predictionA = p.predict(noise(a))
    println(s"predictionA:$predictionA")

    val predictionB = p.predict(noise(b))
    println(s"predictionB:$predictionB")

    val predictionC = p.predict(noise(c))
    println(s"predictionC:$predictionC")

    val predictionD = p.predict(noise(d))
    println(s"predictionD:$predictionD")

    dump(noise(a))
  }


}
