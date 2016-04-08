package imgproc.ocr.perceptron

import java.util

import imgproc.ocr.OneLetterOCR.{LetterProb, LetterResult}
import imgproc.ocr.Pattern
import org.opencv.core.{CvType, Mat}
import org.opencv.ml.CvANN_MLP

/**
 * Created by alvaro on 6/04/16.
 * http://stackoverflow.com/questions/34246265/opencv-ann-mlp-training-in-android
 * http://stackoverflow.com/questions/17774063/opencv-error-bad-argument-in-cvann-mlp
 * http://projectagv.blogspot.com.es/2008/12/sample-code-for-cvannmlp.html
 * http://docs.opencv.org/2.4/doc/tutorials/ml/introduction_to_svm/introduction_to_svm.html
 */

object Perceptron{

  private def imageToSignal(i: Byte ) : Float = 1.0f*i/255
  private def signalToImage(s: Float ) : Byte = (s*255).toByte

  def labelOfLetter( le: Char ) : Int = le - 'A'
  def letterOfLabel( la: Int ) : Char = ('A' + la).toChar

  private def fillRowWithArray( m: Mat, row: Int, b: Array[Float] ){
    assert(m.cols() == b.size )
    val buffer = new Array[Float](1)
    for( i <- 0 until m.cols() ){
      buffer(0) = b(i)
      m.put(row,i,buffer)
    }
  }

}

class Perceptron( nodesInInternalLayers: Int = 50, internalLayers: Int = 2, patternSize : Int = Pattern.patternSize ) {

  import Perceptron._

  private val nodesInInputLayer = patternSize * patternSize

  private def layerSizes( outputNodes: Int ) = {
    val m = new Mat(2+internalLayers, 1, CvType.CV_32SC1)
    m.put( 0, 0, nodesInInputLayer ) // INPUT LAYER
    for( l <- 0 until internalLayers ) {
      m.put(1+l, 0, nodesInInternalLayers)
    }
    m.put( 1+internalLayers, 0, outputNodes ) // OUTPUT LAYER (A,B,C,D)
    m
  }

  def patternToInputData( pattern: Mat ) : Array[Float] = {
    assert( pattern.rows() == patternSize )
    assert( pattern.cols() == patternSize )

    val ret = new Array[Float](nodesInInputLayer)
    val buffer = new Array[Byte](1)
    for( c <- 0 until patternSize ; r <- 0 until patternSize ){
      pattern.get(r,c,buffer)
      ret(patternSize*c+r) = imageToSignal(buffer(0))
    }
    ret
  }



  val ann = new CvANN_MLP

  def train( data: Map[Char,Seq[Mat]], alpha: Double = 1,  beta: Double = 1, activateFunc : Int = CvANN_MLP.SIGMOID_SYM) : Int = {

    def trainDataset( data: Map[Char,Seq[Mat]] ) : (Mat,Mat,Mat) = {
      val letters = data.keys.toList

      assert( letters.toSet == (0 until letters.size).map( letterOfLabel ).toSet )

      val labels = {
        val labelsArray : Array[Int] = letters.flatMap( l => Iterator.continually(labelOfLetter(l)).take(data(l).size) ).toArray
        val ret = new Mat(labelsArray.size, letters.size,CvType.CV_32FC1)
        val buffer = new Array[Float](letters.size)
        for( r <- 0 until labelsArray.size ){
          util.Arrays.fill(buffer,0)
          buffer(labelsArray(r)) = 1
          ret.put(r,0,buffer)
        }
        ret
      }

      val weights = new Mat

      val input = {
        val patterns = letters.flatMap( l => data(l).toList ).toArray
        val n = patterns.size
        val ret = new Mat(n,nodesInInputLayer,CvType.CV_32FC1)
        for( r <- 0 until n ; inputData = patternToInputData(patterns(r)); c <- 0 until inputData.size){
          ret.put(r,c, Array(inputData(c)) )
        }
        ret
      }

      (input,labels,weights)

    }



    ann.create(layerSizes(data.size), activateFunc, alpha, beta)

    val (input,labels,weights) = trainDataset( data )
    ann.train( input, labels, weights )
  }


  /*
  If you are using the default cvANN_MLP::SIGMOID_SYM activation function with
   the default parameter values fparam1=0 and fparam2=0 then the function used is y = 1.7159*tanh(2/3 * x),
   so the output will range from [-1.7159, 1.7159], instead of [0,1].
   */
  def normalizeProbability(d: Double) = {
    val min = -1.7159
    val max = 1.7159
    val norm = (d - min)/(max-min)
    val ret = (norm min 1) max 0
    ret
  }

  def predict( pattern: Mat ) : LetterResult = {
    val input = patternToInputData(pattern)
    val mInput = new Mat(1,nodesInInputLayer,CvType.CV_32FC1)
    fillRowWithArray( mInput, 0, input )
    val mOutput = new Mat
    ann.predict(mInput,mOutput)

    val letters = (0 until mOutput.cols()).map( letterOfLabel )
    letters.map{ l =>
      val doubles: Array[Double] = mOutput.get(0, labelOfLetter(l) )
      val out = doubles(0)/255
      val prob = normalizeProbability( out )
      println( s"l:$l -> $out -> $prob")
      LetterProb(l, prob )
    }
  }
}
