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
 */
object Perceptron {


  val nodesInInputLayer = Pattern.patternSize * Pattern.patternSize


  def layerSizes( outputNodes: Int, nodesInInternalLayers: Int = 50, internalLayers: Int = 2 ) = {
    val m = new Mat(2+internalLayers, 1, CvType.CV_8SC1)
    m.put( 0, 0, nodesInInputLayer ) // INPUT LAYER
    for( l <- 0 until internalLayers ) {
      m.put(1+l, 0, nodesInInternalLayers)
    }
    m.put( 1+internalLayers, 0, outputNodes ) // OUTPUT LAYER (A,B,C,D)
    m
  }


  def patternToInputData( pattern: Mat ) : Array[Byte] = {
    assert( pattern.rows() == Pattern.patternSize )
    assert( pattern.cols() == Pattern.patternSize )

    val ret = new Array[Byte](nodesInInputLayer)
    val buffer = new Array[Byte](1)
    for( c <- 0 until Pattern.patternSize ; r <- 0 until Pattern.patternSize ){
      pattern.get(r,c,buffer)
      ret(Pattern.patternSize*c+r) = buffer(0)
    }
    ret
  }


  def labelOfLetter( le: Char ) : Int = le - 'A'
  def letterOfLabel( la: Int ) : Char = ('A' + la).toChar

  def train( data: Map[Char,Seq[Mat]], alpha: Double,  beta: Double, activateFunc : Int = CvANN_MLP.SIGMOID_SYM ) : CvANN_MLP = {

    def trainDataset( data: Map[Char,Seq[Mat]] ) : (Mat,Mat,Mat) = {
      val letters = data.keys.toList

      assert( letters.toSet == (0 until letters.size).map( (_ + 'A').toChar ).toSet )

      val labels = {
        val labelsArray : Array[Int] = letters.flatMap( l => Iterator.continually(labelOfLetter(l)).take(data(l).size) ).toArray
        val n = labelsArray.size
        val ret = new Mat(n,letters.size,CvType.CV_8SC1)
        val buffer = new Array[Byte](letters.size)
        for( r <- 0 until n ){
          util.Arrays.fill(buffer,0.toByte)
          buffer(labelOfLetter(r.toChar)) = 255.toByte
          ret.put(r,0,buffer)
        }
        ret
      }

      val weights = new Mat

      val input = {
        val patterns = letters.flatMap( l => data(l).toList ).toArray
        val n = patterns.size
        val ret = new Mat(n,nodesInInputLayer,CvType.CV_8SC1)
        for( r <- 0 until n ; inputData = patternToInputData(patterns(r)); c <- 0 until inputData.size){
          ret.put(r,c, Array(inputData(c)) )
        }


        ret
      }

      (input,labels,weights)

    }

    val ann = new CvANN_MLP
    ann.create(layerSizes(data.size), activateFunc, alpha, beta)

    val (input,labels,weights) = trainDataset( data )
    ann.train( input, labels, weights )
    ann
  }

  private def fillRowWithArray( m: Mat, row: Int, b: Array[Byte] ){
    assert(m.cols() == b.size )
    val buffer = new Array[Byte](1)
    for( i <- 0 until m.cols() ){
      buffer(0) = b(i)
      m.put(row,i,buffer)
    }
  }

  def predict( ann: CvANN_MLP, pattern: Mat ) : LetterResult = {
    val input = patternToInputData(pattern)
    val mInput = new Mat(1,nodesInInputLayer,CvType.CV_8SC1)
    fillRowWithArray( mInput, 0, input )
    val mOutput = new Mat
    ann.predict(mInput,mOutput)

    val labels = (0 until mOutput.cols()).map( (_ + 'A').toChar )
    labels.map( l => LetterProb(l, mOutput.get(0,l)(0)/255 ) )
  }
}
