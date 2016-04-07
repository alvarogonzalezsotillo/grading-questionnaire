package imgproc.ocr.perceptron

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
  }


  def labelOfLetter( le: Char ) : Int = le - 'A'
  def letterOfLabel( la: Int ) : Char = ('A' + la).toChar

  def train( data: Map[Char,Seq[Mat]], alpha: Double,  beta: Double, activateFunc : Int = CvANN_MLP.SIGMOID_SYM ) = {


    def trainDataset( data: Map[Char,Seq[Mat]] ) : (Mat,Mat,Mat) = {
      val letters = data.keys.toList

      val labels = {
        val labelsArray : Array[Byte] = letters.flatMap( l => Iterator.continually(l.toByte).take(data(l).size) ).toArray
        val n = labelsArray.size
        val ret = new Mat(n,1,CvType.CV_8SC1)
        for( r <- 0 until n ){
          ret.put(r,0,Array(labelsArray(r)))
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
    val iterations = ann.train( input, labels, weights )
  }
}
