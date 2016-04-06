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



  def layerSizes( outputNodes: Int, nodesInInternalLayers: Int = 50, internalLayers: Int = 2 ) = {
    val m = new Mat(2+internalLayers, 1, CvType.CV_32SC1)
    m.put( 0, 0, Pattern.patternSize*Pattern.patternSize ) // INPUT LAYER
    for( l <- 0 until internalLayers ) {
      m.put(1+l, 0, nodesInInternalLayers)
    }
    m.put( 1+internalLayers, 0, outputNodes ) // OUTPUT LAYER (A,B,C,D)
    m
  }

  def patternToInputData( pattern: Mat ) : Array[Int] = ???

  def inputData( data: Map[Char,Seq[Mat]] ) : Mat = ???
  def outputData( data: Map[Char,Seq[Mat]] ) : Mat = ???

  def train( data: Map[Char,Seq[Mat]], alpha: Double,  beta: Double, activateFunc : Int = CvANN_MLP.SIGMOID_SYM ) = {

    val ann = new CvANN_MLP
    ann.create(layerSizes(data.size), activateFunc, alpha, beta)

    val input = inputData( data )
    val output = outputData( data )
    val weights : Mat = ???
    ann.train( input, output, weights )
  }
}
