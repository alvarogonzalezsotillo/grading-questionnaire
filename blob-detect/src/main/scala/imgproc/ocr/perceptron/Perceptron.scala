package imgproc.ocr.perceptron

import java.util

import com.typesafe.scalalogging.slf4j.LazyLogging
import imgproc.ocr.OneLetterOCR.{LetterProb, LetterResult}
import imgproc.ocr.Pattern
import imgproc.ocr.Pattern.TrainingPatterns
import org.opencv.core.{CvType, Mat, TermCriteria}
import org.opencv.ml.{CvANN_MLP, CvANN_MLP_TrainParams}

/**
 * Created by alvaro on 6/04/16.
 * http://stackoverflow.com/questions/34246265/opencv-ann-mlp-training-in-android
 * http://stackoverflow.com/questions/17774063/opencv-error-bad-argument-in-cvann-mlp
 * http://projectagv.blogspot.com.es/2008/12/sample-code-for-cvannmlp.html
 * http://docs.opencv.org/2.4/doc/tutorials/ml/introduction_to_svm/introduction_to_svm.html
 */

object Perceptron{

  def imageToSignal(i: Byte ) : Float = 1.0f*i/255
  def signalToImage(s: Float ) : Byte = (s*255).toByte


  private def fillRowWithArray( m: Mat, row: Int, b: Array[Float] ){
    assert(m.cols() == b.size )
    val buffer = new Array[Float](1)
    for( i <- 0 until m.cols() ){
      buffer(0) = b(i)
      m.put(row,i,buffer)
    }
  }

}

abstract class Perceptron( val nodesInInputLayer : Int, nodesInInternalLayers: Int, internalLayers: Int = 2) extends LazyLogging{

  import Perceptron._


  private def layerSizes( outputNodes: Int ) = {
    val m = new Mat(2+internalLayers, 1, CvType.CV_32SC1)
    m.put( 0, 0, nodesInInputLayer ) // INPUT LAYER
    for( l <- 0 until internalLayers ) {
      m.put(1+l, 0, nodesInInternalLayers)
    }
    m.put( 1+internalLayers, 0, outputNodes ) // OUTPUT LAYER (A,B,C,D)
    m
  }

  protected def patternToInputData( pattern: Mat ) : Array[Float]

  val ann = new CvANN_MLP
  var characters : IndexedSeq[Char] = null

  def labelOfLetter( le: Char ) : Int = characters.indexOf(le)
  def letterOfLabel( la: Int ) : Char = characters(la)


  def train( data: TrainingPatterns, alpha: Double = 1,  beta: Double = 1, activateFunc : Int = CvANN_MLP.SIGMOID_SYM) : Int = {
    val letters = data.keys.toList

    logger.error( s"data: ${data}" )

    characters = letters.toIndexedSeq

    def trainDataset : (Mat,Mat,Mat) = {

      val labels = {
        val labelsArray = letters.flatMap{ l =>
          val label = labelOfLetter(l)
          val size = data(l).size
          Iterator.continually(label).take(size)
        }.toArray
        val ret = new Mat(labelsArray.size, letters.size,CvType.CV_32FC1)
        val buffer = new Array[Float](letters.size)
        for( r <- 0 until labelsArray.size ){
          util.Arrays.fill(buffer,-1f)
          buffer(labelsArray(r)) = 1f
          ret.put(r,0,buffer)
        }
        ret
      }

      val weights = {
        val max = data.values.map(_.size).max
        val total = data.values.map(_.size).sum
        val weights = new Mat(total,1,CvType.CV_32FC1)
        val array = letters.flatMap{ letter =>
          val size = data(letter).size
          val w = 1.0f*size/total
          Iterator.continually(w).take(size)
        }.toArray
        val buffer = new Array[Float](1)
        for( r <- 0 until array.size){
          buffer(0) = array(r)
          weights.put(r,0,buffer)
        }
        weights
      }

      val input = {
        val patterns = letters.flatMap( l => data(l).toList ).toArray
        val n = patterns.size
        val ret = new Mat(n,nodesInInputLayer,CvType.CV_32FC1)
        for( r <- 0 until n ; inputData = patternToInputData(patterns(r)) ){
          fillRowWithArray(ret,r,inputData)
        }
        ret
      }

      logger.error( s"trainData: input: ${input.size()} labels: ${labels.size()} weights: ${weights.size()}")

      (input,labels,weights)
    }



    ann.create(layerSizes(data.size), activateFunc, alpha, beta)

    val (input,labels,weights) = trainDataset
    val params = {
      // https://github.com/arnaudgelas/OpenCVExamples/blob/master/NeuralNetwork/NeuralNetwork.cpp
      val ret = new CvANN_MLP_TrainParams
      ret.set_train_method(CvANN_MLP_TrainParams.BACKPROP)
      val maxIterations = 1000
      val epsilon = 0.0001
      val termCriteria = new TermCriteria(TermCriteria.MAX_ITER+TermCriteria.EPS,maxIterations,epsilon)
      ret.set_term_crit( termCriteria )
      ret
    }

    val flags = 0// CvANN_MLP.NO_INPUT_SCALE | CvANN_MLP.NO_OUTPUT_SCALE

    ann.train( input, labels, weights, new Mat(), params, flags )
  }


  /*
  If you are using the default cvANN_MLP::SIGMOID_SYM activation function with
   the default parameter values fparam1=0 and fparam2=0 then the function used is y = 1.7159*tanh(2/3 * x),
   so the output will range from [-1.7159, 1.7159], instead of [0,1].

   BUT I GET -0.0039,0.0039, DONT KNOW WHY
   */
  def normalizeProbability(d: Double) = {
    val min = -0.0041
    val max = -min
    val norm = (d - min)/(max-min)
    val ret = (norm min 1) max 0
    ret
  }

  def trained = characters != null

  def predict( pattern: Mat ) : LetterResult = {
    assert( trained )
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
      LetterProb(l, prob )
    }
  }
}

class LetterPerceptron( nodesInInternalLayers: Int = Pattern.patternSize*4, internalLayers: Int = 2, patternSize : Int = Pattern.patternSize ) extends Perceptron(patternSize*patternSize,nodesInInternalLayers,internalLayers){

  import Perceptron._

  protected def patternToInputData( pattern: Mat ) : Array[Float] = {
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

}