package imgproc


import imgproc.ocr.OneLetterOCR.{LetterProb, LetterResult}
import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr.perceptron.{LetterPerceptron, Perceptron}
import imgproc.ocr.perceptron.LetterPerceptron.LetterPerceptronParams
import imgproc.ocr.{DefaultTrainedOneLetterOCR, OneLetterOCR, Pattern}
import org.opencv.core.Mat

import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

/**
  * Created by alvaro on 13/07/17.
  */
object FindBestNeuralNetworkApp extends App{

  nu.pattern.OpenCV.loadLibrary()

  val internalLayersRange = 1 to 4
  val internalLayerNodesRange = Pattern.patternSize to Pattern.patternSize*8 by 10



  // Map[Char,Seq[Mat]]
  val allPatterns: TrainingPatterns = Pattern.letterTrainingPatterns

  val random = new Random

  def accuracyOfOCR(ocr: Perceptron )( trainPercentage: Double = 0.9) = {

    val chars = allPatterns.keySet

    val (trainingPatterns,testPatterns) = {

      def shuffleAndSplit(patterns: Seq[Mat]) = {
        val index = (patterns.length * trainPercentage).toInt
        random.shuffle(patterns).splitAt(index)
      }

      val patterns = chars.map { c =>
        val p = shuffleAndSplit(allPatterns(c))
        (c, p._1, p._2)
      }

      val trainingPatterns = patterns.map { case (c, p, _) => (c, p) }.toMap
      val testPatterns = patterns.map { case (c, _, p) => (c, p) }.toMap
      (trainingPatterns,testPatterns)
    }

    val normalizedTrainingPatterns = OneLetterOCR.normalizeTrainingPatterns(trainingPatterns)

    ocr.train(normalizedTrainingPatterns)

    val statistics = for( c <- chars  ) yield{
      val toTest = testPatterns(c)
      val success = toTest.map( p => ocr.predict( OneLetterOCR.normalizeLetter(p) ).prediction.getOrElse('#') == c ).count(p=>p)
      success.toDouble / toTest.length
    }

    statistics.sum / statistics.size

  }

  def average(times: Int)(proc : => Double )  = {
    val probes = (1 to times).map(_ => proc)
    (probes, probes.sum / times)
  }

  val accuracies = for(l <- internalLayersRange; n <- internalLayerNodesRange.par ) yield{
    val params = LetterPerceptronParams(n,l)
    val ocr = LetterPerceptron(params)
    val (probes,accuracy) = average(10){
      accuracyOfOCR(ocr)()
    }

    println( s"Params:$params  accuracy:$accuracy  probes:$probes")

    (accuracy,params)
  }

  for( (a,LetterPerceptronParams(nodes,layers,_)) <- accuracies ){
    println( s"$nodes\t $layers\t $a" )
  }

  val best = accuracies.maxBy{case (a,_) => a}


  println( "best: " + best )

}
