package imgproc

import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr.perceptron.LetterPerceptron.AnnPerceptronParams
import imgproc.ocr.perceptron.{LetterPerceptron, UntrainedAnnPerceptron}
import imgproc.ocr.{OneLetterOCR, Pattern}
import org.opencv.core.Mat

import scala.collection.immutable.IndexedSeq
import scala.util.Random

/**
  * Created by alvaro on 13/07/17.
  */
object FindBestNeuralNetworkApp extends App{

  imgproc.OpenCVLib.loadLibrary()

  val internalLayersRange = 1 to 10
  val maxIterationsRange = Seq(100,1000)
  val epsilonRange = Seq(0.05, 0.01)
  val internalLayerNodesRange = Pattern.patternSize to Pattern.patternSize*Pattern.patternSize by Pattern.patternSize*4



  // Map[Char,Seq[Mat]]
  val allPatterns: TrainingPatterns = Pattern.letterTrainingPatterns

  val random = new Random

  def accuracyOfOCR(params: AnnPerceptronParams, trainPercentage: Double = 0.9) : Double = {

    val ocr = LetterPerceptron(params)

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

    println( s"   $params: Training start")
    ocr.train(normalizedTrainingPatterns)
    println( s"   $params: Training end")



    def statistics( patterns: TrainingPatterns ) = for( c <- chars  ) yield{
      val toTest = patterns(c)
      val success = toTest.map( p => ocr.predict( OneLetterOCR.normalizeLetter(p) ).prediction.getOrElse('#') == c ).count(p=>p)
      success.toDouble / toTest.length
    }

    val stTest = statistics(testPatterns)
    println( s"   $params: stTest:$stTest")

    stTest.sum / stTest.size

  }

  def average[T: Fractional](times: Int)(proc : => T ): (IndexedSeq[T], T) = {
    implicit val tOps = implicitly[Fractional[T]].mkNumericOps _
    val probes = (1 to times).map(_ => proc)
    (probes, probes.sum / implicitly[Fractional[T]].fromInt(times))
  }

  val accuracies = for(l <- internalLayersRange.par ; n <- internalLayerNodesRange; maxIterations <- maxIterationsRange ; epsilon <- epsilonRange ) yield{
    val params = AnnPerceptronParams(n,l,maxIterations,epsilon)
    val (_,accuracy) = average(1){
      //println( s"For average: $params")
      accuracyOfOCR(params)
    }

    println( s"** Params:$params  accuracy:$accuracy")
nternalLayerNodesRange
    (accuracy,params)
  }

  for( (a,AnnPerceptronParams(nodes,layers,maxIterations,epsilon,_)) <- accuracies ){
    println( s"nodes:$nodes\t layers:$layers\t iterations:$maxIterations\t epsilon:$epsilon\t  $a" )
  }

  val best = accuracies.maxBy{case (a,_) => a}


  println( "best: " + best )

}
