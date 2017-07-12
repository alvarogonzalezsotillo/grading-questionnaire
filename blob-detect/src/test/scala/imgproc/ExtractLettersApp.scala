package imgproc

import java.awt.Image
import java.io.File
import javax.imageio.ImageIO

import common.TestUtil.saveDerivedTestImage
import imgproc.ocr.OneLetterOCR
import imgproc.steps.AnswersInfo.cells
import imgproc.steps.MainInfo.originalMat
import imgproc.steps.ProcessingStep.cellsStep

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Try

/**
  * Created by alvaro on 12/07/17.
  */
object ExtractLettersApp extends App {

  val path = "/home/alvaro/github/grading-questionnaire/blob-detect/samples"

  private def log(s: => String) {
    println(s)
  }

  val files = {

    def descendantFiles(f: File): Stream[File] = {
      Option(f.listFiles()) match {
        case Some(files) => files.toStream.flatMap(fi => fi #:: descendantFiles(fi))
        case None => Stream.empty
      }
    }

    descendantFiles(new File(path))
  }

  case class Sample(file: File) {
    val image = Try(ImageIO.read(file))
    val isOk = image.isSuccess && image.get != null
  }

  val samples = files.
    map(f => Sample(f)).
    filter(_.isOk)

  nu.pattern.OpenCV.loadLibrary()



  def processSample( s: Sample ) = {
    import imgproc.Implicits._
    import imgproc.steps.ProcessingStep.Implicits._
    val info = cellsStep.process(BufferedImage2Mat(s.image.get))
    log(s"$s: ${info(cells)}")
    for (cellsMat <- info(cells)) {
      saveDerivedTestImage(s.file.toString, "original", originalMat(info), "ExtractLettersApp")
      for ((mat, index) <- cellsMat.zipWithIndex; (l, i) <- OneLetterOCR.extractPossibleLettersImage(mat).zipWithIndex) {
        val normalized = OneLetterOCR.normalizeLetter(l)
        saveDerivedTestImage(s.file.toString, s"cell-${index + 1}-letter-$i", l, "ExtractLettersApp")
        saveDerivedTestImage(s.file.toString, s"cell-${index + 1}-letter-$i-normalized", normalized, "ExtractLettersApp")
      }
    }
  }

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  val futures = samples.map( s => Future(processSample(s) ) )

  Await.result( Future.sequence(futures), Duration.Inf )

  println( "end")
}
