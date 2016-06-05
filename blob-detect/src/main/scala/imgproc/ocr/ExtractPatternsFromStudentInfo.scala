package imgproc.ocr

import java.io.File
import javax.imageio.ImageIO

import imgproc.ocr.OneLetterOCR._
import imgproc.steps.AnswersInfo.cells
import imgproc.steps.ProcessingStep
import org.opencv.core.Mat

/**
 * Created by alvaro on 16/04/16.
 */
object ExtractPatternsFromStudentInfo extends App{

  nu.pattern.OpenCV.loadLibrary()


  val baseDir = new File("blob-detect/src/main/resources/training-models/")
  val inDir = new File(baseDir,"studentInfo")
  val outDir = new File(baseDir,"to-clasify")

  val studentInfoFiles = inDir.listFiles()


  private def cellsOfImage(f: File) = {
    import imgproc.Implicits._
    import imgproc.steps.ProcessingStep.Implicits._

    val img : Mat = ImageIO.read(f)
    val info = ProcessingStep.cellsOfAnswerMatrix.process(img)

    if( info(cells).isEmpty ){
      println( s"Not valid: $f")
    }

    info(cells)
  }

  def saveImage(name: String, m: Mat) = {
    import imgproc.Implicits._
    assert( name.takeRight(4).head == '.' )
    val format = name.takeRight(3).toLowerCase
    val f = new File(outDir, name)
    f.getParentFile.mkdirs()
    ImageIO.write(m,format, f)

  }

  object counter{
    private var c = 0
    def next() = {
      c += 1
      c
    }
  }

  for( f <- studentInfoFiles ; maybeCells <- cellsOfImage(f) ; c <- maybeCells ){
    val candidates = extractPossibleLettersImage(c).map( Pattern.resizeToPatterSize )
    for( candidate <- candidates ) {
      saveImage(s"${counter.next}-${f.getName}", candidate)
    }

  }

}
