package imgproc.steps

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import com.typesafe.scalalogging.LazyLogging
import common.{BinaryConverter, HMap, Sounds}
import imgproc.steps.AnswersInfo.{cells, cellsLocation, studentAnswers}
import imgproc.steps.ContoursInfo.{answerColumns, biggestQuadrilaterals}
import imgproc.steps.MainInfo._
import imgproc.steps.ProcessingStep.{ExtendedStep, Info}
import imgproc.steps.QRInfo.{answerMatrixMeasures, qrVersion}
import imgproc.{AnswerMatrixMeasures, ImageProcessing, QRScanner}
import org.opencv.core.{MatOfPoint, _}
import org.opencv.highgui.Highgui


/**
  * Created by alvaro on 13/11/15.
  */


trait ProcessingStep {


  def process(implicit i: Info): Info

  val stepName: String

  def extend(name: String)(p: Info => Info): ProcessingStep = ExtendedStep(this, name, p)

  def withDrawContours(extractContours: Info => Option[Seq[MatOfPoint]]): ProcessingStep = extend(stepName) { p =>

    val matWithContours = for (m <- p(mat); contours <- extractContours(p)) yield {
      val newM = m.clone
      ImageProcessing.drawContours(newM, contours)
    }
    p(mat, matWithContours)
  }

  def withDrawNumberedContours(extractContours: Info => Option[Seq[MatOfPoint]]): ProcessingStep = extend(stepName) { p =>

    import imgproc.Implicits._

    val matWithContours = for (m <- p(mat); contours <- extractContours(p)) yield {
      val newM = m.clone
      ImageProcessing.drawContours(newM, contours)

      for ((c, i) <- contours.zipWithIndex) {
        ImageProcessing.drawString(newM, s"$i", c.center)
        for (pairs <- c.toArray.grouped(2)) {
          val p = pairs.head
          ImageProcessing.drawString(newM, s"$p", p, new Scalar(255, 0, 0))
        }
      }

      newM
    }
    p(mat, matWithContours)
  }


  def withDrawString(extractString: Info => Option[String]): ProcessingStep = extend(stepName) { p =>

    val matWithString = for (m <- p(mat); s <- extractString(p)) yield {
      val newM = m.clone()
      ImageProcessing.drawString(newM, s, new Point(10, 10))
    }
    p(mat, matWithString)
  }

  def withFilter(name: String = "Filtrado:" + stepName, delay: Int = 2500)(accept: Info => Boolean): ProcessingStep = {
    var lastAccept = System.currentTimeMillis()
    var lastInfo: Info = HMap()
    extend(name) { psi =>
      if (System.currentTimeMillis() > lastAccept + delay && accept(psi)) {
        lastAccept = System.currentTimeMillis()
        lastInfo = psi
      }
      lastInfo
    }
  }

  def withSaveMatrix(name: String = "Grabando:" + stepName): ProcessingStep = {
    var lastInfo: Info = HMap()
    extend(name) { psi =>
      if (!(psi eq lastInfo)) {
        lastInfo = psi
        for (m <- lastInfo(mat)) {
          ProcessingStep.saveMatrix(m)
        }
        for (m <- lastInfo(originalMat)) {
          ProcessingStep.saveMatrix(m, "-original")
        }
        Sounds.beep()
      }
      psi
    }
  }

}


object ProcessingStep extends LazyLogging {

  import imgproc.ImageProcessing._
  import imgproc.Implicits._


  type Info = HMap


  private def combined(steps: ProcessingStep*) = {
    steps.tail.foldLeft(steps.head)((ret, step) => ret.extend(ret.stepName + " - " + step.stepName)(step.process(_)))
  }

  private case class Step(override val stepName: String)(val proc: Info => Info) extends ProcessingStep {
    override def process(implicit i: Info): Info = proc(i)
  }

  private case class ExtendedStep(previous: ProcessingStep, override val stepName: String, extendedProcess: Info => Info) extends ProcessingStep {
    def process(implicit src: Info) = extendedProcess(previous.process(src))
  }

  object Implicits {
    implicit def infoFromMat(m: Mat): Info = HMap()(mat, m)(originalMat, m)
  }


  private def saveMatrix(m: Mat, suffix: String = "") {
    val shortDateFormat = new SimpleDateFormat("yyyyMMdd")
    val longDateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val date = new Date
    val lDate = longDateFormat.format(date)
    val sDate = shortDateFormat.format(date)
    new File(sDate).mkdirs
    val file = s"$sDate/$lDate$suffix.png"
    Highgui.imwrite(file, m)
  }


  object initialStep extends ProcessingStep {
    override def process(implicit m: Info) = m

    override val stepName = "Imagen original"
  }


  val resizeStep = initialStep.extend( "Tamaño normalizado" ){ implicit info =>
    val requiredWidth = 1280 // MAX HORIZONTAL RESOLUTION OF MY WEBCAM
    val m = originalMat()
    val h = (1.0*m.height()*requiredWidth/m.width()).toInt
    val resizedMat = ImageProcessing.stretchImage()(m,requiredWidth,h)
    info( originalMat, resizedMat )(mat, resizedMat)
  }


  val thresholdStep = resizeStep.extend("Umbral adaptativo") { implicit psi =>
    import GrayscaleInfo._
    val t = threshold()(originalMat())
    psi(thresholdMat, t)(mat, t)
  }

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)") { implicit psi =>
    import GrayscaleInfo._
    val cleaned = clean()()(thresholdMat())
    psi(cleanedMat, cleaned)(mat, cleaned)
  }

  val contourStep = noiseReductionStep.extend("Búsqueda de contornos") { implicit psi =>
    import ContoursInfo._
    import GrayscaleInfo._
    val c = findContours(cleanedMat())
    psi(contours, c)(mat, originalMat())
  }

  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros") { implicit csi =>
    import ContoursInfo._
    val newContours = approximateContoursToQuadrilaterals()(contours())
    csi(quadrilaterals, newContours)
  }

  val COLUMNS = 5

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros") { implicit csi =>
    import ContoursInfo._

    def findBiggestAlignedQuadrilaterals(number: Int = COLUMNS)(contours: Seq[MatOfPoint]): Option[IndexedSeq[MatOfPoint]] = {
      val ordered = contours.sortBy(_.area).reverse

      def similarQuadrilaterals(quad: MatOfPoint) = {
        implicit val epsilon = Epsilon(quad.area * 0.25)
        contours.filter(_.area ~= quad.area)
      }

      if (false) {
        println("Similar quadrilaterals:")
        println(" area:" + ordered.map(_.area).mkString(", "))
      }

      val ret = ordered.view.map(similarQuadrilaterals).filter(_.size == number).headOption
      ret.map(_.sortBy(_.boundingBox.minX).toIndexedSeq)
    }


    val quads = findBiggestAlignedQuadrilaterals()(quadrilaterals()).map { quads =>
      val sortedQuads = quads.sortBy(_.center.x)
      val orientation = sortedQuads.last.center - sortedQuads.head.center
      sortedQuads.map(q => toMatOfPoint(findProbableQuadrilateral(q.points, orientation)))
    }


    csi(biggestQuadrilaterals, quads)
  }

  private def findProbableQuadrilateral(points: Seq[Point], orientation: Point): Seq[Point] = {
    val center = new Shape(points).center
    val unit = orientation.normalize
    val diffs = points.map(_ - center)
    val (upperLeft, upperRight) = {
      val upperPoints = diffs.filter(_.crossProductZ(unit) > 0)
      (upperPoints.minBy(_.normalize * unit), upperPoints.maxBy(_.normalize * unit))
    }
    val (lowerLeft, lowerRight) = {
      val lowerPoints = diffs.filter(_.crossProductZ(unit) < 0)
      (lowerPoints.minBy(_.normalize * unit), lowerPoints.maxBy(_.normalize * unit))
    }
    Seq(upperLeft, upperRight, lowerRight, lowerLeft).map(_ + center)
  }


  val locateQRStep = biggestQuadrilateralsStep.extend("Localización del código QR") { psi =>
    import QRInfo._

    def locateQR(cellHeaders: Seq[MatOfPoint]): MatOfPoint = {
      val tl = cellHeaders(0)(0)
      val tr = cellHeaders(COLUMNS - 1)(1)
      val xAxis = (tr - tl)
      val yAxis = new Point(-xAxis.y, xAxis.x)

      val topLeft = tl -
        (yAxis * AnswerMatrixMeasures.matrixWithToTopOfQRRatio) -
        (xAxis * AnswerMatrixMeasures.matrixWithToLeftOfQRRatio)
      val topRight = topLeft + (xAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
      val bottomLeft = topLeft + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
      val bottomRight = topRight + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)

      new MatOfPoint(topLeft, topRight, bottomRight, bottomLeft)
    }

    psi(qrLocation, psi(biggestQuadrilaterals).map(locateQR))
  }

  val extractQRStep = locateQRStep.extend("Extracción del código QR") { implicit psi =>
    import QRInfo._
    def compute(rect: MatOfPoint) = {
      val dstPoints = new MatOfPoint((0.0, 0.0), (150.0, 0.0), (150.0, 150.0), (0.0, 150.0))

      val h = findHomography(rect, dstPoints)
      warpImage()(originalMat(), h, new Size(150, 150))
    }

    val m = psi(qrLocation).map(compute)
    psi(mat, m)(qrLocatedMat, m)
  }


  val decodeQRStep = extractQRStep.extend("Decodificación del código QR") { psi =>
    import QRInfo._
    psi(qrText, psi(qrLocatedMat).flatMap(QRScanner.decode))
  }

  val informationOfQRStep = decodeQRStep.extend("Información del código QR") { psi =>
    import AnswersInfo._
    import QRInfo._
    def compute(s: String) = {
      val data = BinaryConverter.fromBase64(s)
      BinaryConverter.fromBinarySolutions(data)
    }

    psi(qrText).map { s =>
      val (ans, v) = compute(s)
      val measures = AnswerMatrixMeasures(Some(ans.size), v)
      psi(answers, ans)(qrVersion, v)(answerMatrixMeasures, measures)
    }.getOrElse(psi)

  }


  def interpolateInLine(p1: Point, p2: Point, currentDistance: Double, requiredDistance: Double) = {
    val vector = p2 - p1
    val factor = vector.modulus * requiredDistance / currentDistance
    p1 + (vector.normalize * factor)
  }

  val answerColumnsStep = informationOfQRStep.extend("Localización de las columnas de respuestas") { implicit psi =>
    import ContoursInfo._


    val columns = for (quads <- psi(biggestQuadrilaterals); measures <- psi(answerMatrixMeasures)) yield {
      val nColumns = quads.size
      val definedColumns = for (i <- 0 until nColumns - 1) yield {
        val prev = quads(i)
        val next = quads(i + 1)

        val tl = prev(1)
        val tr = next(0)
        val br = next(3)
        val bl = prev(2)

        import measures.params._
        val correctedTL = interpolateInLine(tl, tr, answerCellAvailableWidth.w, cellHeaderToCellWidthGap.w)
        val correctedBL = interpolateInLine(bl, br, answerCellAvailableWidth.w, cellHeaderToCellWidthGap.w)
        val correctedTR = interpolateInLine(tl, tr, answerCellAvailableWidth.w, cellHeaderToCellWidthGap.w + cellSize.w.w)
        val correctedBR = interpolateInLine(bl, br, answerCellAvailableWidth.w, cellHeaderToCellWidthGap.w + cellSize.w.w)

        new MatOfPoint(correctedTL, correctedTR, correctedBR, correctedBL)
      }

      val lastColumn = {
        val lastCellHeaders = quads(nColumns - 1)
        val tl = lastCellHeaders(0)
        val tr = lastCellHeaders(1)
        val br = lastCellHeaders(2)
        val bl = lastCellHeaders(3)

        import measures.params._
        val correctedTL = interpolateInLine(tl, tr, cellHeaderSize.w.w, (cellHeaderSize.w + cellHeaderToCellWidthGap).w)
        val correctedBL = interpolateInLine(bl, br, cellHeaderSize.w.w, (cellHeaderSize.w + cellHeaderToCellWidthGap).w)
        val correctedTR = interpolateInLine(tl, tr, cellHeaderSize.w.w, (cellHeaderSize.w + cellHeaderToCellWidthGap + cellSize.w).w)
        val correctedBR = interpolateInLine(bl, br, cellHeaderSize.w.w, (cellHeaderSize.w + cellHeaderToCellWidthGap + cellSize.w).w)

        new MatOfPoint(correctedTL, correctedTR, correctedBR, correctedBL)
      }

      definedColumns ++ Seq(lastColumn)
    }

    psi(answerColumns, columns)(mat, psi(originalMat))
  }


  val cellsLocationStep = answerColumnsStep.extend("Localización de celdas (basada en columnas)") { psi =>
    import imgproc.steps.AnswersInfo._

    val ret = for (cellHeaderColumns <- psi(answerColumns);
                   measures <- psi(answerMatrixMeasures);
                   rows = measures.rows;
                   version <- psi(qrVersion);
                   answers <- psi(answers);
                   questions = answers.size) yield {
      for (question <- 0 until questions) yield {
        val row = measures.rowOfQuestion(question)
        val column = measures.columnOfQuestion(question)
        val rect = cellHeaderColumns(column)
        val tl = interpolateInLine(rect(0), rect(3), rows, row)
        val tr = interpolateInLine(rect(1), rect(2), rows, row)
        val bl = interpolateInLine(rect(0), rect(3), rows, row + 1)
        val br = interpolateInLine(rect(1), rect(2), rows, row + 1)

        new MatOfPoint(tl, tr, br, bl)

      }
    }

    psi(cellsLocation, ret.get)
  }

  val cellsStep = cellsLocationStep.extend("Celdas individuales") { info =>

    val ret = {
      for (measures <- info(answerMatrixMeasures);
           cellsLoc <- info(cellsLocation);
           mat <- info(originalMat)) yield {
        val cellSize = measures.params.cellSize
        for ((cellInMatrix, cell) <- cellsLoc.zip(measures.answerCells)) yield {
          val src = cellInMatrix
          val dst = cellSize.toRect.toOpenCV
          val h = ImageProcessing.findHomography(src, dst)
          ImageProcessing.warpImage()(mat, h, cellSize.toOpenCV)
        }
      }
    }

    info(cells, ret)
  }

  val studentAnswersStep = cellsStep.extend("Respuestas del alumno ") { info =>
    val ret = for (measures <- info(answerMatrixMeasures);
                   cellRecognizer = measures.cellCorrector;
                   cells <- info(cells)) yield {
      val r = for (cell <- cells) yield {
        cellRecognizer.recognize(cell)
      }
      r.zipWithIndex.map(_.toString)
    }

    info(studentAnswers,  ret)
  }

}

