package imgproc.steps

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import com.typesafe.scalalogging.slf4j.LazyLogging
import common.{BinaryConverter, HMap, Sounds}
import imgproc.steps.LocationInfo.location
import imgproc.steps.MainInfo._
import imgproc.steps.ProcessingStep.{ExtendedStep, Info}
import imgproc.steps.QRInfo.qrVersion
import imgproc.{AnswerMatrixMeasures, ImageProcessing, QRScanner}
import org.opencv.core._
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc

import scala.util.Try


/**
  * Created by alvaro on 13/11/15.
  */


trait ProcessingStep {


  def process(implicit i: Info): Info

  val stepName: String

  def extend[T](name: String)(p: Info => Info): ProcessingStep = ExtendedStep(this, name, p)

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

      for( (c,i)<-contours.zipWithIndex ) {
        ImageProcessing.drawString(newM, s"$i", c.center )
        for( pairs <- c.toArray.grouped(2) ){
          val p = pairs.head
          ImageProcessing.drawString(newM,s"$p", p, new Scalar(255,0,0) )
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


object ProcessingStep extends LazyLogging{

  import imgproc.ImageProcessing._
  import imgproc.Implicits._


  type Info = HMap


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

  val thresholdStep = initialStep.extend("Umbral adaptativo") { implicit psi =>
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

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros") { implicit csi =>
    import ContoursInfo._

    def findBiggestAlignedQuadrilaterals(number: Int = 5)(contours: Seq[MatOfPoint]): Option[IndexedSeq[MatOfPoint]] = {
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


    val quads = findBiggestAlignedQuadrilaterals()(quadrilaterals())
    csi(biggestQuadrilaterals, quads)
  }

  val answerMatrixLocationStep = biggestQuadrilateralsStep.extend("Localización de la tabla de respuestas") { implicit lsi =>
    import ContoursInfo._
    import LocationInfo._

    def locateAnswerMatrix(imageWidth: Int, imageHeight: Int, number: Int = 5, insideLimit: Int = -8)(contours: Seq[MatOfPoint], version: Byte): Option[MatOfPoint] =
    {
      import scala.collection.JavaConverters._

      val allPoints = contours.map(_.toList.asScala).flatten


      def doIt() = {
        val (center, orientation) = {
          val shapes = contours.map(c => new Shape(c))
          val leftmostCenter = shapes.map(_.center).minBy(_.x)
          val rightmostCenter = shapes.map(_.center).maxBy(_.x)

          logger.debug( s" leftmostCenter:$leftmostCenter rightmostCenter: $rightmostCenter")

          ((leftmostCenter + rightmostCenter) * 0.5, (rightmostCenter - leftmostCenter))
        }
        logger.debug( s" center:$center orientation: $orientation")

        val diffs = allPoints.map(_ - center)

        val unit = orientation.normalize

        val (upperLeft, upperRight) = {
          val upperPoints = diffs.filter(_.crossProductZ(unit) > 0)
          (upperPoints.minBy(_.normalize * unit), upperPoints.maxBy(_.normalize * unit))
        }
        logger.debug( s"upperLeft: ${upperLeft+center} upperRight:${upperRight+center}")

        val (lowerLeft, lowerRight) = {
          val lowerPoints = diffs.filter(_.crossProductZ(unit) < 0)
          (lowerPoints.minBy(_.normalize * unit), lowerPoints.maxBy(_.normalize * unit))
        }
        logger.debug( s"lowerLeft: ${lowerLeft+center} lowerRight:${lowerRight+center}")

        val extensionFactor = {
          val amm = AnswerMatrixMeasures(version)
          import amm.Params._
          val extension = answerCellAvailableWidth.w
          val cellHeaders = (cellHeaderSize.w.w)*(amm.columns) + extension*(amm.columns-1)
          extension/cellHeaders

        }

        val lowerExtension = (lowerRight - lowerLeft) * extensionFactor
        val upperExtension = (upperRight - upperLeft) * extensionFactor

        val ret = new MatOfPoint(
          upperLeft + center,
          upperRight + center + upperExtension,
          lowerRight + center + lowerExtension,
          lowerLeft + center
        )

        logger.debug( s"doit: $ret")

        ret
      }

      def checkIt(contour: MatOfPoint) = {

        def insideImage(p: Point) = {
          p.x > 0 && p.y > 0 && p.x < imageWidth && p.y < imageHeight
        }

        val contour2f = new MatOfPoint2f()
        contour.convertTo(contour2f, CvType.CV_32FC2)

        def insideContour(p: Point) = {
          val inside = Imgproc.pointPolygonTest(contour2f, p, true)
          inside > insideLimit
        }

        val upperLeft = contour.toArray()(0)
        val upperRight = contour.toArray()(1)
        val w = upperRight.x - upperLeft.x

        val contoursSizeOK = contours.size == number
        val insideImageOK = contour.toArray.forall(insideImage)
        val insideContourOK = allPoints.forall(insideContour)
        val widthOK = w > imageWidth * 0.60

        val ret = contoursSizeOK && insideImageOK && insideContourOK && widthOK

        logger.debug(s"$contoursSizeOK  $insideImageOK $insideContourOK $widthOK $w $imageWidth")

        ret
      }

      Try(doIt).filter(checkIt).toOption
    }

    val l = lsi(biggestQuadrilaterals).flatMap { biggestQuadrilaterals =>
      locateAnswerMatrix(originalMat().width(), originalMat().height())(biggestQuadrilaterals,1)
    }
    lsi(location, l)
  }


  val locateQRStep = answerMatrixLocationStep.extend("Localización del código QR") { psi =>
    import LocationInfo._
    import QRInfo._
    def locateQR(answerMatrixLocation: MatOfPoint): MatOfPoint = {
      val points = answerMatrixLocation.toArray
      val tl = points(0)
      val tr = points(1)
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

    psi(qrLocation, psi(location).map(locateQR))
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

    psi(qrText).map{ s =>
      val (ans,v) = compute(s)
      psi(answers, ans)(qrVersion,v)
    }.getOrElse(psi)

  }


  val answerMatrixStep = informationOfQRStep.extend("Extracción de la tabla de respuestas") { implicit psi =>

    import imgproc.steps.AnswersInfo._
    import imgproc.steps.ContoursInfo._
    import imgproc.steps.LocationInfo._
    import imgproc.steps.MainInfo._

    val loc = for (rect <- psi(location); ans <- psi(answers); biggestQuadrilaterals <- psi(biggestQuadrilaterals); version <- psi(qrVersion) ) yield {

      val dstPoints = AnswerMatrixMeasures(version).answerTableRect(ans.size).toOpenCV

      val h = findHomography(rect, dstPoints)
      val locatedMat = warpImage()(originalMat(), h, AnswerMatrixMeasures(version).answerTableRect(ans.size).size.toOpenCV)
      val locatedCellHeaders = warpContours(biggestQuadrilaterals, h)

      (locatedMat, locatedCellHeaders)
    }

    val am = loc.map(_._1)
    val lch = loc.map(_._2)

    val ret = psi(mat, am)(locatedMat, am)(locatedCellHeaders, lch)
    ret
  }



  val cellsOfAnswerMatrix = answerMatrixStep.extend("Localización de celdas") { psi =>
    import imgproc.steps.AnswersInfo._
    import imgproc.steps.LocationInfo._

    val ret = for (m <- psi(locatedMat); a <- psi(answers); version <- psi(qrVersion) ) yield {
      val cr = AnswerMatrixMeasures(version).answerCells(a.size)
      val c = for (r <- cr) yield submatrix(m, r.toOpenCV, AnswerMatrixMeasures(1).Params.cellSize.w.w, AnswerMatrixMeasures(1).Params.cellSize.h.h)
      psi(cellsRect, cr.map(_.toOpenCV))(cells, c)
    }
    ret.getOrElse(psi)
  }


}

