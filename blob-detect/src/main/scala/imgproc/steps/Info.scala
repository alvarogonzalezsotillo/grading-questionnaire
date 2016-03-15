package imgproc.steps

import org.opencv.core.{Rect, MatOfPoint, Mat}


trait OriginalMatInfo {
  val originalMat: Mat
  val thresholdMat: Mat
  val cleanedMat: Mat
}

trait LocationInfo {
  val location: Option[MatOfPoint]
  val locatedMat: Option[Mat]
}

trait ContoursInfo {
  val contours: Seq[MatOfPoint]
  val quadrilaterals: Seq[MatOfPoint]
  val biggestQuadrilaterals: Seq[MatOfPoint]
}

trait QRLocationInfo {
  val qrLocation: Option[MatOfPoint]
  val qrLocatedMat: Option[Mat]
}

trait QRInfo {
  val qrText: Option[String]
}

trait AnswersInfo {
  val answers: Option[Seq[Int]]
  val cellsRect: Option[Seq[Rect]]
  val cells: Option[Seq[Mat]]
  val cleanedCells: Option[Seq[Mat]]
}

trait StudentInfo{
  val studentInfoLocation : Option[MatOfPoint]
  val studentInfoMat : Option[Mat]
}

case class Info(mat: Option[Mat], originalMat: Mat = null, cleanedMat: Mat = null, thresholdMat: Mat = null, contours: Seq[MatOfPoint] = null,
                quadrilaterals: Seq[MatOfPoint] = null, biggestQuadrilaterals: Seq[MatOfPoint] = null,
                location: Option[MatOfPoint] = None, locatedMat: Option[Mat] = None, qrLocatedMat: Option[Mat] = None, qrLocation: Option[MatOfPoint] = None,
                qrText: Option[String] = None, answers: Option[Seq[Int]] = None, cellsRect: Option[Seq[Rect]] = None, cells: Option[Seq[Mat]] = None,
                 cleanedCells: Option[Seq[Mat]] = None,  studentInfoLocation: Option[MatOfPoint] = None, studentInfoMat: Option[Mat] = None)
  extends OriginalMatInfo with LocationInfo with ContoursInfo with QRLocationInfo with QRInfo with AnswersInfo with StudentInfo{
}
