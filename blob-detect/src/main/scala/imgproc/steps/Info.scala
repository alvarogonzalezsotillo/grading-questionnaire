package imgproc.steps

import org.opencv.core.{Rect, MatOfPoint, Mat}
import common.HMap




object GrayscaleInfo{
  object thresholdMat  extends HKey[Mat]
  object cleanedMat  extends HKey[Mat]
}

object LocationInfo {
  object location extends HKey[MatOfPoint]
  object locatedMat extends HKey[Mat]
  object locatedCellHeaders 
}

object ContoursInfo {
  object contours extends HKey[MatOfPoint]
  object quadrilaterals extends HKeySeq[Seq[MatOfPoint]]
  object biggestQuadrilaterals extends HKey[IndexedSeq[MatOfPoint]]
}

object QRInfo {
  object qrLocation extends HKey[MatOfPoint]
  object qrLocatedMat extends HKey[Mat]
  object qrText extends HKey[String]
}

object AnswersInfo {
  object answers extends HKey[Seq[Int]]
  object cellsRect extends HKey[Seq[MatOfPoint]]
  object cells extends HKey[Seq[Mat]]
  object cleanedCells extends HKey[Seq[Mat]]
}

object StudentInfo{
  object studentInfoLocation extends HKey[MatOfPoint]
  object studentInfoMat extends HKey[Mat]
}

