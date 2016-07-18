package imgproc.steps

import org.opencv.core.{Mat, MatOfPoint, Rect}
import common.{HKey, HMap}
import imgproc.{AnswerMatrixMeasures, AnswerMatrixMeasuresHorizontalTicked}


object MainInfo{
  object mat extends HKey[Mat]
  object originalMat  extends HKey[Mat]
}

object GrayscaleInfo{
  object thresholdMat  extends HKey[Mat]
  object cleanedMat  extends HKey[Mat]
}

object LocationInfo {
  object location extends HKey[MatOfPoint]
  object locatedMat extends HKey[Mat]
  object locatedCellHeaders extends HKey[IndexedSeq[MatOfPoint]]
}

object ContoursInfo {
  object contours extends HKey[Seq[MatOfPoint]]
  object quadrilaterals extends HKey[Seq[MatOfPoint]]
  object biggestQuadrilaterals extends HKey[IndexedSeq[MatOfPoint]]
  object answerColumns extends HKey[IndexedSeq[MatOfPoint]]
  object cellsLocation extends HKey[IndexedSeq[MatOfPoint]]
}

object QRInfo {
  object qrLocation extends HKey[MatOfPoint]
  object qrLocatedMat extends HKey[Mat]
  object qrText extends HKey[String]
  object qrVersion extends HKey[Byte]
  object answerMatrixMeasures extends HKey[AnswerMatrixMeasuresHorizontalTicked]
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

