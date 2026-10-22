package webserver.detector

import imgproc.steps.AnswersInfo.{answers, cellsLocation}
import imgproc.steps.ProcessingStep
import imgproc.steps.QRInfo.{qrText, qrVersion}
import org.opencv.core.Mat
import webserver.spi.{DetectedQuestionnaire, QuestionnaireDetector}

/**
  * Detector based on the blob-detect project.
  *
  * It runs the very same processing chain used by the blob-detect GUI, but it stops at the
  * "Localización de celdas (basada en columnas)" step (cellsLocationStep). That step is only
  * able to locate the answer cells when the QR code has been decoded and the answer matrix
  * measures are known, so a successful location of the cells means that a valid questionnaire
  * has been found.
  */
class BlobDetectQuestionnaireDetector extends QuestionnaireDetector {

  override def name: String = "blob-detect"

  override def detect(image: Mat): Option[DetectedQuestionnaire] = {
    import imgproc.steps.ProcessingStep.Implicits._

    val info = ProcessingStep.cellsLocationStep.process(image)

    info(cellsLocation).flatMap { locatedCells =>
      Some(
        DetectedQuestionnaire(
          image = image,
          qrText = info(qrText),
          version = info(qrVersion).map(_.toInt),
          questions = info(answers).map(_.size),
          cells = locatedCells.size
        )
      )
    }
  }

}
