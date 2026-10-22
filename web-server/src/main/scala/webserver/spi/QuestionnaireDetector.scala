package webserver.spi

import org.opencv.core.Mat

/**
  * Information about a questionnaire that has been successfully located in an image.
  *
  * @param image     the image where the questionnaire was found
  * @param qrText    the raw contents of the QR code (it encodes the answer key and the questionnaire version)
  * @param version   the version of the questionnaire (see common.QuestionnaireVersion)
  * @param questions the number of questions of the questionnaire
  * @param cells     the number of answer cells that have been located
  */
case class DetectedQuestionnaire(
                                image: Mat,
                                qrText: Option[String],
                                version: Option[Int],
                                questions: Option[Int],
                                cells: Int
                                )

/**
  * SPI (Service Provider Interface) for the questionnaire detection.
  *
  * Implementations are discovered at runtime with java.util.ServiceLoader, so new detection
  * algorithms can be added just by dropping a jar with the implementation and its
  * META-INF/services/webserver.spi.QuestionnaireDetector file in the classpath.
  *
  * See webserver.detector.BlobDetectQuestionnaireDetector for the implementation based on the
  * blob-detect project (it stops at the "Localización de celdas (basada en columnas)" step).
  */
trait QuestionnaireDetector {

  /** A human readable name, used to select the detector from the command line */
  def name: String

  /**
    * Looks for a valid questionnaire in the given image.
    *
    * @param image the image to analyze
    * @return the questionnaire found, or None when the image does not contain a valid questionnaire
    */
  def detect(image: Mat): Option[DetectedQuestionnaire]

}
