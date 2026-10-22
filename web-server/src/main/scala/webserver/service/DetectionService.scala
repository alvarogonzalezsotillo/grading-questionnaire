package webserver.service

import com.typesafe.scalalogging.LazyLogging
import org.opencv.core.{Mat, MatOfByte}
import org.opencv.imgcodecs.Imgcodecs
import webserver.spi.{DetectedQuestionnaire, QuestionnaireDetector}

/** The result of analyzing an image sent by the browser */
sealed trait DetectionOutcome

/** The image does not contain a valid questionnaire */
case class NotDetected(reason: String) extends DetectionOutcome

/** The image contains a questionnaire that has already been detected (it is not stored again) */
case class Duplicate(capture: Capture) extends DetectionOutcome

/** A new valid questionnaire has been found and stored */
case class Detected(capture: Capture) extends DetectionOutcome

/**
  * Configuration of the detection service
  *
  * @param minIntervalMs        the minimum time between two accepted questionnaires
  * @param duplicateThreshold   the maximum difference between two images of the same questionnaire
  */
case class DetectionConfig(minIntervalMs: Long = 2000L, duplicateThreshold: Double = 5.0)

/**
  * Uses a [[QuestionnaireDetector]] to look for valid questionnaires in the images sent by the
  * browser, discarding the repetitions of an already detected questionnaire.
  */
class DetectionService(
                          detector: QuestionnaireDetector,
                          store: CaptureStore,
                          config: DetectionConfig = DetectionConfig()
                        ) extends LazyLogging {

  private var lastCapture: Option[Capture] = None
  private var lastAcceptedSignature: Option[ImageSignature] = None
  private var lastQr: Option[String] = None
  private var framesAnalyzed = 0L
  private var questionnairesFound = 0L

  def detectorName: String = detector.name

  /** The number of images that have been analyzed */
  def analyzedFrames: Long = framesAnalyzed

  /** The number of questionnaires that have been found */
  def detectedQuestionnaires: Long = questionnairesFound

  /**
    * Forgets the last detected questionnaire, so the next one is not considered a repetition.
    * It is used when the stored questionnaires are deleted.
    */
  def reset(): Unit = synchronized {
    lastCapture = None
    lastAcceptedSignature = None
    lastQr = None
  }

  /**
    * Analyzes an image.
    *
    * @param bytes the contents of the image
    * @return the outcome of the analysis
    */
  def detect(bytes: Array[Byte]): DetectionOutcome = synchronized {

    if (bytes == null || bytes.isEmpty) {
      return NotDetected("empty image")
    }

    val image = decode(bytes)
    if (image.isEmpty) {
      return NotDetected("the image could not be decoded")
    }

    framesAnalyzed += 1

    val mat = image.get
    try {
      val signature = ImageSignature.of(mat)

      val detected = try {
        detector.detect(mat)
      } catch {
        case t: Throwable =>
          logger.error(s"Error while detecting a questionnaire: ${t.getMessage}", t)
          None
      }

      detected match {
        case None =>
          NotDetected("no valid questionnaire found")

        case Some(questionnaire) =>
          if (isDuplicate(signature, questionnaire)) {
            Duplicate(lastCapture.getOrElse(emptyCapture))
          } else {
            val capture = store.save(
              bytes = bytes,
              width = mat.width(),
              height = mat.height(),
              qr = questionnaire.qrText,
              version = questionnaire.version,
              questions = questionnaire.questions,
              cells = Some(questionnaire.cells)
            )
            questionnairesFound += 1
            lastCapture = Some(capture)
            lastAcceptedSignature = Some(signature)
            lastQr = questionnaire.qrText
            logger.info(s"Questionnaire detected: ${capture.fileName}")
            Detected(capture)
          }
      }
    } finally {
      mat.release()
    }
  }

  private def emptyCapture: Capture =
    Capture("", "", "", System.currentTimeMillis(), 0, 0, None, None, None, None)

  private def decode(bytes: Array[Byte]): Option[Mat] = {
    val decoded = Imgcodecs.imdecode(new MatOfByte(bytes: _*), Imgcodecs.IMREAD_COLOR)
    if (decoded == null || decoded.empty()) None else Some(decoded)
  }

  /**
    * Tells whether the questionnaire that has just been found is actually the same one that was
    * detected previously (the camera does not move, so the images are almost identical).
    *
    * A questionnaire is considered to be a new one when:
    *   - its QR code is different (it is a different questionnaire), or
    *   - the image is different enough and enough time has passed since the last one.
    *
    * When the questionnaire is the stored one, the reference image is updated, so a
    * questionnaire that is moved slowly is not stored again and again.
    */
  private def isDuplicate(signature: ImageSignature, questionnaire: DetectedQuestionnaire): Boolean = {
    val now = System.currentTimeMillis()

    val sameQr = lastQr.isDefined && questionnaire.qrText.isDefined && lastQr == questionnaire.qrText
    val sameScene = lastAcceptedSignature.exists(_.distanceTo(signature) < config.duplicateThreshold)
    val tooSoon = lastCapture.exists(capture => now - capture.timestamp < config.minIntervalMs)

    if (!sameQr) {
      false
    } else if (sameScene) {
      // it is the questionnaire that has already been stored (it may have moved a little)
      lastAcceptedSignature = Some(signature)
      true
    } else if (tooSoon) {
      // the scene has changed, but it is too soon: it will be detected again in a few seconds
      true
    } else {
      false
    }
  }

}
