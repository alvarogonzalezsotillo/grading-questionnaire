package webserver.detector

import java.util.ServiceLoader

import scala.jdk.CollectionConverters._

import webserver.spi.QuestionnaireDetector

/**
  * Discovers the available [[QuestionnaireDetector]] implementations using the standard
  * java.util.ServiceLoader mechanism (the SPI).
  */
object DetectorRegistry {

  /** All the detectors available in the classpath */
  def all(): Seq[QuestionnaireDetector] = {
    val loader = ServiceLoader.load(classOf[QuestionnaireDetector])
    loader.iterator().asScala.toSeq
  }

  /** The detector with the given name, if any */
  def byName(name: String): Option[QuestionnaireDetector] = all().find(_.name == name)

  /**
    * The detector used when the user does not select one explicitly.
    * It falls back to the blob-detect based detector when no implementation is registered
    * in META-INF/services.
    */
  def default(): QuestionnaireDetector =
    all().headOption.getOrElse(new BlobDetectQuestionnaireDetector())

}
