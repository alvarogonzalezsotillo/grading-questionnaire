package webserver

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.Files

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import webserver.detector.BlobDetectQuestionnaireDetector
import webserver.service._

@RunWith(classOf[JUnitRunner])
class DetectionServiceTest extends AnyFlatSpec with Matchers {

  OpenCvLoader.load()

  private def readAll(input: InputStream): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val chunk = new Array[Byte](8192)
    var read = input.read(chunk)
    while (read > 0) {
      buffer.write(chunk, 0, read)
      read = input.read(chunk)
    }
    buffer.toByteArray
  }

  private def sample(name: String): Array[Byte] = {
    val stream = getClass.getResourceAsStream(s"/samples/$name")
    assert(stream != null, s"the sample $name is missing")
    try readAll(stream) finally stream.close()
  }

  private def newService(config: DetectionConfig = DetectionConfig()): (DetectionService, CaptureStore) = {
    val directory = Files.createTempDirectory("web-server-captures").toFile
    val store = new CaptureStore(directory)
    (new DetectionService(new BlobDetectQuestionnaireDetector(), store, config), store)
  }

  "The blob detect detector" should "find a questionnaire in a picture of a questionnaire" in {
    val (service, store) = newService()

    val outcome = service.detect(sample("questionnaire.jpg"))

    outcome shouldBe a[Detected]
    val capture = outcome.asInstanceOf[Detected].capture
    assert(capture.cells.get > 0)
    assert(capture.questions.get > 0)
    assert(store.all().size == 1)
    assert(store.fileOf(capture.id).get.exists())
  }

  it should "not find a questionnaire in an empty image" in {
    val (service, store) = newService()
    val empty = Array.fill[Byte](64 * 64 * 3)(200.toByte)

    val outcome = service.detect(empty)

    outcome shouldBe a[NotDetected]
    assert(store.all().isEmpty)
  }

  it should "not store twice the same questionnaire" in {
    val (service, store) = newService()

    val first = service.detect(sample("questionnaire.jpg"))
    val second = service.detect(sample("questionnaire.jpg"))

    first shouldBe a[Detected]
    second shouldBe a[Duplicate]
    assert(store.all().size == 1)
  }

  it should "store a new questionnaire when the scene changes" in {
    val (service, store) = newService(DetectionConfig(minIntervalMs = 0L))

    val first = service.detect(sample("questionnaire.jpg"))
    val second = service.detect(sample("questionnaire2.jpg"))

    first shouldBe a[Detected]
    second shouldBe a[Detected]
    assert(store.all().size == 2)
  }

  it should "wait a little before storing a new questionnaire" in {
    val (service, store) = newService(DetectionConfig(minIntervalMs = 60000L))

    val first = service.detect(sample("questionnaire.jpg"))
    val second = service.detect(sample("questionnaire2.jpg"))

    first shouldBe a[Detected]
    second shouldBe a[Duplicate]
    assert(store.all().size == 1)
  }

}
