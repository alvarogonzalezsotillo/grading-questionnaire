package webserver

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.{HttpURLConnection, URL}
import java.nio.file.Files

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import webserver.detector.BlobDetectQuestionnaireDetector
import webserver.http.WebServerApp
import webserver.service.{CaptureStore, DetectionService}

@RunWith(classOf[JUnitRunner])
class WebServerAppTest extends AnyFlatSpec with Matchers {

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

  private def sampleQuestionnaire: Array[Byte] = {
    val stream = getClass.getResourceAsStream("/samples/questionnaire.jpg")
    assert(stream != null, "the sample questionnaire is missing")
    try readAll(stream) finally stream.close()
  }

  private case class Response(code: Int, contentType: String, body: Array[Byte]) {
    def text: String = new String(body, "UTF-8")
  }

  private var server: WebServerApp = _
  private var port: Int = 0

  private def withServer(test: => Unit): Unit = {
    val directory = Files.createTempDirectory("web-server-captures").toFile
    val store = new CaptureStore(directory)
    val service = new DetectionService(new BlobDetectQuestionnaireDetector(), store)
    server = new WebServerApp("localhost", 0, service, store)
    server.start()
    port = server.listeningPort
    try {
      test
    } finally {
      server.stop()
      server = null
    }
  }

  private def request(method: String, path: String, body: Option[Array[Byte]] = None): Response = {
    val url = new URL(s"http://localhost:$port$path")
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod(method)
    connection.setConnectTimeout(5000)
    connection.setReadTimeout(60000)
    body.foreach { bytes =>
      connection.setDoOutput(true)
      connection.setRequestProperty("Content-Type", "image/jpeg")
      connection.setFixedLengthStreamingMode(bytes.length)
      val os = connection.getOutputStream
      try os.write(bytes) finally os.close()
    }
    val code = connection.getResponseCode
    val contentType = connection.getContentType
    val stream = Option(connection.getInputStream).getOrElse(connection.getErrorStream)
    val content = if (stream == null) Array.empty[Byte] else readAll(stream)
    Response(code, contentType, content)
  }

  "The web server" should "serve the web application" in {
    withServer {
      val response = request("GET", "/")
      assert(response.code == 200)
      assert(response.text.contains("<video"))
    }
  }

  it should "detect the questionnaires of the images that the browser sends" in {
    withServer {
      val response = request("POST", "/api/detect", Some(sampleQuestionnaire))

      assert(response.code == 200)
      assert(response.text.contains("\"detected\":true"), response.text)
      assert(response.text.contains("\"cells\""), response.text)
    }
  }

  it should "answer that no questionnaire has been found in a blank image" in {
    withServer {
      val blank = Array.fill[Byte](64 * 64 * 3)(200.toByte)

      val response = request("POST", "/api/detect", Some(blank))

      assert(response.code == 200)
      assert(response.text.contains("\"detected\":false"), response.text)
    }
  }

  it should "list and serve the questionnaires that have been detected" in {
    withServer {
      val detectResponse = request("POST", "/api/detect", Some(sampleQuestionnaire))
      val id = """"id":"([^"]+)"""".r.findFirstMatchIn(detectResponse.text).get.group(1)

      val list = request("GET", "/api/captures")
      assert(list.code == 200)
      assert(list.text.contains(id), list.text)

      val image = request("GET", s"/api/captures/$id")
      assert(image.code == 200)
      assert(image.contentType.startsWith("image/jpeg"), image.contentType)
      assert(image.body.length > 1000)
      // it is a jpeg
      assert(image.body(0) == 0xFF.toByte && image.body(1) == 0xD8.toByte)
    }
  }

}
