package webserver.http

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.InetSocketAddress
import java.util.concurrent.Executors

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import webserver.json.Json
import webserver.service.{CaptureStore, DetectionOutcome, DetectionService, Detected, Duplicate, NotDetected}

/**
  * The http server.
  *
  * It is implemented with the http server that comes with the JDK (com.sun.net.httpserver), so
  * no new dependency is needed.
  *
  * Endpoints:
  *   - POST   /api/detect        the body is the image (jpeg/png). It answers with a json with
  *                              the information of the questionnaire when it has been found
  *   - GET    /api/captures      the list of the questionnaires detected so far
  *   - DELETE /api/captures      removes every capture
  *   - GET    /api/captures/{id} the image of a capture
  *   - DELETE /api/captures/{id} removes a capture
  *   - GET    /api/status        information about the server
  *   - GET    /                  the web application (html, javascript and css)
  */
class WebServerApp(
                    host: String,
                    port: Int,
                    service: DetectionService,
                    store: CaptureStore
                  ) {

  private var server: Option[HttpServer] = None
  private val startedAt = System.currentTimeMillis()

  def start(): Unit = {
    val httpServer = HttpServer.create(new InetSocketAddress(host, port), 0)

    httpServer.createContext("/api/detect", detectHandler)
    httpServer.createContext("/api/captures", capturesHandler)
    httpServer.createContext("/api/captures/", captureHandler)
    httpServer.createContext("/api/status", statusHandler)
    httpServer.createContext("/", staticHandler)

    httpServer.setExecutor(Executors.newFixedThreadPool(4))
    httpServer.start()

    server = Some(httpServer)
  }

  def stop(): Unit = {
    server.foreach(_.stop(0))
    server = None
  }

  /** The port where the server is listening (useful when it has been started with the port 0) */
  def listeningPort: Int = server.map(_.getAddress.getPort).getOrElse(port)

  /**
    * Wraps a handler so an unexpected error is answered with a json instead of leaving the
    * connection opened.
    */
  private def handler(body: HttpExchange => Unit): HttpHandler = new HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      try {
        body(exchange)
      } catch {
        case t: Throwable =>
          try {
            respond(exchange, 500, Json.obj("error" -> Json.str(String.valueOf(t.getMessage))))
          } catch {
            case _: Throwable => // the response was already started
          }
      } finally {
        exchange.close()
      }
    }
  }

  private val detectHandler = handler { exchange =>
    if (!"POST".equalsIgnoreCase(exchange.getRequestMethod)) {
      respond(exchange, 405, Json.obj("error" -> Json.str("only POST is allowed")))
    } else {
      val bytes = readAll(exchange.getRequestBody)
      val outcome = service.detect(bytes)
      respond(exchange, 200, outcomeToJson(outcome))
    }
  }

  private val capturesHandler = handler { exchange =>
    exchange.getRequestMethod.toUpperCase match {
      case "GET" =>
        respond(exchange, 200, Json.arr(store.all().map(_.toJson)))
      case "DELETE" =>
        store.clear()
        service.reset()
        respond(exchange, 200, Json.obj("deleted" -> Json.bool(true)))
      case _ =>
        respond(exchange, 405, Json.obj("error" -> Json.str("only GET and DELETE are allowed")))
    }
  }

  private val captureHandler = handler { exchange =>
    val id = exchange.getRequestURI.getPath.stripPrefix("/api/captures/")
    exchange.getRequestMethod.toUpperCase match {
      case "GET" =>
        store.fileOf(id) match {
          case Some(file) => respondFile(exchange, file)
          case None => respond(exchange, 404, Json.obj("error" -> Json.str("capture not found")))
        }
      case "DELETE" =>
        val deleted = store.delete(id)
        service.reset()
        respond(exchange, if (deleted) 200 else 404, Json.obj("deleted" -> Json.bool(deleted)))
      case _ =>
        respond(exchange, 405, Json.obj("error" -> Json.str("only GET and DELETE are allowed")))
    }
  }

  private val statusHandler = handler { exchange =>
    respond(exchange, 200, Json.obj(
      "detector" -> Json.str(service.detectorName),
      "captures" -> Json.num(store.all().size),
      "analyzedFrames" -> Json.num(service.analyzedFrames),
      "detectedQuestionnaires" -> Json.num(service.detectedQuestionnaires),
      "uptimeMs" -> Json.num(System.currentTimeMillis() - startedAt)
    ))
  }

  private val staticHandler = handler { exchange =>
    val path = exchange.getRequestURI.getPath
    val resource = if (path == "/" || path.isEmpty) "/public/index.html" else s"/public$path"

    Option(getClass.getResourceAsStream(resource)) match {
      case Some(input) =>
        try {
          respondBytes(exchange, 200, contentTypeOf(resource), readAllBytes(input))
        } finally {
          input.close()
        }
      case None =>
        respond(exchange, 404, Json.obj("error" -> Json.str("not found: " + path)))
    }
  }

  private def outcomeToJson(outcome: DetectionOutcome): String = outcome match {
    case Detected(capture) =>
      Json.obj(
        "detected" -> Json.bool(true),
        "duplicate" -> Json.bool(false),
        "capture" -> capture.toJson
      )
    case Duplicate(capture) =>
      Json.obj(
        "detected" -> Json.bool(false),
        "duplicate" -> Json.bool(true),
        "reason" -> Json.str("this questionnaire has already been detected"),
        "capture" -> capture.toJson
      )
    case NotDetected(reason) =>
      Json.obj(
        "detected" -> Json.bool(false),
        "duplicate" -> Json.bool(false),
        "reason" -> Json.str(reason)
      )
  }

  private def contentTypeOf(path: String): String = {
    if (path.endsWith(".html")) "text/html; charset=utf-8"
    else if (path.endsWith(".js")) "application/javascript; charset=utf-8"
    else if (path.endsWith(".css")) "text/css; charset=utf-8"
    else if (path.endsWith(".json")) "application/json; charset=utf-8"
    else if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg"
    else if (path.endsWith(".png")) "image/png"
    else if (path.endsWith(".svg")) "image/svg+xml"
    else "application/octet-stream"
  }

  private def readAll(input: InputStream): Array[Byte] = readAllBytes(input)

  private def readAllBytes(input: InputStream): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val chunk = new Array[Byte](8192)
    var read = input.read(chunk)
    while (read > 0) {
      buffer.write(chunk, 0, read)
      read = input.read(chunk)
    }
    buffer.toByteArray
  }

  private def respond(exchange: HttpExchange, code: Int, json: String): Unit =
    respondBytes(exchange, code, "application/json; charset=utf-8", json.getBytes("UTF-8"))

  private def respondFile(exchange: HttpExchange, file: java.io.File): Unit =
    respondBytes(exchange, 200, contentTypeOf(file.getName), java.nio.file.Files.readAllBytes(file.toPath), cache = true)

  private def respondBytes(
                            exchange: HttpExchange,
                            code: Int,
                            contentType: String,
                            body: Array[Byte],
                            cache: Boolean = false
                          ): Unit = {
    exchange.getResponseHeaders.add("Content-Type", contentType)
    exchange.getResponseHeaders.add("Access-Control-Allow-Origin", "*")
    exchange.getResponseHeaders.add("Cache-Control", if (cache) "private, max-age=3600" else "no-store")
    exchange.sendResponseHeaders(code, body.length.toLong)
    val os = exchange.getResponseBody
    try {
      os.write(body)
    } finally {
      os.close()
    }
  }

}
