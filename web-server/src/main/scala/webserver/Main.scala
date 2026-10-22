package webserver

import java.io.File

import scopt.OptionParser
import webserver.detector.DetectorRegistry
import webserver.http.WebServerApp
import webserver.service.{CaptureStore, DetectionConfig, DetectionService}

/**
  * Entry point of the web server.
  *
  * It serves the web application (that uses the camera of the browser) and analyzes the
  * images that the browser sends, looking for valid questionnaires.
  */
object Main {

  case class Config(
                     host: String = "0.0.0.0",
                     port: Int = 8080,
                     capturesDir: File = new File("web-server/captures"),
                     detector: Option[String] = None,
                     minIntervalMs: Long = 2000L,
                     duplicateThreshold: Double = 5.0,
                     listDetectors: Boolean = false
                   )

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("web-server") {

      head("web-server: detects questionnaires in the images sent by a browser")

      opt[Unit]("list-detectors").action((_, c) => c.copy(listDetectors = true))
        .text("lists the available detectors and exits")

      opt[String]("host").action((x, c) => c.copy(host = x))
        .text("the address the server listens to (default: 0.0.0.0)")

      opt[Int]("port").action((x, c) => c.copy(port = x))
        .text("the port the server listens to (default: 8080)")

      opt[File]("captures-dir").action((x, c) => c.copy(capturesDir = x))
        .text("the directory where the detected questionnaires are stored (default: web-server/captures)")

      opt[String]("detector").action((x, c) => c.copy(detector = Some(x)))
        .text("the detector to use (default: the first one available)")

      opt[Long]("min-interval-ms").action((x, c) => c.copy(minIntervalMs = x))
        .text("the minimum time between two accepted questionnaires (default: 2000)")

      opt[Double]("duplicate-threshold").action((x, c) => c.copy(duplicateThreshold = x))
        .text("how different two images must be to be considered different questionnaires (default: 5.0)")

      help("help").text("shows this usage")
    }

    parser.parse(args, Config()) match {
      case None => // the parser has already shown the error
      case Some(config) =>
        if (config.listDetectors) {
          DetectorRegistry.all().foreach(detector => println(detector.name))
        } else {
          run(config)
        }
    }
  }

  private def run(config: Config): Unit = {

    val detector = config.detector match {
      case Some(name) =>
        DetectorRegistry.byName(name).getOrElse {
          System.err.println(s"Unknown detector: $name. Available ones: ${availableDetectors()}")
          sys.exit(1)
        }
      case None =>
        DetectorRegistry.default()
    }

    OpenCvLoader.load()

    val store = new CaptureStore(config.capturesDir)
    val service = new DetectionService(
      detector = detector,
      store = store,
      config = DetectionConfig(config.minIntervalMs, config.duplicateThreshold)
    )

    val server = new WebServerApp(config.host, config.port, service, store)
    try {
      server.start()
    } catch {
      case e: java.net.BindException =>
        System.err.println(s"Could not listen on ${config.host}:${config.port}: ${e.getMessage}")
        sys.exit(1)
    }

    val port = server.listeningPort
    println(s"[web-server] Detector: ${detector.name}")
    println(s"[web-server] Captures are stored in ${store.directory.getAbsolutePath}")
    println(s"[web-server] Open http://localhost:$port in the browser to use the camera")
    if (config.host != "localhost" && config.host != "127.0.0.1") {
      println(s"[web-server] Other devices can use http://<this-machine-ip>:$port")
    }

    val shutdown = new Thread(new Runnable {
      override def run(): Unit = {
        println("[web-server] Stopping the server")
        server.stop()
      }
    })
    Runtime.getRuntime.addShutdownHook(shutdown)

    Thread.currentThread().join()
  }

  private def availableDetectors(): String =
    DetectorRegistry.all().map(_.name).mkString(", ")

}
