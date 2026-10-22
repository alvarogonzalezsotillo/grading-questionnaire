package webserver.service

import java.io.File
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong

import scala.jdk.CollectionConverters._

/**
  * Stores the images of the detected questionnaires in the disk, so they can be downloaded
  * by the browser (and inspected later on).
  */
class CaptureStore(val directory: File) {

  private val captures = new ConcurrentLinkedQueue[Capture]()
  private val counter = new AtomicLong(0)

  directory.mkdirs()
  loadAlreadyStored()

  /** The url where a capture can be downloaded */
  def urlOf(id: String): String = s"/api/captures/$id"

  /**
    * Saves the image of a detected questionnaire.
    *
    * @param bytes     the contents of the image
    * @param width     the width in pixels of the image
    * @param height    the height in pixels of the image
    * @param qr        the raw contents of the QR code
    * @param version   the version of the questionnaire
    * @param questions the number of questions of the questionnaire
    * @param cells     the number of answer cells that have been located
    */
  def save(
            bytes: Array[Byte],
            width: Int,
            height: Int,
            qr: Option[String],
            version: Option[Int],
            questions: Option[Int],
            cells: Option[Int]
          ): Capture = {

    val timestamp = System.currentTimeMillis()
    val id = newId(timestamp)
    val extension = extensionOf(bytes)
    val fileName = s"$id.$extension"
    val file = new File(directory, fileName)

    Files.write(file.toPath, bytes)

    val capture = Capture(
      id = id,
      fileName = fileName,
      url = urlOf(id),
      timestamp = timestamp,
      width = width,
      height = height,
      qr = qr,
      version = version,
      questions = questions,
      cells = cells
    )
    captures.add(capture)
    capture
  }

  /** All the stored captures, the newest ones first */
  def all(): Seq[Capture] = captures.asScala.toSeq.sortBy(c => -c.timestamp)

  /** The file where a capture is stored */
  def fileOf(id: String): Option[File] = {
    val safeId = id.replaceAll("[^A-Za-z0-9._-]", "")
    val candidates = Option(directory.listFiles()).getOrElse(Array.empty[File])
    candidates.find(f => f.getName == safeId || f.getName.startsWith(safeId + "."))
  }

  /** Removes a capture (both from the index and from the disk) */
  def delete(id: String): Boolean = {
    val file = fileOf(id)
    val removed = captures.removeIf(c => c.id == id)
    file.foreach(f => f.delete())
    removed || file.exists(_.exists())
  }

  /** Removes every capture */
  def clear(): Unit = {
    captures.clear()
    Option(directory.listFiles()).getOrElse(Array.empty[File]).foreach(f => f.delete())
  }

  private def newId(timestamp: Long): String = {
    val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val sequence = (counter.incrementAndGet() % 1000).toInt
    s"${dateFormat.format(new Date(timestamp))}-${"%03d".format(sequence)}"
  }

  private def extensionOf(bytes: Array[Byte]): String = {
    if (bytes.length > 3 && bytes(0) == 0xFF.toByte && bytes(1) == 0xD8.toByte) "jpg"
    else if (bytes.length > 8 && bytes(0) == 0x89.toByte && bytes(1) == 'P'.toByte) "png"
    else "jpg"
  }

  /**
    * Restores the captures that are already stored in the disk, so the browser can show them
    * even after a restart of the server.
    */
  private def loadAlreadyStored(): Unit = {
    val files = Option(directory.listFiles()).getOrElse(Array.empty[File])
    files
      .filter(f => f.isFile && (f.getName.endsWith(".jpg") || f.getName.endsWith(".png")))
      .foreach { file =>
        val id = file.getName.takeWhile(_ != '.')
        val timestamp = file.lastModified()
        captures.add(
          Capture(
            id = id,
            fileName = file.getName,
            url = urlOf(id),
            timestamp = timestamp,
            width = 0,
            height = 0,
            qr = None,
            version = None,
            questions = None,
            cells = None
          )
        )
      }
  }

}
