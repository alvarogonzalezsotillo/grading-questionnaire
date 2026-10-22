package webserver.service

/**
  * A questionnaire that has been detected and stored in the server.
  *
  * @param id         the unique identifier of the capture (it is also the name of the file)
  * @param fileName   the name of the file where the image is stored
  * @param url        the url where the image can be downloaded
  * @param timestamp  the moment (epoch millis) when the questionnaire was detected
  * @param width      the width in pixels of the stored image
  * @param height     the height in pixels of the stored image
  * @param qr         the raw contents of the QR code
  * @param version    the version of the questionnaire
  * @param questions  the number of questions of the questionnaire
  * @param cells      the number of answer cells that have been located
  */
case class Capture(
                     id: String,
                     fileName: String,
                     url: String,
                     timestamp: Long,
                     width: Int,
                     height: Int,
                     qr: Option[String],
                     version: Option[Int],
                     questions: Option[Int],
                     cells: Option[Int]
                     ) {

  def toJson: String = {
    import webserver.json.Json
    Json.obj(
      "id" -> Json.str(id),
      "fileName" -> Json.str(fileName),
      "url" -> Json.str(url),
      "timestamp" -> Json.num(timestamp),
      "width" -> Json.num(width),
      "height" -> Json.num(height),
      "qr" -> Json.optStr(qr),
      "version" -> Json.optNum(version),
      "questions" -> Json.optNum(questions),
      "cells" -> Json.optNum(cells)
    )
  }

}
