package controllers

import javax.inject._
import play.api._
import play.api.libs.Files
import play.api.mvc._

import java.nio.file.Paths
import java.io.File

import common.HKey
import common.HMap


object WebServerInfo {

  object originalImageFileName extends HKey[File]

}

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def explore() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.explore())
  }

  def tutorial() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.tutorial())
  }

  def form() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.form())
  }


  def upload = Action(parse.multipartFormData) { request =>
    val files = request.body.files

    if (files.size == 0) {
      NotAcceptable("Missing file").flashing("error" -> "Missing file")
    }
    else{
      files.foreach { picture =>
        // only get the last part of the filename
        // otherwise someone can send a path like ../../home/foo/bar.txt to write to other files on the system
        val filename = Paths.get(picture.filename).getFileName
        val fileSize = picture.fileSize
        val contentType = picture.contentType
        val destDir =  Paths.get("/tmp/picture")
        val destFile = destDir.resolve( filename)

        destDir.toFile().mkdirs()
        picture.ref.copyTo(destFile, replace = true)
      }
      val fileNames = files.map(_.filename).mkString(",")
      Ok(s"Files uploaded. $fileNames")
    }
  }
}
