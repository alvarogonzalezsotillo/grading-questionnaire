package giftToLatex

import java.io.{InputStream, File}

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.sys.process.{Process, ProcessIO}

/**
 * Created by alvaro on 4/07/15.
 */
object LatexCompiler extends LazyLogging{

  import giftParser.Util._


  private def consumeInputStream(in:InputStream){
    new Thread{
      while( in.read != -1 ){
      }
    }.run()
  }


  private def compile(f: File): Process = {
    val fileName = f.getName
    val quotes = '\"'
    val cmd = s"pdflatex  --shell-escape -synctex=1 -interaction=nonstopmode $quotes$fileName$quotes"
    val io = new ProcessIO( _ => (), consumeInputStream, _ => () )
    Process( cmd, f.getParentFile).run(io)
  }

  private def move(src: File, dst: File){
    dst.delete()

    logger.error( s"movig:${src.toPath} to:${dst.toPath}")
    java.nio.file.Files.move( src.toPath, dst.toPath )
  }

  def apply( latex: String, outputFile : File, keepTexFile : Boolean = true, times: Int = 2 ) = {
    compile(latex, outputFile, keepTexFile, times )
  }

  def extractResourcesToDir(dir: File) = {
    val resources = Map("giftToLatex/qrcode.sty" -> "qrcode.sty")
    for( r <- resources ){
      resourceToFile( r._1, new File(dir,r._2) )
    }
  }

  def compile( latex: String, outputFile : File, keepTexFile : Boolean = true, times: Int = 2 ) : IndexedSeq[Process] = {
    val dir = createTempDirectory

    val fileName = outputFile.getName.take( outputFile.getName.lastIndexOf('.') )
    val texFileName = fileName + ".tex"
    val texFile = new File(dir, texFileName)

    extractResourcesToDir(dir)

    logger.error( s"outputFile:$outputFile fileName:$fileName texFileName:$texFileName ")

    createFile( latex, texFile )
    val processes = for( t <- 0 to times ) yield {
      val p = compile(texFile)
      p.exitValue()
      p
    }


    val pdfFileName = fileName + ".pdf"
    val pdfFile = new File(dir,pdfFileName)
    move( pdfFile, outputFile )

    if( keepTexFile ){
      val texOutputFile = new File( outputFile.getParentFile, texFileName )
      move( texFile, texOutputFile )
      extractResourcesToDir(texOutputFile.getParentFile)
    }

    cleanDirOrFile(dir)

    processes
  }


}
