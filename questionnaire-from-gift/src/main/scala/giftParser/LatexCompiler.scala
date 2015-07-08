package giftParser

import java.io.{FileWriter, File}
import java.nio.file.Path

import scala.sys.process.{ProcessBuilder, Process}

/**
 * Created by alvaro on 4/07/15.
 */
object LatexCompiler{

  import giftParser.Util._



  private def compile(f: File): Process = {
    val fileName = f.getName
    val quotes = '\"'
    val cmd = s"pdflatex  --shell-escape -synctex=1 -interaction=nonstopmode $quotes$fileName$quotes"
    Process( cmd, f.getParentFile).run()
  }

  private def move(src: File, dst: File){
    dst.delete()
    java.nio.file.Files.move( src.toPath, dst.toPath )
  }


  def compile( latex: String, outputFile : File, keepTexFile : Boolean = true, times: Int = 2 ) : IndexedSeq[Process] = {
    val dir = createTempDirectory

    val fileName = outputFile.getName.take( outputFile.getName.indexOf('.') )
    val texFileName = fileName + ".tex"
    val texFile = new File(dir, texFileName)

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
    }

    cleanDirOrFile(dir)

    processes
  }


}
