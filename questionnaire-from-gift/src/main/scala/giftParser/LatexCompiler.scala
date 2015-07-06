package giftParser

import java.io.{FileWriter, File}
import java.nio.file.Path

import scala.sys.process.{ProcessBuilder, Process}

/**
 * Created by alvaro on 4/07/15.
 */
object LatexCompiler{

  private def createTempDirectory : File = {
    val file = File.createTempFile("LatexCompile", "" + System.currentTimeMillis() )
    file.delete()
    file.mkdirs()
    file.deleteOnExit()
    file
  }

  private def createFile( content: String, file: File ) = {
    val out = new FileWriter(file)
    out.write(content)
    out.close()
  }

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

  private def cleanDirOrFile( f: File ): Unit ={
    if( f.isDirectory ){
      for( file <- f.listFiles() ){
        cleanDirOrFile( file )
      }
    }
    f.delete()
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
