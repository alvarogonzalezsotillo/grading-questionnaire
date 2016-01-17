package giftParser

import java.io._

import giftToLatex.GiftToLatex._

/**
 * Created by alvaro on 8/07/15.
 */
object Util {

  def withResource[T, U <: AutoCloseable](resource: U)(proc: (U => T)): T = {
    try {
      proc(resource)
    }
    finally {
      resource.close()
    }
  }

  implicit class Times(n: Int) {
    def times(proc: () => Unit): Unit = for (_ <- 0 to n) proc

    def times(proc: (Int) => Unit): Unit = for (i <- 0 to n) proc(i)
  }

  def inputToOutput( in: InputStream, out: OutputStream ): Unit ={
    val buffer = new Array[Byte](1024)
    var bytesRead = in.read(buffer)
    while (bytesRead > 0) {
      out.write(buffer, 0, bytesRead)
      bytesRead = in.read(buffer)
    }
  }


  def createTempDirectory: File = {
    val file = File.createTempFile("LatexCompile", "" + System.currentTimeMillis())
    file.delete()
    file.mkdirs()
    file.deleteOnExit()
    file
  }

  def cleanDirOrFile(f: File): Unit = {
    if (f.isDirectory) {
      for (file <- f.listFiles()) {
        cleanDirOrFile(file)
      }
    }
    f.delete()
  }

  def createFile( content: String, file: File ) = {
    withResource(  new FileWriter(file) ) { out =>
      out.write(content)
      out.flush()
    }
    file
  }

  def streamToString(in: InputStream) = {
    withResource( new ByteArrayOutputStream() ) { bos =>
      inputToOutput(in, bos)
      bos.toString
    }
  }

  def resourceToString( resource: String, loader: ClassLoader = this.getClass.getClassLoader ) = {

    withResource( loader.getResourceAsStream(resource) ){ in =>
      streamToString(in)
    }

  }
  
  def resourceToFile( resource: String, file: File,  loader: ClassLoader = this.getClass.getClassLoader ) = {
    withResource( loader.getResourceAsStream(resource) ) { in =>
      withResource( new FileOutputStream(file) ) { out =>
        inputToOutput(in,out)
      }
    }
  }


}
