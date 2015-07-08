package giftParser

import java.io.{FileWriter, File}

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

  def createTempDirectory: File = {
    val file = File.createTempFile("LatexCompile", "" + System.currentTimeMillis())
    file.delete()
    file.mkdirs()
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

}
