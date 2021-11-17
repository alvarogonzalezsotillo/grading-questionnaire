package common

import java.io.{File, IOException}
import javax.sound.sampled._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Created by alvaro on 24/01/16.
 */
object Sounds extends App {

  private val BUFFER_SIZE: Int = 128000


  def playSound2( resource: String ) : Unit = {
    val clip = AudioSystem.getClip()
    val soundFileUrl = getClass().getResource(resource)
    val inputStream = AudioSystem.getAudioInputStream(soundFileUrl);
    clip.open(inputStream)
    clip.start()
    clip.flush()
    clip.close()
  }

  def playSound(resource: String) : Unit = {

    val soundFileUrl = getClass().getResource(resource)
    val audioStream = AudioSystem.getAudioInputStream(soundFileUrl)

    val audioFormat = audioStream.getFormat
    val info: DataLine.Info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
    val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    sourceLine.open(audioFormat)
    sourceLine.start
    var nBytesRead: Int = Integer.MAX_VALUE
    val abData: Array[Byte] = new Array[Byte](BUFFER_SIZE)
    while (nBytesRead > 0) {
      try {
        nBytesRead = audioStream.read(abData, 0, abData.length)
        println(s"nBytesRead:$nBytesRead")
      }
      catch {
        case e: IOException => {
          e.printStackTrace
        }
      }
      if (nBytesRead >= 0) {
        val nBytesWritten = sourceLine.write(abData, 0, nBytesRead)
      }
    }
    sourceLine.drain
    sourceLine.close
    audioStream.close
  }

  def beep() = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val BEEP_FILE = "/sounds/Censored_Beep.wav"
    Future {
      playSound(BEEP_FILE)
    }
  }

  Await.ready( beep(), Duration.Inf)

}
