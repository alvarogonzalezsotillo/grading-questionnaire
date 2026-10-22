package webserver

import org.opencv.core.Core

/**
  * Loads the OpenCV native library.
  *
  * The openpnp artifact bundles the native libraries inside the jar, so they are extracted to a
  * temporary directory and loaded from there. When that is not possible (for instance when the
  * library has been installed system wide) it falls back to the standard library loading
  * mechanism, which requires the java.library.path to be set.
  */
object OpenCvLoader {

  @volatile private var loaded = false

  def load(): Unit = synchronized {
    if (!loaded) {
      loadFromJarOrSystem()
      loaded = true
    }
  }

  private def loadFromJarOrSystem(): Unit = {
    var loadedFromJar = false
    try {
      nu.pattern.OpenCV.loadLocally()
      loadedFromJar = true
    } catch {
      case t: Throwable =>
        System.err.println(s"[web-server] Could not load the OpenCV natives from the jar: ${t.getMessage}")
    }
    if (!loadedFromJar) {
      System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    }
  }

}
