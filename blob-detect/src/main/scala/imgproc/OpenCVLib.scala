package imgproc

object OpenCVLib{
  import org.opencv.core.Core;
  import java.lang.System;
  def loadLibrary(): Unit = {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }
}
