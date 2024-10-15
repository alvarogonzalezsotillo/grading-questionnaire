package imgproc

object OpenCVLib{
  import org.opencv.core.Core;
  import java.lang.System;
  def loadLibrary(){
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  }
}
