package common

import java.util.logging.LogManager

/**
 * Created by alvaro on 24/01/16.
 */
object Logging {
  def disableLogging() = LogManager.getLogManager().reset()
}
