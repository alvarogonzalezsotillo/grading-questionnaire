package common

/**
 * Created by alvaro on 25/04/16.
 */
object QuestionnaireVersion {

  val versions = Map[Byte, String](
    0.toByte -> "Horizontal ticked answers",
    1.toByte -> "Vertical ticked answers",
    2.toByte -> "Horizontal free letter answers",
    3.toByte -> "Vertical letter answers"
  )

  def isHorizontalVersion(v: Byte) = v == 0 || v == 2

  def isVerticalVersion(v: Byte) = !isHorizontalVersion(v)

  def isTickedVersion(v: Byte) = v == 0 || v == 1

  def isLetterVersion(v: Byte) = !isTickedVersion(v)


  def version(horizontal: Boolean, ticked: Boolean) = {
    (if (horizontal) 0 else 1) + (if (ticked) 0 else 2)
  }.toByte

}
