package common

/**
 * Created by alvaro on 25/04/16.
 */
object QuestionnaireVersion {

  val versions = Map[Byte, String](
    0.toByte -> "Horizontal free hand letter answers",
    1.toByte -> "Vertical free hand letter answers",
    2.toByte -> "Horizontal ticked answers",
    3.toByte -> "Vertical Ticked answers"
  )

  def isHorizontalVersion(v: Byte) = v == 0 || v == 2

  def isVerticalVersion(v: Byte) = !isHorizontalVersion(v)

  def isTickedVersion(v: Byte) = v == 2 || v == 3

  def isLetterVersion(v: Byte) = !isTickedVersion(v)


  def version(horizontal: Boolean, ticked: Boolean) = {
    (if (horizontal) 0 else 1) + (if (ticked) 0 else 2)
  }.toByte

}
