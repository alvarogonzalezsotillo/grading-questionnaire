package imgproc

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.opencv.core._



object AnswerMatrixMeasures {
  def apply(version: Int) = version match {
    case 0 => new AnswerMatrixMeasuresHorizontalTicked()
    case 1 => new AnswerMatrixMeasuresHorizontalTicked()
  }

  val matrixWithToLeftOfQRRatio = 0.02
  val matrixWithToTopOfQRRatio: Double = 0.23
  val matrixWithToQRWidthRatio: Double = 0.20


  object TypeSafeWidthHeight {

    case class X(x: Double) {
      def +(ox: X) = X(x + ox.x)

      def *(r: Double) = X(x * r)
    }

    case class Y(y: Double) {
      def +(oy: Y) = Y(y + oy.y)

      def *(r: Double) = Y(y * r)
    }

    case class Point(x: X, y: Y) {
      def +(p: Point) = Point(x + p.x, y + p.y)

      def *(d: Double) = Point(x * d, y * d)

      def toOpenCV = new org.opencv.core.Point(x.x,y.y)

      def toRect( s: Size ) = Rect( this, s.w, s.h )
    }

    case class Width(w: Double) {
      def *(r: WidthToHeightRatio): Height = Height(w * r.r)

      def *(r: Double) = Width(w * r)

      def +(wi: Width) = Width(w + wi.w)

      def -(wi: Width) = this + (wi * -1.0)

      lazy val toX = X(w)

      lazy val toPoint = Point( toX, Y(0) )
    }

    case class Height(h: Double) {
      def *(r: HeightToWidthRatio) = Width(h * r.r)

      def *(r: Double) = Height(h * r)

      def +(he: Height) = Height(h + he.h)

      def toY = Y(h)

      lazy val toPoint = Point( X(0), toY )
    }

    trait Area{
      val w: Width
      val h: Height
      def area = w.w * h.h
    }

    case class Size(w: Width, h: Height) extends Area{
      def on(o: Point) = Rect(o, w, h)
      lazy val toOpenCV = new org.opencv.core.Size(w.w,h.h)
      def toRect = on( Point(X(0),Y(0)) )
    }

    case class Rect(o: Point, w: Width, h: Height) extends Area{
      lazy val corners = Seq(
        o,
        o + w.toPoint,
        o + w.toPoint + h.toPoint,
        o + h.toPoint
      )
      lazy val toOpenCV = new MatOfPoint( corners.map(_.toOpenCV):_* )
      lazy val size = new Size(w,h)
    }

    case class WidthToHeightRatio(r: Double)

    case class HeightToWidthRatio(r: Double)

  }

  import TypeSafeWidthHeight._

  object MeasuresToOpenCV{
    def fromMeasures( src: Seq[Rect], anchorInMeasures: Rect, anchorInOpenCV: MatOfPoint ) : Seq[MatOfPoint] = {
      import ImageProcessing._
      assert( anchorInOpenCV.size == 4)
      val H = findHomography(anchorInMeasures.toOpenCV, anchorInOpenCV)
      warpContours( src.map(_.toOpenCV), H)
    }
  }

}



class AnswerMatrixMeasuresHorizontalTicked(val columns: Int = 5) extends LazyLogging{

  def rows(questions: Int) = (1.0 * questions / columns).ceil.toInt

  import AnswerMatrixMeasures.TypeSafeWidthHeight._


  object Params {
    private val f = 5
    val cellHeaderSize = Size(Width(24*f), Height(14*f))
    val answerCellAvailableWidth = Width(67*f) // INCLUDING cellHeaderToCellWidthGap, cellSize AND THE FOLLOWING SPACE
    val cellHeaderToCellWidthGap = Width(2*f)
    val cellSize = Size(Width(52*f), cellHeaderSize.h)
    val answerTableOrigin = Point(X(0), Y(0))
  }

  import Params._

  private val columnWidth = cellHeaderSize.w + answerCellAvailableWidth
  private val answerTableWidth = columnWidth * columns
  private def answerTableHeight(questions:Int) = cellHeaderSize.h * rows(questions)

  private def answerTableSize(questions: Int) = Size(answerTableWidth, answerTableHeight(questions))

  def answerTableRect(questions: Int) = answerTableOrigin.toRect(answerTableSize(questions))

  def cellArea = cellSize.area

  def qrLocation = ???

  private object ValuesForAnswerCell {
    val basePointForCells = answerTableOrigin + Point((cellHeaderSize.w + cellHeaderToCellWidthGap ).toX, Y(0))
    val columnOffset = Point((answerCellAvailableWidth + cellHeaderSize.w).toX, Y(0))
    val rowOffset = Point(X(0), cellSize.h.toY)
    logger.error( s"rowOffset:$rowOffset")
  }

  def rowOfQuestion(question: Int, questions: Int ) = question / columns
  def columnOfQuestion(question: Int, questions: Int ) = question % columns

  def answerCells(questions: Int) = {

    def answerCell(question: Int) = {
      assert(question >= 0 && question < questions)

      val row = rowOfQuestion(question,questions)
      val column = columnOfQuestion(question,questions)

      import ValuesForAnswerCell._

      cellSize.on(basePointForCells + rowOffset * row + columnOffset * column)
    }

    (0 until questions).map(answerCell)
  }
}


