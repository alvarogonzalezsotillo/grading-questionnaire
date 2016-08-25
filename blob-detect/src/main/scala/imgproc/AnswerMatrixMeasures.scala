package imgproc

import com.typesafe.scalalogging.slf4j.LazyLogging
import imgproc.AnswerMatrixMeasures.TypeSafeWidthHeight.{Y, Rect => _, _}
import org.opencv.core.MatOfPoint


object AnswerMatrixMeasures {
  def apply(version: Int): AnswerMatrixMeasures = apply(None, version)

  def apply(questions: Int, version: Int): AnswerMatrixMeasures = apply(Some(questions), version)

  def apply(questions: Option[Int], versionI: Int) = {
    import common.QuestionnaireVersion._
    val v = versionI.toByte
    (isVerticalVersion(v), isLetterVersion(v) ) match{
      case (true,true) =>  new AnswerMatrixMeasuresVerticalLetter(questions)
      case (true,false) => new AnswerMatrixMeasuresVerticalTicked(questions)
      case (false,true) => new AnswerMatrixMeasuresHorizontalLetter(questions)
      case (false,false) => new AnswerMatrixMeasuresHorizontalTicked(questions)
    }
  }

  val matrixWithToLeftOfQRRatio = 0.02
  val matrixWithToTopOfQRRatio: Double = 0.25
  val matrixWithToQRWidthRatio: Double = 0.25


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

      def toOpenCV = new org.opencv.core.Point(x.x, y.y)

      def toRect(s: Size) = Rect(this, s.w, s.h)
    }

    case class Width(w: Double) {
      def *(r: WidthToHeightRatio): Height = Height(w * r.r)

      def *(r: Double) = Width(w * r)

      def +(wi: Width) = Width(w + wi.w)

      def -(wi: Width) = this + (wi * -1.0)

      lazy val toX = X(w)

      lazy val toPoint = Point(toX, Y(0))
    }

    case class Height(h: Double) {
      def *(r: HeightToWidthRatio) = Width(h * r.r)

      def *(r: Double) = Height(h * r)

      def +(he: Height) = Height(h + he.h)

      def toY = Y(h)

      lazy val toPoint = Point(X(0), toY)
    }

    trait Area {
      val w: Width
      val h: Height

      def area = w.w * h.h
    }

    case class Size(w: Width, h: Height) extends Area {
      def on(o: Point) = Rect(o, w, h)

      lazy val toOpenCV = new org.opencv.core.Size(w.w, h.h)

      def toRect = on(Point(X(0), Y(0)))
    }

    case class Rect(o: Point, w: Width, h: Height) extends Area {
      lazy val corners = Seq(
        o,
        o + w.toPoint,
        o + w.toPoint + h.toPoint,
        o + h.toPoint
      )
      lazy val toOpenCV = new MatOfPoint(corners.map(_.toOpenCV): _*)
      lazy val size = new Size(w, h)
    }

    case class WidthToHeightRatio(r: Double)

    case class HeightToWidthRatio(r: Double)

  }

  import TypeSafeWidthHeight._

  object MeasuresToOpenCV {
    def fromMeasures(src: Seq[Rect], anchorInMeasures: Rect, anchorInOpenCV: MatOfPoint): Seq[MatOfPoint] = {
      import ImageProcessing._
      assert(anchorInOpenCV.size == 4)
      val H = findHomography(anchorInMeasures.toOpenCV, anchorInOpenCV)
      warpContours(src.map(_.toOpenCV), H)
    }
  }

}


abstract class AnswerMatrixMeasures(questionsO: Option[Int], val columns: Int, val possibleAnswers: Int) extends LazyLogging {

  val cellCorrector: CellCorrector

  lazy val questions = questionsO.getOrElse(throw new IllegalStateException("Number of questions not known"))

  lazy val rows = (1.0 * questions / columns).ceil.toInt

  def rowOfQuestion(question: Int): Int

  def columnOfQuestion(question: Int): Int

  trait Params {

    import AnswerMatrixMeasures.TypeSafeWidthHeight._

    val cellHeaderSize: Size
    val answerCellAvailableWidth: Width
    // INCLUDING cellHeaderToCellWidthGap, cellSize AND THE FOLLOWING SPACE
    val cellHeaderToCellWidthGap: Width
    val cellSize: Size
    val answerTableOrigin: Point
  }

  val params: Params

  import params._

  private lazy val columnWidth = cellHeaderSize.w + answerCellAvailableWidth
  private lazy val answerTableWidth = columnWidth * columns
  private lazy val answerTableHeight = cellHeaderSize.h * rows

  private lazy val answerTableSize = Size(answerTableWidth, answerTableHeight)

  lazy val answerTableRect = answerTableOrigin.toRect(answerTableSize)

  lazy val cellArea = cellSize.area


  lazy val answerCells = {

    object ValuesForAnswerCell {
      val basePointForCells = answerTableOrigin + Point((cellHeaderSize.w + cellHeaderToCellWidthGap).toX, Y(0))
      val columnOffset = Point((answerCellAvailableWidth + cellHeaderSize.w).toX, Y(0))
      val rowOffset = Point(X(0), cellSize.h.toY)
      logger.error(s"rowOffset:$rowOffset")
    }

    def answerCell(question: Int) = {
      assert(question >= 0 && question < questions)

      val row = rowOfQuestion(question)
      val column = columnOfQuestion(question)

      import ValuesForAnswerCell._

      cellSize.on(basePointForCells + rowOffset * row + columnOffset * column)
    }

    (0 until questions).map(answerCell)
  }

}

trait HorizontalMeasures {
  self: AnswerMatrixMeasures =>
  def rowOfQuestion(question: Int) = question / columns

  def columnOfQuestion(question: Int) = question % columns

}

trait VerticalMeasures {
  self: AnswerMatrixMeasures =>


  def rowOfQuestion(question: Int) = question % rows

  def columnOfQuestion(question: Int) = question / rows
}

trait TickedMeasures {
  self: AnswerMatrixMeasures =>
  lazy val cellCorrector = new LetterCellCorrector(possibleAnswers) //new TickedCellCorrector(possibleAnswers)

  val params = new Params {
    private val f = 5
    val cellHeaderSize = Size(Width(24 * f), Height(14 * f))
    val answerCellAvailableWidth = Width(67 * f)  // INCLUDING cellHeaderToCellWidthGap, cellSize AND THE FOLLOWING SPACE
    val cellHeaderToCellWidthGap = Width(2 * f)
    val cellSize = Size(Width(52 * f), cellHeaderSize.h)
    val answerTableOrigin = Point(X(0), Y(0))
  }

}

trait LetterMeasures {
  self: AnswerMatrixMeasures =>
  lazy val cellCorrector = new LetterCellCorrector(possibleAnswers)

  val params = new Params {
    private val f = 1
    val cellHeaderSize = Size(Width(65 * f), Height(28 * f))
    val answerCellAvailableWidth = Width(152 * f)  // INCLUDING cellHeaderToCellWidthGap, cellSize AND THE FOLLOWING SPACE
    val cellHeaderToCellWidthGap = Width(12 * f)
    val cellSize = Size(Width(100 * f), cellHeaderSize.h)
    val answerTableOrigin = Point(X(0), Y(0))
  }
}

class AnswerMatrixMeasuresHorizontalTicked(questionsO: Option[Int], columns: Int = 5, possibleAnswers: Int = 4)
  extends AnswerMatrixMeasures(questionsO, columns, possibleAnswers)
    with HorizontalMeasures
    with TickedMeasures {


}

class AnswerMatrixMeasuresVerticalTicked(questionsO: Option[Int], columns: Int = 5, possibleAnswers: Int = 4)
  extends AnswerMatrixMeasures(questionsO, columns, possibleAnswers)
    with VerticalMeasures
    with TickedMeasures {

}

class AnswerMatrixMeasuresHorizontalLetter(questionsO: Option[Int], columns: Int = 5, possibleAnswers: Int = 4)
  extends AnswerMatrixMeasures(questionsO, columns, possibleAnswers)
    with HorizontalMeasures
    with LetterMeasures {



}

class AnswerMatrixMeasuresVerticalLetter(questionsO: Option[Int], columns: Int = 5, possibleAnswers: Int = 4)
  extends AnswerMatrixMeasures(questionsO, columns, possibleAnswers)
    with VerticalMeasures
    with LetterMeasures {




}
