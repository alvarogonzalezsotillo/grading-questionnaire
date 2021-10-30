package imgproc

import java.awt.{FlowLayout, GridLayout, Image}
import javax.imageio.ImageIO
import javax.swing.{JFrame, ImageIcon, JLabel, JPanel}

import imgproc.ocr.{OneLetterOCR, TrainedOneLetterOCR}
import org.opencv.core.{Rect, Scalar, MatOfPoint, Mat}



/**
 * Created by alvaro on 21/04/16.
 */
class OCRPanel(cell: Mat) extends JPanel {

  import OneLetterOCR._
  import Implicits._

  case class ImageView[A]( image: A )(implicit ev: A => Image) extends JLabel( new ImageIcon(image) )

  case class LabelAndImagesPanel[A]( label: String, images: Seq[A] )(implicit ev: A=> Image) extends JPanel {
    setLayout( new FlowLayout(FlowLayout.LEFT,4,4) )
    add( new JLabel(label) )
    images.foreach( i => add( ImageView(i) ) )
  }


  setLayout(new GridLayout(10, 1))

  add( LabelAndImagesPanel("Original cell:", Seq(cell) ) )
  private val thresh = thresholdLettersImage(cell)
  add( LabelAndImagesPanel("Threshold:", Seq(thresh)))

  val contours = findContoursOfLetterFragment(thresh)

  add( LabelAndImagesPanel("Contours:", Seq(cellWithContours(cell,contours))))



  val bboxes = extractPossibleLettersBBox(cell)

  def cellWithContours( m: Mat, contours: Seq[MatOfPoint] ) = {
    val copy = m.clone()
    ImageProcessing.drawContours(copy,contours,new Scalar(255,0,255),1)
    copy
  }

  add( LabelAndImagesPanel("Boxes:", Seq(cellWithContours(cell, bboxes.map(_.asShape)))))

  val candidates = extractPossibleLettersImage(cell)
  add( LabelAndImagesPanel("Letter candidates:", candidates ) )
  add( LabelAndImagesPanel("Letter candidates normalized:", candidates.map(normalizeLetter(_)) ) )

  for( c <- candidates ){
    val again = extractPossibleLettersImage(c,0)
    add( LabelAndImagesPanel("Candidate again:", Seq(c) ++ again ++ again.map(normalizeLetter(_))) )
  }

}

object OCRPanel extends App{
  import java.io.File
  import Implicits._

  nu.pattern.OpenCV.loadLibrary()

  val file = new File("/home/alvaro/github/grading-questionnaire/blob-detect/A-con-ruido.png")

  val frame = new JFrame(file.getName)
  frame.add( new OCRPanel(ImageIO.read(file) ) )
  frame.pack()
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)
}
