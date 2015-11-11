import java.awt.event.{MouseEvent, MouseAdapter}
import java.awt._
import java.awt.image.BufferedImage
import java.util
import javax.swing.{JPanel}

/**
 * Created by alvaro on 18/10/15.
 */
object ImageCanvas{
  lazy val defaultImage = {
    val w = 320
    val h = 200
    val image = new BufferedImage(w,h,BufferedImage.TYPE_INT_RGB)
    val g = image.getGraphics
    g.setColor(Color.black)
    g.drawRect(0,0,w,h)
    g.setColor(Color.blue)
    g.drawString("No image",0,h/2)
    g.dispose()
    image
  }
}

class ImageCanvas( img: Image = ImageCanvas.defaultImage ) extends JPanel{

  def imageWidth = image.getWidth(null)
  def imageHeight = image.getHeight(null)

  private var _image = img


  def image_=(img: Image) : Unit = {
    _image = img
    setPreferredSize( new Dimension(imageWidth,imageHeight))
    repaint()
  }

  def image : Image = _image

  image_=(img)


  setDoubleBuffered(true)

  override def paintComponent(g: Graphics) = {
    import scala.collection.JavaConversions._
    g.drawImage(_image,0,0,getWidth,getHeight,null)
    for( p <- puntosDeCirculos ) drawCircles(p,g)
  }

  val puntosDeCirculos = new util.ArrayList[(Int,Int)]

  addMouseListener( new MouseAdapter(){

    override def mouseClicked(e: MouseEvent): Unit ={
      println( "Me han pinchado en:" + e )

      val p: (Int, Int) = (e.getX, e.getY)
      puntosDeCirculos.add( p )
      drawCircles(p,getGraphics)
    }
  })

  def drawCircles( p: (Int,Int), g: Graphics ) {
    g.setColor(Color.pink)
    val radio = 20
    val x = p._1
    val y = p._2
    g.drawArc(x - radio, y - radio, radio * 2, radio * 2, 0, 360)
  }
}