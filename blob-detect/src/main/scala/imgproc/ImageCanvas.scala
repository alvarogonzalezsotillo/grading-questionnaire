package imgproc

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.util
import javax.swing.JPanel

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

  def computeBestFit = {
    val imgW = _image.getWidth(null)
    val imgH = _image.getHeight(null)
    val w = getWidth
    val h = getHeight

    val imgRatio = 1.0*imgW/imgH
    val ratio = 1.0*w/h
    val imageIsWider = imgRatio > ratio

    if( imageIsWider ){
      val scale = 1.0*w/imgW
      val dstH = imgH*scale
      val y = (h - dstH)/2
      new Rectangle( 0, y.toInt, w, dstH.toInt)
    }
    else{
      val scale = 1.0*h/imgH
      val dstW = imgW*scale
      val x = (w - dstW)/2
      new Rectangle(x.toInt,0,dstW.toInt,h)
    }
  }

  override def paintComponent(g: Graphics) = {
    import scala.collection.JavaConversions._
    val r = computeBestFit
    g.setColor(getBackground())
    g.fillRect(0,0,getWidth(),getHeight())
    g.drawImage(_image,r.x,r.y,r.width,r.height,null)
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
