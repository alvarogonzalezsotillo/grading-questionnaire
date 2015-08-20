package experiments

;

import java.awt.Graphics
import java.awt.event.{KeyEvent, KeyListener}
import javax.swing._

import experiments.Properties.RWProp

/**
 * Created by alvaro on 16/08/15.
 */
object CirculosFord extends App {

  implicit def toKeyListener(l: (Int) => Unit) = new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {}
    override def keyPressed(e: KeyEvent): Unit = l(e.getKeyCode)
    override def keyReleased(e: KeyEvent): Unit = {}
  }

  object Frame extends JFrame {

    val x0 = RWProp(0.0)
    val y0 = RWProp(0.0)
    val zoom = RWProp(1000.0)

    Seq(x0, y0, zoom).foreach(_.addListener(repaint()))

    addKeyListener { k: Int =>
      k match {
        case KeyEvent.VK_UP => y0() = y0() + 10.0
        case KeyEvent.VK_DOWN => y0() = y0() - 10.0
        case KeyEvent.VK_LEFT => x0() = x0() + 10.0
        case KeyEvent.VK_RIGHT => x0() = x0() - 10.0
        case KeyEvent.VK_PLUS => zoom() = zoom() + 10.0
        case KeyEvent.VK_MINUS => zoom() = zoom() - 10.0
        case _ =>
      }
    }

    def mcd(a: Int, b: Int) : Int = {
      println( s"$a $b" )
      a min b match {
        case 0 => a max b
        case _ => mcd(a min b, (a max b) % (a min b))
      }
    }

    override def paint(g: Graphics) = {

      def circle(x: Double, y: Double, r: Double) = {
        import Properties.implicits._
        val zx = x0 + zoom * x
        val zy = y0 + zoom * y
        val zr = zoom * r
        val w = (zr*2).toInt
        g.drawArc((zx-zr).toInt, (zy-zr).toInt, w,w, 0, 360)
        //g.drawRect((zx-zr).toInt,(zy-zr).toInt,w,w)
        //g.drawString( s"$x,$y,$r", zx.toInt, zy.toInt)
      }

      g.setColor(getBackground)
      g.fillRect(0, 0, getWidth, getHeight)

      g.setColor(getForeground)
      g.drawString(s"x:${x0()} y:${y0()} zoom:${zoom()}", 0, getHeight - 20)

      for (p <- 0 to 100; q <- 1 to 100 if mcd(p,q) == 1 ) {
        val x: Double = 1.0 * p / q
        val y: Double = 1.0 / (2 * q * q)
        val r = y
        circle(x, y, r)
      }
    }
  }

  Frame.setVisible(true)
}
