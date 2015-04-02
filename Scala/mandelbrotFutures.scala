/*
Authored by Bryan Schrock in 2015.

This program generates the mandelbrot set in parallel using Futures. 
It is interactive and allwos the user to zoom and navigate freely
until the values are too long for Scala's Double.

Controls:
Mouse Wheel to zoom
Arrow Keys to navigate
page up to increase "MaxIterations" and enable more detail at closer depth
page down to reduce "MaxIterations"

Enjoy!
*/


import java.awt.RenderingHints._
import java.awt.image.BufferedImage
import scala.swing.event.Key

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.swing.event.KeyPressed
import scala.swing.event.MouseMoved
import scala.swing.event.MouseWheelMoved
import scala.util.Failure
import scala.util.Success
import java.awt.Color;

object MandelbrotFutures2 {

  val w = 1000
  val h = 1000
  var scrollSpeed = 0.1
  var moveSpeed = 0.1
  var rmax: Double = .5
  var rmin: Double = -1.5
  var imin: Double = -1.0
  var imax: Double = 1.0
  var typed = ""

  val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  val rand = scala.util.Random
  var MaxIterations = rand.nextInt(30) + 30

  def mandelCount(c: Complex2, maxIters: Int): Int = {
    var z = new Complex2(0, 0)
    var cnt = 0
    while (z.magSqr < 16 && cnt < maxIters) {
      cnt += 1
      z = z * z + c
    }
    cnt
  }

  def getColor(iters: Int): Int = {
    if (iters == MaxIterations) Color.black.getRGB //white
    else {
      iters * (-16777216.0 / MaxIterations.toDouble).toInt //Arbitrary seeming number is "Color.black.getRGB"
    }
  }

  def drawMandel() {
		val numP = 500
    val iters = Array.fill(img.getWidth, img.getHeight)(0)
    for (t <- 0 until numP) {
      val fRow: Future[Array[Array[Int]]] = Future {
        val ret = Array.fill(w / numP, h)(0)
        val lowerBound = (w.toDouble / numP.toDouble) * t
        val higherBound = (w.toDouble / numP) * t + (w.toDouble / numP)

        for (i <- lowerBound.toInt until higherBound.toInt) {
          val i2 = i % (w / numP)
          for (j <- 0 until img.getHeight) {
            val c = new Complex2(rmin + i * (rmax - rmin) / img.getWidth, imin + j * (imax - imin) / img.getHeight)
            ret(i2)(j) = mandelCount(c, MaxIterations)
            img.setRGB(i, j, getColor(ret(i2)(j)));
          }
        }
        ret
      }
      fRow onComplete {
        case Failure(a) => println("Failed! " + a.getMessage)
        case Success(a) => {drawPanel.repaint }
      }

    }
  }
  def convertXToR(x: Int) = ((x.toDouble / w.toDouble) * (rmax - rmin)) + rmin
  val drawPanel = new Panel {
    focusable = true
    requestFocus
    preferredSize = new Dimension(w, h);
    var x, y = 0
    var zoom = 1
    listenTo(mouse.moves, mouse.wheel, keys)
    reactions += {
      case mv: MouseMoved => {
        x = mv.point.x
        y = mv.point.y
        //println(convertXToR(x))
        // println("(" + x + ", " + y + ")")
      }
      case mw: MouseWheelMoved => {
        if (mw.rotation == -1) {
          rmin += scrollSpeed
          rmax -= scrollSpeed
          imin += scrollSpeed
          imax -= scrollSpeed
          scrollSpeed *= 1- (scrollSpeed*2)/(rmax-rmin)
          moveSpeed *= 1- (moveSpeed*2)/(rmax-rmin)
        } else if (mw.rotation == 1) {
          rmin -= scrollSpeed
          rmax += scrollSpeed
          imin -= scrollSpeed
          imax += scrollSpeed
          scrollSpeed /= 1- (scrollSpeed*2)/(rmax-rmin)
          moveSpeed /= 1- (moveSpeed*2)/(rmax-rmin)
        }
        drawMandel()
      }
      case kp: KeyPressed => {
        if (kp.key == Key.Left) {
          rmin -= moveSpeed
          rmax -= moveSpeed
          drawMandel
        } else if (kp.key == Key.Down) {
          imin += moveSpeed
          imax += moveSpeed
          drawMandel
        } else if (kp.key == Key.Right) {
          rmin += moveSpeed
          rmax += moveSpeed
          drawMandel
        } else if (kp.key == Key.Up) {
          imin -= moveSpeed
          imax -= moveSpeed
          drawMandel
        } else if (kp.key == Key.PageUp) {
          MaxIterations = (MaxIterations.toDouble * 1.2).toInt
          drawMandel
        } else if (kp.key == Key.PageDown) {
          MaxIterations = (MaxIterations.toDouble / 1.2).toInt
          drawMandel
        } else if(kp.key == Key.R){
          MaxIterations = 50
          rmin = -1.5
          rmax = .5
          imin = -1
          imax = 1
          moveSpeed = .1
          scrollSpeed = .1
          drawMandel
        }

        try {
          val num = kp.key.toString.toInt
          typed += num.toString
          //println(typed)
        } catch {
          case nfe: NumberFormatException => {
            if (kp.key == Key.Enter) {
              MaxIterations = typed.toInt
              typed = ""
              drawMandel
            }
          }
        }
      }
    }
    override def paint(g: Graphics2D) {
      g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
      setTitle
      g.drawImage(img, 0, 0, null);
    }
  }
  def setTitle() {
    mainframe.title = "Madnelbrot Set with Max Iterations = " + MaxIterations.toString
  }
  val mainframe = new MainFrame {
    contents = drawPanel;
    title = "Mandelbrot Set with Max Iterations = " + MaxIterations.toString
    centerOnScreen;
  }

  def main(args: Array[String]) {
    mainframe.open
    drawMandel();

  }
}
MandelbrotFutures2.drawMandel();
MandelbrotFutures2.mainframe.open
class Complex2(var r: Double, var i: Double) {
  def +(o: Complex2): Complex2 = new Complex2(r + o.r, i + o.i)
  def -(o: Complex2): Complex2 = new Complex2(r - o.r, i - o.i)
  def *(o: Complex2): Complex2 = new Complex2(r * o.r - i * o.i, r * o.i + i * o.r)
  def update(o: Complex2): Complex2 = {
    r = o.r;
    i = o.i;
    this
  }
  def stringFormat: String = r + "+i" + i
  def mag: Double = math.sqrt(r * r + i * i)
  def magSqr: Double = r * r + i * i
}

