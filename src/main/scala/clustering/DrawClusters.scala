package clustering

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scala.collection.mutable
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
object DrawClusters extends JFXApp {
  val xs = mutable.Buffer[(Double, Double)]()
  val ms = mutable.Buffer[(Double, Double)]()

  private var mode: KeyCode = KeyCode.X
  private var selected = -1
  val colors = Vector(Color.Red, Color.Blue, Color.Green, Color.Yellow, Color.Cyan, Color.Magenta)

  stage = new JFXApp.PrimaryStage {
    title = "Clustering"
    scene = new Scene(800, 800) {
      val canvas = new Canvas(800, 800)
      val gc = canvas.graphicsContext2D
      content = canvas

      onKeyPressed = (ke: KeyEvent) => { 
        if (ke.code == KeyCode.C) {
          xs.clear()
          ms.clear()
          draw(gc)
        } else if (ke.code == KeyCode.J) {
          val groups = xs.groupBy{ case (x,y) => if (ms.isEmpty) 0 else ms.indices.minBy(i => dist(x, y, ms(i)._1, ms(i)._2))}
          for (i <- ms.indices) {
            ms(i) = (groups(i).map(_._1).sum / groups(i).length, groups(i).map(_._2).sum / groups(i).length)
          }
          draw(gc)
        } else mode = ke.code
      }

      onMouseClicked = (me: MouseEvent) => {
        mode match {
          case KeyCode.M =>
            ms += me.x -> me.y
          case KeyCode.X =>
            xs ++= (1 to 10).map(_ => (me.x + util.Random.nextGaussian()*20, me.y + util.Random.nextGaussian()*20))
          case _ =>
            if (ms.nonEmpty) selected = ms.indices.minBy(i => dist(me.x, me.y, ms(i)._1, ms(i)._2))
        }
        draw(gc)
      }

      onMouseDragged = (me: MouseEvent) => {
        if (selected >=0 && selected < ms.length) {
          ms(selected) = me.x -> me.y
        }
        draw(gc)
      }
    }
  }

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 0, 800, 800)
    gc.lineWidth = 3
    
    for ((x, y) <- xs) {
      val i = if (ms.isEmpty) 0 else ms.indices.minBy(i => dist(x, y, ms(i)._1, ms(i)._2))
      gc.fill = colors(i % colors.length)
      gc.fillOval(x-3, y-3, 6, 6)
    }
    for (((x, y), i) <- ms.zipWithIndex) {
      gc.fill = colors(i % colors.length)
      gc.stroke = colors(i % colors.length)
      gc.strokeOval(x-5, y-5, 10, 10)
      if (i == selected) gc.fillOval(x-3, y-3, 6, 6)
    }
  }

  def dist(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val dx = x1-x2
    val dy = y1-y2
    math.sqrt(dx*dx + dy*dy)
  }
}