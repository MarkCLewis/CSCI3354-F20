package decisiontrees

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import collection.mutable
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode

case class PointClass(x: Double, y: Double, c: Int)
class TreeNode(var splitDim: Int, var splitVal: Double, var left: Option[TreeNode], var right: Option[TreeNode])

object UnivariateTree extends JFXApp {
  val canvas = new Canvas(1000, 1000)
  val gc = canvas.graphicsContext2D
  val data = mutable.Buffer[PointClass]()
  val colors = Vector(Color.Red, Color.Green, Color.Blue, Color.Cyan, Color.Magenta, Color.Yellow)
  private var mode = 0
  val treeRoot = new TreeNode(0, 500, None, None)
  private var selectedNode = treeRoot

  stage = new JFXApp.PrimaryStage {
    title = "Univariate Decision Tree"
    scene = new Scene(1000, 1000) {
      content = canvas

      onMouseClicked = (me: MouseEvent) => {
        if (colors.indices.contains(mode)) {
          data ++= Array.fill(10)(PointClass(me.x + 10 * util.Random.nextGaussian(), me.y + 10 * util.Random.nextGaussian(), mode))
        } else if (mode == -1) {
          selectedNode = selectNode(treeRoot, me.x, me.y)
        } else if (mode == -2) {
          selectedNode.splitVal = if (selectedNode.splitDim == 0) me.x else me.y
        }
        draw()
      }
      onMouseDragged = (me: MouseEvent) => {
        if (colors.indices.contains(mode)) {
          data ++= Array.fill(10)(PointClass(me.x + 10 * util.Random.nextGaussian(), me.y + 10 * util.Random.nextGaussian(), mode))
        } else if (mode == -2) {
          selectedNode.splitVal = if (selectedNode.splitDim == 0) me.x else me.y
        }
        draw()
      }
      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Digit1 => mode = 0
          case KeyCode.Digit2 => mode = 1
          case KeyCode.Digit3 => mode = 2
          case KeyCode.Digit4 => mode = 3
          case KeyCode.Digit5 => mode = 4
          case KeyCode.Digit6 => mode = 5
          case KeyCode.X => selectedNode.splitDim = 0
          case KeyCode.Y => selectedNode.splitDim = 1
          case KeyCode.S => mode = -1 // Split/select node
          case KeyCode.M => mode = -2 // Move the split
          case KeyCode.Delete => data.clear()
          case KeyCode.BackSpace => 
            treeRoot.left = None
            treeRoot.right = None
            selectedNode = treeRoot
          case _ =>
        }
        draw()
      }
    }
  }

  def draw(): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width(), canvas.height())
    for (pc <- data) {
      gc.fill = colors(pc.c)
      gc.fillOval(pc.x - 3, pc.y - 3, 6, 6)
    }
    drawTree(treeRoot, 0, 1000, 0, 1000)
  }

  def drawTree(node: TreeNode, minx: Double, maxx: Double, miny: Double, maxy: Double): Unit = {
    if (node == selectedNode) {
      gc.fill = Color(0.5, 0.5, 0.5, 0.3)
      gc.fillRect(minx, miny, maxx - minx, maxy - miny)
      gc.fill = Color.Black
      val localData = data.filter(pc => pc.x >= minx && pc.x <= maxx && pc.y >= miny && pc.y <= maxy)
      val (leftData, rightData) = localData.partition(pc => if (node.splitDim == 0) pc.x < node.splitVal else pc.y < node.splitVal)
      val entropy = colors.indices.map { i => 
        val piLeft = leftData.count(_.c == i).toDouble
        val entropyLeft = if (leftData.length == 0 || piLeft == 0) 0.0 else -piLeft/leftData.length * math.log(piLeft/leftData.length)/math.log(2)
        val piRight = rightData.count(_.c == i).toDouble
        val entropyRight = if (rightData.length == 0 || piRight == 0) 0.0 else -piRight/rightData.length * math.log(piRight/rightData.length)/math.log(2)
        entropyLeft + entropyRight
      }.sum
      gc.fillText(entropy.toString(), 20, 50)
    }
    if (node.splitDim == 0) {
      gc.strokeLine(node.splitVal, miny, node.splitVal, maxy)
      node.left.foreach(left => drawTree(left, minx, node.splitVal, miny, maxy))
      node.right.foreach(left => drawTree(left, node.splitVal, maxx, miny, maxy))
    } else {
      gc.strokeLine(minx, node.splitVal, maxx, node.splitVal)
      node.left.foreach(left => drawTree(left, minx, maxx, miny, node.splitVal))
      node.right.foreach(left => drawTree(left, minx, maxx, node.splitVal, maxy))
    }
  }

  def selectNode(node: TreeNode, x: Double, y: Double): TreeNode = {
    if (node.splitDim == 0) {
      if ((x-node.splitVal).abs < 3) node
      else if (x < node.splitVal) {
        if (node.left.isEmpty) {
          node.left = Some(new TreeNode(0, x, None, None))
          node.left.get
        } else selectNode(node.left.get, x, y)
      } else {
        if (node.right.isEmpty) {
          node.right = Some(new TreeNode(0, x, None, None))
          node.right.get
        } else selectNode(node.right.get, x, y)
      }
    } else {
      if ((y-node.splitVal).abs < 3) node
      else if (y < node.splitVal) {
        if (node.left.isEmpty) {
          node.left = Some(new TreeNode(0, x, None, None))
          node.left.get
        } else selectNode(node.left.get, x, y)
      } else {
        if (node.right.isEmpty) {
          node.right = Some(new TreeNode(0, x, None, None))
          node.right.get
        } else selectNode(node.right.get, x, y)
      }
    }
  }
}