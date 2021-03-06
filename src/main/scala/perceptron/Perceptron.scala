package perceptron

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.SplitPane
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.TableView
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.cell.TextFieldTableCell
import scalafx.util.StringConverter
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import javafx.scene.control.Button
import scalafx.scene.layout.HBox

case class DataRow(inputs: Array[Double], outputs: Array[Double])
case class Weights(w: Array[Double])

object Perceptron extends JFXApp {
  val inputs = 2
  val outputs = 1
  private var eta = 0.01
  private var epoch = 0
  private var learnData = Array[DataRow]()

  val ios = ObservableBuffer(
    DataRow(Array(0.0, 0.0), Array(0.0)),
    DataRow(Array(1.0, 0.0), Array(0.0)),
    DataRow(Array(0.0, 1.0), Array(0.0)),
    DataRow(Array(1.0, 1.0), Array(1.0))
  )
  val weights = ObservableBuffer(
    Seq.fill(outputs)(
      Weights(Array.fill(inputs + 1)((math.random - 0.5) * 0.02))
    ): _*
  )
  val canvas = new Canvas(2000, 2000)
  val gc = canvas.graphicsContext2D

  stage = new JFXApp.PrimaryStage {
    title = "Perceptron"
    scene = new Scene(1200, 1000) {
      val splitPane = new SplitPane
      val addDataButton = new Button("Add Data")
      addDataButton.onAction = (ae) => {
        ios += DataRow(Array.fill(inputs)(0.0), Array.fill(outputs)(0.0))
        dataTable.refresh()
      }
      val removeDataButton = new Button("Remove Data")
      removeDataButton.onAction = (ae) => {
        ios += DataRow(Array.fill(inputs)(0.0), Array.fill(outputs)(0.0))
        dataTable.refresh()
      }
      val inputBox = new HBox(addDataButton, removeDataButton)
      val dataTable = new TableView(ios)
      val inputColumns = (0 until inputs).map { i =>
        val col = new TableColumn[DataRow, Double](s"x ${i + 1}")
        col.cellValueFactory = dr => ObjectProperty(dr.value.inputs(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[DataRow, Double](
          new StringConverter[Double] {
            def fromString(string: String): Double = string.toDouble
            def toString(t: Double): String = t.toString
          }
        )
        col.onEditCommit = (event) => {
          ios(dataTable.selectionModel.value.getSelectedIndex()).inputs(i) =
            event.getNewValue()
          drawData()
        }
        col
      }
      val outputColumns = (0 until outputs).map { i =>
        val col = new TableColumn[DataRow, Double](s"r ${i + 1}")
        col.cellValueFactory = dr => ObjectProperty(dr.value.outputs(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[DataRow, Double](
          new StringConverter[Double] {
            def fromString(string: String): Double = string.toDouble
            def toString(t: Double): String = t.toString
          }
        )
        col.onEditCommit = (event) => {
          ios(dataTable.selectionModel.value.getSelectedIndex()).outputs(i) =
            event.getNewValue()
          drawData()
        }
        col
      }
      val resultColumns = (0 until outputs).map { i =>
        val col = new TableColumn[DataRow, Double](s"Output ${i + 1}")
        col.cellValueFactory = dr =>
          ObjectProperty(calcYs(dr.value.inputs).head)
        col
      }
      dataTable.columns ++= inputColumns.map(_.delegate) ++ outputColumns.map(
        _.delegate
      ) ++ resultColumns.map(_.delegate)
      dataTable.editable = true
      val weightTable = new TableView(weights)
      val weightColumns = (0 until inputs + 1).map { i =>
        val col = new TableColumn[Weights, Double](s"Weight ${i}")
        col.cellValueFactory = w => ObjectProperty(w.value.w(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[Weights, Double](
          new StringConverter[Double] {
            def fromString(string: String): Double = string.toDouble
            def toString(t: Double): String = t.toString
          }
        )
        col.onEditCommit = (event) => {
          weights(weightTable.selectionModel.value.getSelectedIndex()).w(i) =
            event.getNewValue()
          dataTable.refresh()
          drawData()
        }
        col
      }
      weightTable.editable = true
      weightTable.columns ++= weightColumns.map(_.delegate)
      val learnButton = new Button("Learn")
      learnButton.onAction = (ae) => {
        learnOne()
        dataTable.refresh()
        weightTable.refresh()
        drawData()
      }
      val learnButton2 = new Button("Learn 10")
      learnButton2.onAction = (ae) => {
        for (_ <- 1 to 10) learnOne()
        dataTable.refresh()
        weightTable.refresh()
        drawData()
      }
      val learnButton3 = new Button("Learn 100")
      learnButton3.onAction = (ae) => {
        for (_ <- 1 to 100) learnOne()
        dataTable.refresh()
        weightTable.refresh()
        drawData()
      }
      val learnBox = new HBox(learnButton, learnButton2, learnButton3)
      val vBox = new VBox(inputBox, dataTable, weightTable, learnBox) //scrollPane1a, scrollPane1b)
      splitPane.items += vBox
      val scrollPane2 = new ScrollPane
      scrollPane2.content = canvas
      splitPane.items += scrollPane2
      root = splitPane
      drawData()
    }
  }

  def sigmoid(x: Double): Double = 1.0 / (1.0 + math.exp(-x))

  def calcYs(inputs: Seq[Double]): Seq[Double] = {
    (0 until outputs).map(
      i => sigmoid((1.0 +: inputs, weights(i).w).zipped.map(_ * _).sum)
    )
  }

  def learnOne(): Unit = {
    if (learnData.isEmpty) {
      epoch += 1
      learnData = ios.toArray
    }
    val index = util.Random.nextInt(learnData.length)
    val ys = calcYs(learnData(index).inputs)
    val augmentedInputs = 1.0 +: learnData(index).inputs
    for (o <- 0 until outputs; i <- 0 until inputs + 1) {
      weights(o).w(i) += eta * (learnData(index).outputs(o) - ys(o)) * augmentedInputs(i)
    }
    learnData = learnData.patch(index, Nil, 1)
  }

  def drawData(): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 0, 2000, 2000)
    val scale = 100
    val cx = 300
    val cy = 500
    for (i <- 0 until 500; j <- 0 until 500) {
      val x = (i - 250) / 20.0
      val y = (j - 250) / 20.0
      val out = calcYs(Array(x, y).padTo(inputs, 0.0)).head
      val c = out max 0.0 min 1.0
      gc.fill = Color(c, 1.0 - c, 0.0, 1.0)
      gc.fillRect(cx + x * scale, cy - y * scale, 5, 5)
    }
    gc.stroke = Color.Black
    gc.strokeLine(0, cy, 2000, cy)
    gc.strokeLine(cx, 0, cx, 2000)
    for (io <- ios) {
      val c = io.outputs(0) max 0.0 min 1.0
      gc.fill = Color(c, c, c, 1.0)
      gc.fillOval(
        io.inputs(0) * scale + cx - 5,
        cy - io.inputs(1) * scale - 5,
        10,
        10
      )
      gc.strokeOval(
        io.inputs(0) * scale + cx - 5,
        cy - io.inputs(1) * scale - 5,
        10,
        10
      )
    }
  }
}
