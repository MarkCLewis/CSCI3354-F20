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

case class DataRow(inputs: Array[Double], outputs: Array[Double])
case class Weights(w: Array[Double])

object Perceptron extends JFXApp {
  private var inputs = 2
  private var outputs = 1
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
    Seq.fill(outputs)(Weights(Array.fill(inputs+1)((math.random-0.5)*0.2))) :_*
  )
  val canvas = new Canvas(2000, 2000)
  val gc = canvas.graphicsContext2D

  stage = new JFXApp.PrimaryStage {
    title = "Preceptron"
    scene = new Scene(1200, 1000) {
      val splitPane = new SplitPane
      val dataTable = new TableView(ios)
      val inputColumns = (0 until inputs).map { i => 
        val col = new TableColumn[DataRow, Double](s"x ${i+1}")
        col.cellValueFactory = dr => ObjectProperty(dr.value.inputs(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[DataRow, Double](new StringConverter[Double] {
          def fromString(string: String): Double = string.toDouble
          def toString(t: Double): String = t.toString
        })
        col.onEditCommit = (event) => {
          ios(dataTable.selectionModel.value.getSelectedIndex()).inputs(i) = event.getNewValue()
          drawData()
        }
        col
      }
      val outputColumns = (0 until outputs).map { i => 
        val col = new TableColumn[DataRow, Double](s"r ${i+1}")
        col.cellValueFactory = dr => ObjectProperty(dr.value.outputs(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[DataRow, Double](new StringConverter[Double] {
          def fromString(string: String): Double = string.toDouble
          def toString(t: Double): String = t.toString
        })
        col.onEditCommit = (event) => {
          ios(dataTable.selectionModel.value.getSelectedIndex()).outputs(i) = event.getNewValue()
          drawData()
        }
        col
      }
      val resultColumns = (0 until outputs).map { i => 
        val col = new TableColumn[DataRow, Double](s"Output ${i+1}")
        col.cellValueFactory = dr => ObjectProperty((1.0 +: dr.value.inputs, weights(i).w).zipped.map(_ * _).sum)
        col
      }
      dataTable.columns ++= inputColumns.map(_.delegate) ++ outputColumns.map(_.delegate) ++ resultColumns.map(_.delegate)
      dataTable.editable = true
      // val scrollPane1a = new ScrollPane
      // scrollPane1a.content = dataTable
      val weightTable = new TableView(weights)
      val weightColumns = (0 until inputs+1).map { i => 
        val col = new TableColumn[Weights, Double](s"Weight ${i}")
        col.cellValueFactory = w => ObjectProperty(w.value.w(i))
        col.cellFactory = TextFieldTableCell.forTableColumn[Weights, Double](new StringConverter[Double] {
          def fromString(string: String): Double = string.toDouble
          def toString(t: Double): String = t.toString
        })
        col.onEditCommit = (event) => {
          weights(weightTable.selectionModel.value.getSelectedIndex()).w(i) = event.getNewValue()
          dataTable.refresh()
          drawData()
        }
        col
      }
      weightTable.editable = true
      weightTable.columns ++= weightColumns.map(_.delegate)
      // val scrollPane1b = new ScrollPane
      // scrollPane1b.content = weightTable
      val learnButton = new Button("Learn")
      learnButton.onAction = (ae) => {
        if (learnData.isEmpty) {
          epoch += 1
          learnData = ios.toArray
        }
        val index = util.Random.nextInt(learnData.length)
        val ys = (0 until outputs).map(i => (1.0 +: learnData(index).inputs, weights(i).w).zipped.map(_ * _).sum)
        for (o <- 0 until outputs; i <- 0 until inputs+1) {
          weights(o).w(i) += eta*(learnData(index).outputs(o)-y(o))*learnData(index).inputs(i)
        }
        learnData = learnData.patch(index, Nil, 1)
        dataTable.refresh()
        drawData()
      }
      val vBox = new VBox(dataTable, weightTable) //scrollPane1a, scrollPane1b)
      splitPane.items += vBox
      val scrollPane2 = new ScrollPane
      scrollPane2.content = canvas
      splitPane.items += scrollPane2
      root = splitPane
      drawPerceptron()
      drawData()
    }
  }

  def drawPerceptron(): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 0, 2000, 250)
    for (o <- 0 until outputs; i <- 0 until inputs) {
      gc.strokeLine(125 + o*200, 75, 125 + i*200, 225)
    }
    for (o <- 0 until outputs) {
      gc.fillOval(100 + o*200, 50, 50, 50)
      gc.strokeOval(100 + o*200, 50, 50, 50)
    }
    for (i <- 0 until inputs) {
      gc.fillOval(100 + i*200, 200, 50, 50)
      gc.strokeOval(100 + i*200, 200, 50, 50)
    }
  }

  def drawData(): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 250, 2000, 1000)
    val scale = 100
    val cx = 300
    val cy = 550
    for (i <- 0 until 100; j <- 0 until 100) {
      val x = (i-50)/20.0
      val y = (j-50)/20.0
      val out = (Array(1.0, x, y), weights(0).w).zipped.map(_ * _).sum
      val c = out max 0.0 min 1.0
      gc.fill = Color(c, 1.0-c, 0.0, 1.0)
      gc.fillRect(cx+x*scale, cy-y*scale, 5, 5)
    }
    for (io <- ios) {
      val c = io.outputs(0) max 0.0 min 1.0
      gc.fill = Color(c, c, c, 1.0)
      gc.fillOval(io.inputs(0)*scale+cx-5, cy-io.inputs(1)*scale-5, 10, 10)
      gc.strokeOval(io.inputs(0)*scale+cx-5, cy-io.inputs(1)*scale-5, 10, 10)
    }

    gc.stroke = Color.Black
    gc.strokeLine(0, cy, 2000, cy)
    gc.strokeLine(cx, cy-2*scale, cx, cy+2*scale)
  }
}