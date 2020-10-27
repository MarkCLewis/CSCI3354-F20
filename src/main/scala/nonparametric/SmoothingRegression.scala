package nonparametric

import collection.mutable
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import javax.swing._
import java.awt.GridLayout
import java.awt.BorderLayout
import swiftvis2.plotting.styles.ScatterStyle

object SmoothingRegression extends App {
  val xs = mutable.Buffer.fill(100)(math.random*100)
  val ys = xs.map(x => math.sin(x / 10) + 0.2 * util.Random.nextGaussian())
  private var h = 5.0
  private var style = 0
  val lineX = (0 until 1000).map(i => i/10.0)

  val updater = SwingRenderer(regressogramPlot(), 1200, 1200, true)
  val controlFrame = new JFrame("Control")
  controlFrame.setLayout(new BorderLayout)
  val styles = new JPanel
  styles.setLayout(new GridLayout(1, 5))
  val styleGroup = new ButtonGroup
  val histogramButton = new JRadioButton("Regressogram", true)
  histogramButton.addChangeListener(cl => if (histogramButton.isSelected()) { style = 0; replot() })
  styleGroup.add(histogramButton)
  styles.add(histogramButton)
  val naiveButton = new JRadioButton("Running Mean", false)
  naiveButton.addChangeListener(cl => if (naiveButton.isSelected()) { style = 1; replot() })
  styleGroup.add(naiveButton)
  styles.add(naiveButton)
  val kernelButton = new JRadioButton("Kernel", false)
  kernelButton.addChangeListener(cl => if (kernelButton.isSelected()) { style = 2; replot() })
  styleGroup.add(kernelButton)
  styles.add(kernelButton)
  val knnButton = new JRadioButton("k-NN", false)
  knnButton.addChangeListener(cl => if (knnButton.isSelected()) { style = 3; replot() })
  styleGroup.add(knnButton)
  styles.add(knnButton)
  val knnkernelButton = new JRadioButton("k-NN Kernel", false)
  knnkernelButton.addChangeListener(cl => if (knnkernelButton.isSelected()) { style = 4; replot() })
  styleGroup.add(knnkernelButton)
  styles.add(knnkernelButton)
  controlFrame.add(styles, BorderLayout.NORTH)
  val slider = new JSlider(1, 20, 5)
  slider.addChangeListener(e => { h = slider.getValue(); replot() })
  controlFrame.add(slider, BorderLayout.CENTER)
  controlFrame.setSize(1000, 200)
  controlFrame.setVisible(true)

  def replot(): Unit = {
    if (xs.length > 0) {
      val basePlot = style match {
        case 0 => regressogramPlot()
        case 1 => runningMeanPlot()
        case 2 => kernelPlot()
        case 3 => knnPlot()
        case 4 => knnkernelPlot()
      }
      updater.update(basePlot.updatedPlotGrid(_.withStyle(ScatterStyle(xs, ys, colors = RedARGB), "x", "y", stack = 1)))
    }
  }

  def regressogramPlot(): Plot = {
    val y = lineX.map { lx => 
      val bin = (lx / h).toInt
      val inBin = (xs, ys).zipped.filter((x, y) => (x/h).toInt == bin)
      inBin._2.sum / inBin._2.length
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def runningMeanPlot(): Plot = {
    val y = lineX.map { lx => 
      val inBin = (xs, ys).zipped.filter((x, y) => (x-lx).abs < h/2)
      inBin._2.sum / inBin._2.length
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def kernelPlot(): Plot = {
    val y = lineX.map { lx => 
      val yWeight = (xs, ys).zipped.map((x, y) => { val w = math.exp(-(x-lx)*(x-lx)/(2*h)); w*y -> w })
      yWeight.map(_._1).sum / yWeight.map(_._2).sum
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def knnPlot(): Plot = {
    val y = lineX.map { lx =>
      val xy = xs.zip(ys).sortBy(t => (t._1 - lx).abs).take(h.toInt)
      xy.map(_._2).sum / h
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def knnkernelPlot(): Plot = {
    val y = lineX.map { lx => 
      val sorted = xs.map(x => (x-lx).abs).sorted
      val dk = sorted.apply(h.toInt).abs
      val yWeight = (xs, ys).zipped.map((x, y) => { val w = math.exp(-(x-lx)*(x-lx)/(2*dk)); w*y -> w })
      yWeight.map(_._1).sum / yWeight.map(_._2).sum
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }
}