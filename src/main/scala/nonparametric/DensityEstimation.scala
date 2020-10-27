package nonparametric

import collection.mutable
import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer
import javax.swing._
import java.awt.GridLayout
import java.awt.BorderLayout
import swiftvis2.plotting.styles.ScatterStyle

object DensityEstimation extends App {
  val xs = mutable.Buffer.fill(100)(math.random*100)
  private var h = 5.0
  private var style = 0
  val lineX = (0 until 1000).map(i => i/10.0)

  val updater = SwingRenderer(histogramPlot(), 1200, 1200, true)
  val controlFrame = new JFrame("Control")
  controlFrame.setLayout(new BorderLayout)
  val styles = new JPanel
  styles.setLayout(new GridLayout(1, 5))
  val styleGroup = new ButtonGroup
  val histogramButton = new JRadioButton("Histogram", true)
  histogramButton.addChangeListener(cl => if (histogramButton.isSelected()) { style = 0; replot() })
  styleGroup.add(histogramButton)
  styles.add(histogramButton)
  val naiveButton = new JRadioButton("Naive", false)
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
  controlFrame.add(styles, BorderLayout.NORTH)
  val slider = new JSlider(1, 20, 10)
  slider.addChangeListener(e => { h = slider.getValue(); replot() })
  controlFrame.add(slider, BorderLayout.CENTER)
  controlFrame.setSize(1000, 200)
  controlFrame.setVisible(true)

  def replot(): Unit = {
    if (xs.length > 0) {
      val basePlot = style match {
        case 0 => histogramPlot()
        case 1 => naivePlot()
        case 2 => kernelPlot()
        case 3 => knnPlot()
      }
      updater.update(basePlot.updatedPlotGrid(_.withStyle(ScatterStyle(xs, xs.map(_ => 0.1), colors = RedARGB), "x", "y", stack = 1)))
    }
  }

  def histogramPlot(): Plot = {
    Plot.histogramPlotFromData((0 to (100/h).ceil.toInt).map(i => i * h), xs, BlueARGB)
  }

  def naivePlot(): Plot = {
    val y = lineX.map(lx => xs.count(x => (x-lx).abs < h/2))
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def kernelPlot(): Plot = {
    val y = lineX.map(lx => xs.map(x => math.exp(-(x-lx)*(x-lx)/(2*h))).sum)
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }

  def knnPlot(): Plot = {
    val y = lineX.map { lx => 
      val sorted = xs.map(x => (x-lx).abs).sorted
      val dk = sorted.apply(h.toInt).abs
      h / (2* xs.length * dk)
    }
    Plot.scatterPlotWithLines(lineX, y, symbolSize = 0, lineGrouping = 0)
  }
}