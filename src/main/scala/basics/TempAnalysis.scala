package basics

import swiftvis2.plotting._
import swiftvis2.plotting.renderer.SwingRenderer

case class TempData(day: Int, doy: Int, month: Int, year: Int, precip: Double, 
  tave: Double, tmax: Double, tmin: Double)

object TempAnalysis {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("data/SanAntonioTemps.csv")
    val data = source.getLines.drop(2).map { line =>
      val p = line.split(",")
      TempData(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble, p(6).toDouble, p(7).toDouble, p(8).toDouble)
    }.toArray
    source.close

    val max = data.maxBy(_.tmax)
    println(s"$max")

    val rainyDays = data.foldLeft(0)((count, curData) => if (curData.precip >= 1.0) count+1 else count)
    println(s"The fraction of days with rain was ${rainyDays.toDouble/data.length}")
    
    val groups = data.groupBy(x => x.month)
    val monthTemps = groups.mapValues(temps => temps.foldLeft(0.0)(_+_.tmax)/temps.length)
    monthTemps.toSeq.sortBy(_._1).foreach(println)

    val yearGrad = ColorGradient(1946.0 -> BlueARGB, 2014.0 -> RedARGB)
    val tempGrad = ColorGradient(20.0 -> BlueARGB, 55.0 -> GreenARGB, 90.0 -> RedARGB)
    val tempPlot = Plot.scatterPlot(data.map(_.doy.toDouble), data.map(_.tmax), "Temps", "Day of Year", "Degrees",
      5, tempGrad(data.map(_.tmin)))
    SwingRenderer(tempPlot, 1000, 1000, true)
    
  }
}

// yeet yote yoten yeeten yeetist
// leet lote loten leeten leetist
// neat note noten neaten neatist
// hi hellote helloten helleeten helleetist
// it was all nick and kenny and nick and kenny