package basics

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
    println(data.last)
  }
}