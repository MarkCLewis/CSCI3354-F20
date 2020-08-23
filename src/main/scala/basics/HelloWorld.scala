package basics

import swiftvis2.plotting.Plot
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

/**
 * This is here to remind you how to write Scala and to make it so that
 * the directories for src actually go into the git repository.
 */
object HelloWorld {
	def main(args: Array[String]): Unit = {
		val conf = new SparkConf().setAppName("Temp Data").setMaster("local[*]")
  	val sc = new SparkContext(conf)
  
		sc.setLogLevel("WARN")

		val rdd = sc.parallelize((1 to 10).map(i => i -> i*i))
	
		val data = rdd.collect()
		val plot = Plot.simple(ScatterStyle(data.map(_._1), data.map(_._2)), "Grade vs. Effort", "Effort Required", "Grade")
		SwingRenderer(plot, 1000, 1000, true)

		sc.stop()
	}
}
