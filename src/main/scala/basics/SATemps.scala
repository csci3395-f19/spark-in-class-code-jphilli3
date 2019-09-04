package basics
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double, tave: Double, tmax: Double, tmin: Double)

object SATemps {
    def parseLine(line: String): TempRow = {
        val p = line.split(",")
        TempRow(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble, p(6).toDouble, p(7).toDouble, p(8).toDouble)

    }

    def main(args:Array[String]): Unit = {

        val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv")
        val lines = source.getLines()
        val data = lines.drop(2).map(parseLine).toArray
        data.take(5).foreach(println)

        //**Hottest Days**
        val hotDay1 = data.maxBy(_.tmax) //Hottest day method 1
        val hotDay2 = data.reduce((d1,d2) => if(d1.tmax > d2.tmax) d1 else d2)//Hottest day method 2
        
        println(s"Hottest Day: $hotDay1")
        

        //**Day of Most Precipitation**
        val rainyDay1 = data.maxBy(_.precip) //Rainiest day method 1
        val rainyDay2 = data.reduce((d1,d2) => if (d1.precip > d2.precip) d1 else d2) //Rainiest day method 2

        println(s"Rainiest Day: $rainyDay1")

        //**Fraction of Days With More Than 1 Inch of Precipitation**
        val num = data.filter(_.precip > 1).length
        val den = data.length.toDouble
        val frac = num/den

        println(s"Fraction of Days With More Than 1 Inch of Precipitation: $frac")
        
        //**Average High Temperature For The Rainy Days**
        val rDays = data.filter(_.precip >= 1.0)
        val avHighRDay = rDays.foldLeft(0.0)(_ + _.tmax)/rDays.length.toDouble

        val (rainySum, rainyCount) = data.foldLeft((0.0,0)) { case ((sum, cnt), day) =>
            if (day.precip >= 1.0) (sum + day.precip, cnt + 1) else (sum,cnt)
        }

        println(s"Average High Temperature For The Rainy Days: $avHighRDay")

        //**Average High Temperature By Month**
        val months = data.groupBy(_.month)
        val avHighTMon = months.mapValues((rows) => rows.map(_.tmax).sum / rows.length)

        println(s"Average High Temperature By Month: $avHighTMon")

        //**Average Amount of Precipitation By Month**
        val avPreciMon = months.mapValues((rows) => rows.map(_.precip).sum / rows.length)

        println(s"Average Amount of Precipitation By Month: $avPreciMon")

        //**Median Amount of Precipitation By Month**
        val mdPreciMon = months.mapValues(m => m.sortBy(_.precip).apply(m.length/2))

        println(s"Median Amount of Precipitation By Month: $mdPreciMon")

        //Plots
        val cg = ColorGradient(1946.0 -> RedARGB, 1975.0 -> BlueARGB, 2014.0 -> GreenARGB)
        val tempByDayPlot = Plot.simple(
            ScatterStyle(data.map(_.doy), data.map(_.tave), symbolWidth = 3, symbolHeight = 3, colors = cg(data.map(_.year))),
            "Day of Year", "Temp", "SA Temps")
            SwingRenderer(tempByDayPlot,800,800, true)
    }
}