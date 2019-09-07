package basics
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer
import org.apache.spark.Success
import spire.std.`package`.map

//MARK: Cases

/*"Country Name","Country Code","Indicator Name","Indicator Code","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","19
83","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015",
"2016","2017"*/
case class countryGDP(countryName: String, countryCode: String, indicatorName: String, indicatorCode: String, years: Array[Option[Double]])

// country acronym, latitude, longitude, name
case class countryLat(countryAcro: String, countryLat: Double, countryLon: Double, countryName: String)

// location_id,location_code,location_name,year,age_group_id,age_group_name,sex_id,sex_name,metric,unit,mean,upper,lower
case class countryEdu(locationId: Int, countryAcro: String, countryName: String, year: String, ageGroupId: Int, 
ageGroupName: String, sexId: Int, sexName: String, metric: String, unit: String, mean: Double, upper: Double, lower: Double)

object Basics {

    def parseGDP(line: String): countryGDP = {
        val p = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1)
        val left = p.take(4)
        val right = p.drop(4)
            countryGDP(
                left(0).toString, //countryName
                left(1).toString, //countryCode
                left(2).toString, //indicatorName
                left(3).toString, //indicatorCode
                right.map(x => if (x.length <= 2) None else Some(x.drop(1).dropRight(1).toDouble)) //years
            )
    }

    def parseLat(line: String): countryLat = {
        val p = line.split("\\s+")
            try countryLat(
                p(0).toString, //countryAcro
                p(1).toDouble, //countryLat
                p(2).toDouble, //countryLon
                p(3).toString()
            ) catch {
                case error: Throwable => countryLat(
                    "Error",
                    0.0,
                    0.0,
                    "Error"
                )
            } 
    }

    def parseEdu(line: String): countryEdu = {
        val p = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1)
            countryEdu(
                p(0).toInt, //locationId
                p(1).toString, //countryAco
                p(2).toString, //countryName
                p(3).toString, //year
                p(4).toInt, //ageGroupId
                p(5).toString, //ageGroupName
                p(6).toInt, //sexId
                p(7).toString, //sexName
                p(8).toString, //metric
                p(9).toString, //unit
                p(10).toDouble, //mean
                p(11).toDouble, //upper
                p(12).toDouble //lower
            )
    }

    def main(args:Array[String]): Unit = {

    val sourceEdu = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/IHME_GLOBAL_EDUCATIONAL_ATTAINMENT_1970_2015_Y2015M04D27.CSV")
    val sourceGDP = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_10081022.csv")
    val sourceLat = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/countries.tsv")

    val linesEdu = sourceEdu.getLines()
    val linesGDP = sourceGDP.getLines()
    val linesLat = sourceLat.getLines()

    val dataEdu = linesEdu.drop(1).map(parseEdu).toArray
    val dataGDP = linesGDP.drop(5).map(parseGDP).toArray
    val dataLat = linesLat.drop(1).map(parseLat).toArray

    dataGDP.take(5).foreach(println)
    dataEdu.take(5).foreach(println)
    dataLat.take(5).foreach(println)

    //#1 The Number of value types reported in the education file in the metric column.
    println("****QUESTION #1****")

    val metricTypes = dataEdu.map(_.metric).distinct
    val metricCount = metricTypes.size
    metricTypes.foreach(x => println(s"Metric Value: $x"))
    println(s"Metric Count: $metricCount")
    
    //#2 The five entries with the highest value for "Education Per Capita Mean". 
    println("****Question #2****")

    val highestEduPerCap = dataEdu.sortWith(_.mean > _.mean).take(5)
    println("Highest Means For Education Per Capita:")
    highestEduPerCap.foreach(x => println(s"Country - ${x.countryName}, Year - ${x.year}, Mean - ${x.mean}"))

    //3. Country with the largest increase in education per capita over the time. The difference? You should potentially have a different country for each age/gender combo.
    println("****Question #3****")

    //Group the data by their country name, ageGroupId, and sexId
    val groupedEduData = dataEdu.filter(x => x.year == "1970" || x.year == "2015").map(x => (x.countryName, x.ageGroupName, x.sexName, x.mean)).groupBy(x => (x._1, x._2, x._3)).mapValues(_ map(_._4))
    val groupedEduDifference = groupedEduData.map(x => (x._1, ((x._2).reverse).reduceLeft(_-_))).toSeq.sortBy(_._2)(Ordering[Double].reverse)

    println(s"Country with the largest increase in education per capita: ${groupedEduDifference.take(1)}")

    //4. Country with the largest GDP per capita in 1970? What was it? Give me the same information for the smallest value.
    println("****Question #4****")

    val largest1970GDP = dataGDP.maxBy(x => x.years(10))
    val smallest1970GDP = dataGDP.filter(x => x.years(10) != None).minBy(x => x.years(10))
    val largest1970 = largest1970GDP.years(10) match {
        case Some(year: Double) => year
        case None => None
    }
    val smallest1970 = smallest1970GDP.years(10) match {
        case Some(year: Double) => year
        case None => None
    }
    println(s"Largest GDP per capita in 1970: Country - ${largest1970GDP.countryName}, GDP - ${largest1970}")
    println(s"Smallest GDP per capita in 1970: Country - ${smallest1970GDP.countryName}, GDP - ${smallest1970}")

    //5. Country with the largest GDP per capita in 2015? What was it? Give me the same information for the smallest value.
    println("****Question #5****")

    val largest2015GDP = dataGDP.maxBy(x => x.years(55))
    val smallest2015GDP = dataGDP.filter(x => x.years(55) != None).minBy(x => x.years(55))
    val largest2015 = largest1970GDP.years(55) match {
        case Some(year: Double) => year
        case None => None
    }
    val smallest2015 = smallest1970GDP.years(55) match {
        case Some(year: Double) => year
        case None => None
    }
    println(s"Largest GDP per capita in 2015: Country - ${largest2015GDP.countryName}, GDP - ${largest2015}")
    println(s"Smallest GDP per capita in 2015: Country - ${smallest2015GDP.countryName}, GDP - ${smallest2015}")

    //6. Country with the largest increase in GDP per capita from 1970 to 2015? What were the starting and ending values? (Note that you can't assume no data means 0. It just means that it wasn't reported.)
    println("****Question #6****")

    val filteredGDPData = (dataGDP.filter(x => x.years(10) != None)).filter(x => x.years(55) != None)
    val mappedFilteredData = filteredGDPData.map(x =>  (x.countryName, 
                                        x.years(10) match {
                                        case Some(year1970:Double ) => year1970
                                        case None => None}, 
                                        x.years(55) match {
                                        case Some(year2015:Double) => year2015
                                        case None => None}))
    val differenceInData = mappedFilteredData.map(x => (x._1, x._3.toString.toDouble - x._2.toString.toDouble)).toSeq.sortBy(_._2)(Ordering[Double].reverse)
    println(s"Country with the largest increase in GDP per capita from 1970 to 2015: ${differenceInData(0)}")


    //7. Pick three countries and make a scatter plot with year on the X-axis and educational attainment of females ages 25-34 on the Y-axis. Your three countries should have good data going back to at least 1970.
    println("****Question #7****")
    //"Kenya","KEN","GDP per capita (constant 2010 US$)","NY.GDP.PCAP.KD"
    //Netherlands","NLD","GDP per capita (constant 2010 US$)","NY.GDP.PCAP.KD"
    //"Trinidad and Tobago","TTO","GDP per capita (constant 2010 US$)","NY.GDP.PCAP.KD"
    
    val filteredCountryEduData = dataEdu.filter(x => (x.countryName == "Kenya" || x.countryName == "Netherlands" || x.countryName == "Trinidad and Tobago") && x.ageGroupName == "25 to 34" && x.sexName == "Females")

    val cg = ColorGradient(1946.0 -> RedARGB, 1975.0 -> BlueARGB, 2014.0 -> GreenARGB)


    //)
    val femaleEduPlot = Plot.simple(
        ScatterStyle(filteredCountryEduData.map(_.year.toDouble), filteredCountryEduData.map(_.mean), symbolWidth = 3, symbolHeight = 3, colors = cg(filteredCountryEduData.map(_.mean))),
        "Educational Attainment of Females Ages 25-34 in Kenya, Netherlands, and Trinidad and Tobago", "Year", "Educational Attainment")
        SwingRenderer(femaleEduPlot,800,800, true)
    

    //8. For those same three countries you picked for #7, make a scatter plot of GDP over time.
    println("****Question #8****")
    val filteredCountryGDPData = dataGDP.filter(x => (x.countryName == "Kenya" || x.countryName == "Netherlands" || x.countryName == "Trinidad and Tobago")).map(x =>  
    x.years.map(x => x match {
    case Some(year:Double ) => year
    case None => None}))


    /*val GDPPlot = Plot.simple(
        ScatterStyle(1970 to 2015, filteredCountryGDPData.map(x => x.toString.toDouble), symbolWidth = 3, symbolHeight = 3, colors = BlackARGB),
        "GDP Over Time of Kenya, Netherlands, and Trinidad and Tobago", "Year", "Educational Attainment")
        SwingRenderer(GDPPlot,800,800, true)
    
        */

    //9. Make a scatter plot with one point per country (for all countries) with GDP on the X-axis and education level of males ages 25-34 on the Y-axis. Make a similar plot for females. Do this for both 1970 and 2015.
    println("****Question #9****")

    //10. Make a scatter plot with longitude and latitude on the X and Y axes. Color the points by educational attainment of females ages 25-34. Have the size of the points indicate the per capita GDP. Do this for both 1970 and 2015.
    println("****Question #10****")


    }

}
