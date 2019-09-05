package basics
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer

//MARK: Cases

/*"Country Name","Country Code","Indicator Name","Indicator Code","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","19
83","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015",
"2016","2017"*/
case class countryEdu(countryName: String, countryCode: String, indicatorName: String, indicatorCode: String, years: Array[Double])

// country acronym, latitude, longitude, name
case class countryGDP(countryAcro: String, countryLat: Double, countryLon: Double, countryName: String)

// location_id,location_code,location_name,year,age_group_id,age_group_name,sex_id,sex_name,metric,unit,mean,upper,lower
case class countryLat(locationId: Int, countryAcro: String, countryName: String, year: String, ageGroupId: Int, ageGroupName: String, sexId: Int, sexName: String, metric: String, unit: String, mean: Double, upper: Double, lower: Double)


object Basics {

    def parseEdu(line: String): countryEdu = {
        val (left,right) = line.splitAt(3)
            countryEdu(
                left(0).toString, //countryName
                left(1).toString, //countryCode
                left(2).toString, //indicatorName
                left(3).toString, //indicatorCode
                right.split(",").map(_.toDouble) //years
            )
    }

    def parseGDP(line: String): countryGDP = {
        val p = line.split(",")
            countryGDP(
                p(0).toString, //countryAcro
                p(1).toDouble, //countryLat
                p(2).toDouble, //countryLon
                p(3).toString, //countryName
            )
    }

    def parseLat(line: String): countryLat = {
        val p = line.split(",")
            countryLat(
                p(0).toInt,
                p(1).toString,
                p(2).toString,
                p(3).toString,
                p(4).toInt,
                p(5).toString,
                p(6).toInt,
                p(7).toString,
                p(8).toString,
            )
    }

    def main(args:Array[String]): Unit = {

    val sourceEdu = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_10081022.csv")
    val sourceGDP = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/countries.tsv")
    val sourceLat = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/IHME_GLOBAL_EDUCATIONAL_ATTAINMENT_1970_2015_Y2015M04D27.CSV")

    //#1 The Number of value types reported in the education file in the metric column.
    
    }

}
