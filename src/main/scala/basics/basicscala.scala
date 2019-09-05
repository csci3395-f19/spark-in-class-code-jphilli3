package basics
import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer

//Cases
case class countryEdu()
case class countryGDP()
case class countryLat()


object Basics {

    def parseLine(line: String): countryEdu = {
        countryEdu()
    }

    def main(args:Array[String]): Unit = {

    val sourceEdu = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_10081022.csv")
    val sourceGDP = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/countries.tsv")
    val sourceLat = scala.io.Source.fromFile("/users/mlewis/workspaceF18/CSCI3395-F18/data/BasicScala/IHME_GLOBAL_EDUCATIONAL_ATTAINMENT_1970_2015_Y2015M04D27.CSV")

    //#1 The Number of value types reported in the education file in the metric column.
    
    }

}
