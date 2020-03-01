package wordcount

import java.awt.{Color, GridLayout}

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities

import scala.io.Source


/**
  * @author hendrik
  *         modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 5
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {

    /* bekommt als Parameter einen Dateinamen sowie
      die Anzahl von Wörtern, die innerhalb eines Abschnitts zusammengefasst werden sollen.
      Ergebnis der Funktion ist ein Tupel, bestehend aus der Abschnittsnummern und
      einer Liste von Wörtern, die dort vorkommen. Die Abschnittsnummern sollten bei 1
      anfangen.   */

    Source.fromFile(getClass.getResource("/" + filename).getPath)
      .getLines() //returns an iterator who returns lines (including newline character). a line ends in \n.
      .flatMap(proc.getWords)  // mit getWords aus Processing Wörter holen
      .grouped(wordCount) //siehe alternativ auch sliding(wordCount, wordCount)
      .map(_.toList) //Sequence in List umwandeln
      .zipWithIndex  //mit Indizes versehen
      .map { case (value, index) => (index + 1, value) }  //Reihenfolge vertauschen und Index bei 1 beginnen
      .toList
  }


  def getDocumentSplitByPredicate(filename: String, predicate: String => Boolean): List[(Int, List[String])] = {
    ???

  }


  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {

    /* bekommt eine Liste von Abschnitten (Abschnittnr, Liste von Wörtern) und errechnet den jeweiligen
      Sentimentwert. Der Sentimentwert wird als der Durchschnitt der Sentimentwerte aller
      bekannten Wörtern in dem Abschnitt berechnet. Ergebnis ist ein Tripel, bestehend aus
      der Abschnittsnummer, dem Sentimentwert und der relativen Anzahl von Wörtern, die
      für die Sentimentanalyse verwendet werden konnten. */

    l.map {
      case (paraNr, wList) => {
        val sentW = wList.map(w => if (sentiments.contains(w)) sentiments(w) else 0)  // Sentimentwerte zuweisen, wenn vorhanden
        val usedW = sentW.count(_ != 0).toDouble   // verwendete Wörter als Double
        (paraNr,          // Abschnittsnummer
          sentW.sum / usedW,   // Sentimentwert
          usedW / sentW.size  // relative Anzahl verwendeter Wörter
        )
      }
    }

  }

  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel: String = "Abschnitt", title: String = "Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2, 1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
  }
}
