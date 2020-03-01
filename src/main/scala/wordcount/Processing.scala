package wordcount

class Processing {

  /** ********************************************************************************************
    *
    * Aufgabe 1
    *
    * ********************************************************************************************
    */
  def getWords(line: String): List[String] = line match {
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    case "" => List.empty

    case _ =>
      line
        .toLowerCase
        .replaceAll("[^a-z]", " ")
        .replaceAll("[ ]{2,}", " ")
        .split(" ")
        .toList
  }


  def getAllWords(l: List[(Int, String)]): List[String] =
  /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */

    l.flatMap(x => getWords(x._2))


  def countWords(l: List[String]): List[(String, Int)] = {
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    l
      .map(x => (x, 1))
      .groupBy(_._1)
      .mapValues(l => { l.map(_._2).sum })
      .toList
  }

  /** ********************************************************************************************
    *
    * Aufgabe 2
    *
    * ********************************************************************************************
    */

  def mapReduce[S, B, R](mapFun: (S => B),
                         redFun: (R, B) => R,
                         base: R,
                         l: List[S]): R =

    l.map(mapFun).foldLeft(base)(redFun)


  def countWordsMR(l: List[String]): List[(String, Int)] = {

    /* Implementieren Sie das Wörterzählen auf Basis der vorgegebenen MapReduce-Funktion.
      Definieren Sie dazu eine Map- sowie eine Reduce-Funktion und verwenden Sie diese
      für den Aufruf. Die Funktion countWordsMR soll demnach nur noch aus einem Aufruf
      von MapReduce mit den entsprechenden Typparametern bestehen. */

    //mapReduce[???,???,???](null,null,null,l)

    def insertL(l: List[(String, Int)], el: (String, Int)): List[(String, Int)] = l match {
      case Nil => List(el)
      case x :: xs if (el._1.equals(x._1)) => (el._1, el._2 + x._2) :: xs
      case x :: xs => x :: insertL(xs, el)
    }

    mapReduce[String, (String, Int), List[(String, Int)]]((x => (x, 1)), insertL, List(), l)

  }



  /** ********************************************************************************************
    *
    * Aufgabe 3
    *
    * ********************************************************************************************
    */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    /* bekommt den Text im Format List((Zeilennr, Zeilentext)), extrahiert alle
      Wörter und speichert diese als Tupel in der Form (Zeilennr, Wort). */

    l
      .flatMap(x => getWords(x._2).map(y => (x._1, y)))
  }


  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    /* fasst sämtliche Vorkommen eines Wortes (Zeilennummern)
      innerhalb einer Map zusammen. Ergebnis der Funktion ist somit eine Map,
      die ein Wort auf eine Liste von Zeilennummern (Int) abbildet. */

    l
      .groupBy(_._2)
      .map { case (s, i) => (s, i.map(_._1)) }


  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {

    /* Ziel der Funktionen ist es, für eine Liste von Wörtern die Zeilen herauszusuchen, die diese enthalten.
      Dabei sollen bei orConjunction die Wörter verortet werden und diese Zeilen zurückgegeben werden,
      in denen mindestens eines der Wörter vorkommt */

    words
      .flatMap(x => invInd.getOrElse(x, List.empty))
      .distinct

  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] =

    words
      .map(x => invInd.getOrElse(x, List.empty))
      .reduceLeft((x, y) => x.intersect(y))


}

object Processing{
  
  def getData(filename:String):List[(Int,String)]={
    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}