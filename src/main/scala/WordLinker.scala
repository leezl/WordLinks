import java.io.{BufferedReader, FileReader}
import scala.collection.mutable._

object WordLinker {
  var totals = Map[Double, Double]()
  var words = List[(String,Double,Double)]()
  var dictionary = List[String]()

  def main(args: Array[String]) {
    //data in
    //FOR NOW: read from file, (read in whole file)
    totals = readTotals("/home/lieslw/NGramSearch/data/googlebooks-eng-all-totalcounts-20090715.txt")
    words = readWords("/home/lieslw/NGramSearch/data/SpecialSubset")
    //normalize
    words = normalizer(words, totals)
    //find word match, collect matched values into array
    var wordList = findWordList("war", words)
    //smoothers
    var wordList3 = smoother(wordList, 3)
    var wordList5 = smoother(wordList, 5)
    var wordList10 = smoother(wordList,10)
    //find peaks (store ranges)
    var wordPeaks = peakFinder(wordList)
    var wordPeaks3 = peakFinder(wordList3)
    var wordPeaks5 = peakFinder(wordList5)
    var wordPeaks10 = peakFinder(wordList10)
    //check where peaks are with each smoothing:
    //comparePeaks(wordList, wordPeaks, wordList3, wordPeaks3)
    //comparePeaks(wordList, wordPeaks, wordList5, wordPeaks5)
    //comparePeaks(wordList, wordPeaks, wordList10, wordPeaks10)
    //run various DTW (write own or use library?)
    //iterate through 
    var wordListOther = List[(Double, Double)]
    for (i<- 0 until dictionary.length){
      //compare other word to current
      //repeat above...
      var wordListOther = findWordList(dictionary(i), words)
      var wordListOther3 = smoother(wordListOther, 3)
      var wordListOther5 = smoother(wordListOther, 5)
      var wordListOther10 = smoother(wordListOther,10)
      //runDTW inside each peakSet
      for(j<- 0 until wordPeaks.length){

      }
      for(j<- 0 until wordPeaks3.length){

      }
      for(j<- 0 until wordPeaks5.length){

      }
      for(j<- 0 until wordPeaks10.length){

      }
      //check Costs
      //TRY find derivatives and compare those
      //Check Costs
    }
  }

  //Normalizes according to yearly publications (or some other array to divide by)
  def normalizer(data : List[(String, Double,Double)], yearlyGrams : Map[Double,Double]) : List[(String,  Double,Double)] = {
    //make temp array (no overwriting)
    var newData = List[(String,Double,Double)]()
    var tempTuple1 = ""
    var tempTuple2 : Double = 0.0
    var tempTuple3 : Double = 0.0
    //divide every count by the number of words that year (or some other array if this ends up being general)
    for (i<- 0 until data.length){
      tempTuple1 = data(i)._1
      tempTuple2 = data(i)._2
      yearlyGrams.get(data(i)._2) match{
        case Some(x) =>
          tempTuple3= data(i)._3/x
        case None =>
          println("Could not find year in Totals List")
          tempTuple3= 0.0
      }
      newData =  (tempTuple1,tempTuple2,tempTuple3) :: newData
    }
    newData  //return
  }

  //Uses Moving Average Smoothing to smooth data
  def smoother(data : List[(Double,Double)], smoothness : Int = 3) : List[(Double,Double)] = {
    //create temp Array
    var newData = List[(Double,Double)]()
    var tempVal =0.0
    if(smoothness*2+1 < data.length){
      //sliding window for averages : (smoothness size buffer at begin and end)
      for (i<-0 until smoothness){
        newData = (data(i)._1, data(i)._2) :: newData///ends are not smoothed
      }
      for (i<- smoothness until data.length-smoothness){
        //Add values from i-smoothness, until i+smoothness
        for (j<- i-smoothness until i+smoothness){
          tempVal += data(j)._2
        }
        //divide by 2*smoothness+1
        tempVal = tempVal/((smoothness*2)+1)
        newData = (data(i)._1, tempVal) :: newData
      }
      for (i<- data.length-smoothness until data.length){
        newData = (data(i)._1, data(i)._2) :: newData///ends are not smoothed
      }
    } else{
      println("Smoothness value too large")
    }
    newData.sortWith(_._1<_._1)
  }
  
  //Cheap Slope calculation, returns same length array, where final value is same as previous
  // //(bad.If you don't like it send me more data))
  def deriver(data : List[(Double,Double)]) : Array[Double]  = {
    var newData = Array[Double](data.length)
    //find slope between every two points
    for(i<- 0 until data.length-1) {
      //next-current
      newData(i) = data(i+1)._2 - data(i)._2
    }
    newData(newData.length-1) = newData(newData.length-2)
    newData       //return
  }
  
  //Find "Peaks": given array, find Pos + Neg slopes (consistent) return ranges for peaks in array of tuples...return index of maxima? Need ranges
  //  //Use strictness parameter? //findMaxima, with threshold for tolerance yay
  def peakFinder(data : List[(Double,Double)], threshold : Int = 2) : List[(Int, Int, Int)] = {//List = [leftLow, Peak, rightLow]
    //like sliding window, find points that are higher then -threshold- many neighbors
    var peaks = List[(Int,Int,Int)]()
    //loop along data searching for maxima (expand threshold to find width of maxima: call this function in loop with varying threshold?)
    //find largest range beyond threshold?
    //find peaks:
    for (i<-1 until data.length-1){
      //0 at max and mins, local
      if (data(i)._2>data(i-1)._2 && data(i)._2>data(i+1)._2){ //slope at
        //find lowest backwards
        var lowBack = data(i-1)._2
        var j = i-2
        while(j>1 && data(j)._2<lowBack){
          lowBack=data(j)._2
          j=j-1
        }
        //find lowest forwards
        var lowFore = data(i+1)._2
        var k = i+2
        while(k<data.length-1 && data(k)._2<lowFore){
          lowFore=data(k)._2
          k=k+1
        }
        //check min threshold
        if (i-j>threshold && k-i>threshold){
          peaks = (j, i, k) :: peaks
          //println(" with start at " + data(j)._1 + " Peak at " + data(i)._1 + " and end at " + data(k)._1)
          //println("Values were " + data(j)._2 + " " + data(i)._2 + " " + data(k)._1)
        }
      }
    }
    //Add to list
    peaks //return
  } //low, mid, high
  
  def readTotals(filename : String) : Map[Double, Double] = {
    var totals = Map[Double,Double]()
    try{
      val reader = new BufferedReader(new FileReader(filename))
      var line = reader.readLine()
      var text = Array[String]()
      while(line!=null){
        text = line.split("""[\s]+""")
        totals(text(0).toDouble) = text(1).toDouble
        line = reader.readLine()
      }
      reader.close()
    }
    catch{
      case whatever =>
        println("Error: " + whatever)
    }
    totals
  }

  def readWords(filename : String) : List[(String, Double, Double)] = {
    var counts = List[(String,Double,Double)]()
    try{
      val reader = new BufferedReader(new FileReader(filename))
      var line = reader.readLine()
      line = reader.readLine()
      var text = Array[String]()
      while(line!=null){
        text = line.split("""[\,]+""")
        counts = (text(0), text(1).toDouble, text(2).toDouble) :: counts
        //println(text(0) + " " + text(1).toDouble + " " + text(2).toDouble)
        if (!dictionary.contains(text(0))){
          dictionary = text(0) :: dictionary
          //println(text(0))
        }
        line = reader.readLine()
      }
      reader.close()
    }
    catch{
      case whatever => 
        println("Error: " + whatever)
    }
    counts
  }

  def findWordList(word : String, data : List[(String, Double, Double)]) : List[(Double, Double)] = {
    var wordOccur = List[(Double, Double)]()
    //search list for word matches, append to wordOccur
    for (i<- 0 until data.length){
      if (data(i)._1 == word) {
        //println("Year, Count: " + data(i)._2 + " " + data(i)._3)
        wordOccur = (data(i)._2, data(i)._3) :: wordOccur
      }
    }
    wordOccur.sortWith(_._1<_._1) //sort by year
  }

  def comparePeaks(firstWords : List[(Double,Double)], first : List[(Int, Int, Int)], secondWords : List[(Double,Double)], second: List[(Int, Int, Int)]) {
    //compare overlaping ranges for peaks
    var tolerance = 4
    println("Comparing Peaks: ")
    for (i<-0 until first.length){
      for (j<- 0 until second.length){
        if ((firstWords(first(i)._1)._1 < secondWords(second(j)._1)._1+tolerance && firstWords(first(i)._1)._1 > secondWords(second(j)._1)._1-tolerance) || (tolerance+firstWords(first(i)._1)._1 > secondWords(second(j)._1)._1 && firstWords(first(i)._1)._1-tolerance < secondWords(second(j)._1)._1)){
          println("Lows match")
        } else{
          //println("Failed: " + firstWords(first(i)._1)._1 + " " + secondWords(second(j)._1)._1)
        }
        if ((firstWords(first(i)._3)._1 < secondWords(second(j)._3)._1+tolerance && firstWords(first(i)._3)._1 > secondWords(second(j)._3)._1-tolerance) || (tolerance+firstWords(first(i)._3)._1 > secondWords(second(j)._3)._1 && firstWords(first(i)._3)._1-tolerance < secondWords(second(j)._3)._1)){
          println("Highs match")
        }
        else{
          //println("Failed: " + firstWords(first(i)._3)._1 + " " + secondWords(second(j)._3)._1)
        }
        if ((firstWords(first(i)._2)._1 < secondWords(second(j)._2)._1+tolerance && firstWords(first(i)._2)._1 > secondWords(second(j)._2)._1-tolerance) || (tolerance+firstWords(first(i)._2)._1 > secondWords(second(j)._2)._1 && firstWords(first(i)._2)._1-tolerance < secondWords(second(j)._2)._1)){
          println("Peak Matches")
        }
        else{
          //println("Failed: " + firstWords(first(i)._2)._1 + " " + secondWords(second(j)._2)._1)
        }
      }
      println("End peak search: " + i)
    }
  }

}