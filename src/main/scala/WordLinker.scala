package main.scala

import java.io.{BufferedReader, FileReader}
import scala.collection.mutable._
import net.sf.javaml.distance.fastdtw._
import net.sf.javaml.distance.fastdtw.timeseries.TimeSeries
import net.sf.javaml.distance.fastdtw.timeseries.TimeSeriesPoint
import scala.util.Random

object WordLinker {
  var words = List[(String,Double,Double)]()
  var startWord = "war"
  var radius = 5
  var waitCount = 0
  var sizeLimit = 200
  var indices = List[Int]()
  //Initialize Query Client
  val quester = ClientQuery
  //initialize Normalization Counts
  var totals : Map[Double, Double] = readTotals("/home/lieslw/WordLinks/data/googlebooks-eng-all-totalcounts-20090715.txt")
  //Load Mini-Dictionary from file:
  var dictionary : List[String] = readWords("/home/lieslw/WordLinks/data/RandomList.txt")

  def main(args: Array[String]) {
    //Query Initial Word
    var currentWordList = findWord(startWord)
    //currentWordList.foreach(item => println(item))   //DEBUG
    //find derivatives
    var currentWordSlopes = deriver(currentWordList)
    //find peaks
    var currentWordPeaks = peakFinder(currentWordSlopes)
    //Pick random subset
    //println("Num Words: " + dictionary.length) //~860
    var randgen = new Random
    for (i<- 0 until 20){
      indices = i :: indices //ensure first 20 are present
    }
    while(indices.length < sizeLimit){
      var newInd = randgen.nextInt(860)
      if (!indices.contains(newInd)){
        indices = newInd :: indices
      }
    }
    println("Created index list")

    //Collect stats
    var lowCostPeak = MutableList[Double]()
    var highCostPeak = MutableList[Double]()
    var farthestWordPeak = MutableList[String]()
    var closestWordPeak = MutableList[String]()
    for (i<- 0 until currentWordPeaks.length){
      lowCostPeak += 999.0
      highCostPeak += -1.0
      farthestWordPeak += ""
      closestWordPeak += ""
    }
    //loop over other words (read one in, gather stats, delete what you don't need)
    for (i<- 0 until indices.length){  //dictionary
      //grab word
      var otherWordList = findWord(dictionary(indices(i)))
      //find slopes
      var otherWordSlopes = deriver(otherWordList)
      //run DTW on peaks
      for (j <- 0 until currentWordPeaks.length){  //peaks
        if (otherWordList(0)._1!= startWord){
          var timeSeries = new TimeSeries(1)//size TimeSeries
          var timeSeriesOther = new TimeSeries(1)//size TimeSeries
          for (k<- currentWordPeaks(j)._1 to currentWordPeaks(j)._3){
            timeSeriesOther.addLast(otherWordSlopes(k)._1, new TimeSeriesPoint(Array(otherWordSlopes(k)._2))) //add count
            timeSeries.addLast(currentWordSlopes(k)._1, new TimeSeriesPoint(Array(currentWordSlopes(k)._2))) //add count
          }
          //run DTW
          var info = dtw.FastDTW.getWarpInfoBetween(timeSeries, timeSeriesOther, radius)
          if (info.getDistance < lowCostPeak(i)){
            lowCostPeak(i) = info.getDistance
            closestWordPeak(i) = otherWordList(0)._1
          } else if (info.getDistance > highCostPeak(i)){
            highCostPeak(i) = info.getDistance
            farthestWordPeak(i) = otherWordList(0)._1
          }
        }
      }

    }


    /*
    //smoothers
    //var wordList3 = smoother(wordList, 3)
    //run on PEAKS W/ SLOPES
    for (i<- 0 until wordPeaks.length){
      for(k<- 0 until  dictionary.length){
        var wordListOther = findWordList(dictionary(k), words)
        var derivListOther = deriver(wordListOther)
        if (dictionary(k)!= startWord){
          var timeSeries = new TimeSeries(1)//size TimeSeries
          var timeSeriesOther = new TimeSeries(1)//size TimeSeries
          for (j<- wordPeaks(i)._1 to wordPeaks(i)._3){
            timeSeriesOther.addLast(derivListOther(j)._1, new TimeSeriesPoint(Array(derivListOther(j)._2))) //add count
            timeSeries.addLast(derivList(j)._1, new TimeSeriesPoint(Array(derivList(j)._2))) //add count
          }
          //run DTW
          var info = dtw.FastDTW.getWarpInfoBetween(timeSeries, timeSeriesOther, radius)
          if (info.getDistance < lowCostSlopeP(i)){
            lowCostSlopeP(i) = info.getDistance
            closestWordSlopeP(i) = dictionary(k)
          } else if (info.getDistance > highCostSlopeP(i)){
            highCostSlopeP(i) = info.getDistance
            farthestWordSlopeP(i) = dictionary(k)
          }
        }
      }
    }
    */
    //CHECK OUTPUT
    for (i<- 0 until currentWordPeaks.length){
      println("Closest Word by Slope at " + currentWordList(wordPeaks(i)._2)._1 + ": " + closestWordPeak(i) + " Distance: " + lowCostPeak(i))
      println("Farthest Word by Slope at " + currentWordList(wordPeaks(i)._2)._1 + ": " + farthestWordPeak(i) + " Distance: " + highCostPeak(i))
      println("======")
    }
    println("======================================")
  }
  /*Handles fidnign word, waiting for reutrn, and printing progress*/
  def findWord(word : String) : List[(String, Double, Double)] = {
    var resultWord = ""
    quester.requestWord(word)
    //Wait for Result...this is buggy
    while(quester.done != true){
      if(waitCount%100000000 == 0){
        println("..." + waitCount)//println(quester.done )
      }
      waitCount += 1
    }
    waitCount = 0
    if (quester.done==true && quester.noFinish == false){
      resultWord = quester.result
      //println(resultWord)
    } else{
      println("Failed to find result: Server may not be up")
      sys.exit
    }
    normalizer(convertQueryToList(resultWord)).sortWith(_._2<_._2)//CHECK THIS
  }

  //Normalizes according to yearly publications (or some other array to divide by)
  /* NEEDS: DATA: Word,Year,Count
  *         yearlyGRams: Year,TotalCount
  *  RETURNS:newData: Word,Year,CountNew 
  * */
  def normalizer(data : List[(String, Double, Double)], yearlyGrams : Map[Double,Double] = totals) : List[(String,  Double,Double)] = {
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
  /* NEEDS: data: Year,Count
 *         smoothness:
 *  RETURNS:newData: Year,CountNew 
 * */
  def smoother(data : List[(String, Double, Double)], smoothness : Int = 3) : List[(String, Double, Double)] = {
    //create temp Array
    var newData = List[(String, Double, Double)]()
    var tempVal =0.0
    if(smoothness*2+1 < data.length){
      //sliding window for averages : (smoothness size buffer at begin and end)
      for (i<-0 until smoothness){
        newData = (data(i)._1, data(i)._2, data(i)._3) :: newData///ends are not smoothed
      }
      for (i<- smoothness until data.length-smoothness){
        //Add values from i-smoothness, until i+smoothness
        for (j<- i-smoothness until i+smoothness){
          tempVal += data(j)._3
        }
        //divide by 2*smoothness+1
        tempVal = tempVal/((smoothness*2)+1)
        newData = (data(i)._1, data(i)._2, tempVal) :: newData
      }
      for (i<- data.length-smoothness until data.length){
        newData = (data(i)._1, data(i)._2, data(i)._3) :: newData///ends are not smoothed
      }
    } else{
      println("Smoothness value too large")
    }
    newData.sortWith(_._2<_._2)
  }
  
  //Cheap Slope calculation, returns same length array, where final value is same as previous
  // //(bad.If you don't like it send me more data))
  /* NEEDS: data: Year,Count
 *  RETURNS:newData: year,slopes
 * */
  def deriver(data : List[(String, Double, Double)]) : List[(String, Double, Double)]  = {
    var newData = List[(String, Double, Double)]()
    //find slope between every two points
    newData = (data(0)._1, data(0)._2, 0.0) :: newData//slope at begin and end ==0.0 default
    newData = (data(data.length-1)._1, data(data.length-1)._2, 0.0) ::newData
    for(i<- 1 until data.length-1) {
      //next-current
      newData = (data(i)._1, data(i)._2, ((data(i+1)._3 - data(i)._3) + (data(i)._3 - data(i-1)._3))/2.0) :: newData //be sure data is sorted
    }
    newData.sortWith(_._2<_._2)       //return
  }
  
  //Find "Peaks": given array, find Pos + Neg slopes (consistent) return ranges for peaks in array of tuples...return index of maxima? Need ranges
  //  //Use strictness parameter? //findMaxima, with threshold for tolerance yay
  /* NEEDS: data: Year,Count
 *         threshold:
 *  RETURNS:newData: begin, peak, end 
 * */
  def peakFinder(data : List[(String, Double, Double)], threshold : Int = 2) : List[(Int, Int, Int)] = {//List = [leftLow, Peak, rightLow]
    //like sliding window, find points that are higher then -threshold- many neighbors
    var peaks = List[(Int,Int,Int)]()
    //loop along data searching for maxima (expand threshold to find width of maxima: call this function in loop with varying threshold?)
    //find largest range beyond threshold?
    //find peaks:
    for (i<-1 until data.length-1){
      //0 at max and mins, local
      if (data(i)._3>data(i-1)._3 && data(i)._3>data(i+1)._3){ //slope at
        //find lowest backwards
        var lowBack = data(i-1)._3
        var j = i-2
        while(j>1 && data(j)._3<lowBack){
          lowBack=data(j)._3
          j=j-1
        }
        //find lowest forwards
        var lowFore = data(i+1)._3
        var k = i+2
        while(k<data.length-1 && data(k)._3<lowFore){
          lowFore=data(k)._3
          k=k+1
        }
        //check min threshold
        if (i-j>threshold && k-i>threshold){
          peaks = (j, i, k) :: peaks
          //println(" with start at " + data(j)._2 + " Peak at " + data(i)._2 + " and end at " + data(k)._2)
          //println("Values were " + data(j)._3 + " " + data(i)._3 + " " + data(k)._2)
        }
      }
    }
    //Add to list
    peaks //return
  } //low, mid, high

  /* NEEDS: filename: string
 *  RETURNS:totals: Year, totalCounts 
 * */
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

  /* NEEDS: filename: string
 *  RETURNS:newData: List[string]
 * */
  def readWords(filename : String) : List[String] = {
    var counts = List[String]()
    try{
      val reader = new BufferedReader(new FileReader(filename))
      var line = reader.readLine()
      line = reader.readLine()
      var text = Array[String]()
      while(line!=null){
        text = line.split("[ \t\n]+")
        if (!counts.contains(text(0))){
          counts = text(0) :: counts
        }
        line = reader.readLine()
      }
      reader.close()
    }
    catch{
      case whatever => 
        println("Error: " + whatever)
    }
    counts.sortWith(_<_)
  }

  /*This takes the String containing all word year count values and splits it*/
  def convertQueryToList(result : String) : List[(String, Double, Double)] = {
    //create list
    var tempList = List[(String, Double, Double)]()
    //split string
    var line = result.split("[\n]+")  //split lines
    //println("Line: " + line(0))
    //loop thru string, and parse
    for(i<- 0 until line.length) {
      //println("Length: " + line(i).length + " " + line(i).getClass)
      var words = line(i).split("[ \t]+") //split on spaces and tabs
      if (words.length>3) {
        tempList = (words(0), words(1).toDouble, words(2).toDouble) :: tempList
      } else{
        println("TOO SHORT: " + words)
      }
    }
    tempList.sortWith(_._2<_._2) //(reverse/ sort by year)
  }

  /*def comparePeaks(firstWords : List[(String, Double,Double)], first : List[(Int, Int, Int)], secondWords : List[(String, Double, Double)], second: List[(Int, Int, Int)]) {
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
  }*/

}