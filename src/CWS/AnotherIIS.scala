package CWS

import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/26/12
 * Time: 8:32 PM
 */
class AnotherIIS (cfeatureset:List[(List[(String,Char,Int)],String)],ctext:List[Array[T.Point]],clabels:List[String]) {
  val text = ctext
  val featureset = cfeatureset.toArray
  val labels = clabels

  val count:Int = text.length

  //  var windowoffset = (-1,1)
  val length = featureset.length
  var features = new Array[Feature](length)
  var allword = new mutable.HashMap[Int,List[(String,Char,Int)]]()
  var allwordfeature = new mutable.HashMap[Int,Int]()
  var allfeaturefreq = new mutable.HashMap[Int,Int]()
  var allwordfreq = new mutable.HashMap[Int,Int]()
  var emp_dist = mutable.HashMap[Int,Int]()
  var f_total = mutable.HashMap[Int,Int]()
  var alambda = Array.fill[Double](length){0.0}
  alambda(0) = 1
  alambda(2) = 5
  trainiis()

//<<<<<<< HEAD
//=======
  def setfeature() {
    var i = 0
    while(i < length) {
      features(i) = new Feature(featureset(i))
      i += 1
    }
  }
  def getword() {
    for (sentences <- text) {
      var i = 0
      while(i < sentences.length) {
        for (j <- 0 until length) {
          if (features(j).checkx(sentences,i) == 1) {
            if(!(allwordfreq contains j))
              allwordfreq.put(j,1)
            else
              allwordfreq.update(j,allwordfreq.get(j).get + 1)
          }

          if (features(j).run(sentences,i,sentences(i)._2) == 1) {
            if(!(allfeaturefreq contains j))
              allfeaturefreq.put(j,1)
            else
              allfeaturefreq.update(j,allfeaturefreq.get(j).get + 1)
           }
        }
        i += 1
      }
    }
  }


  def calculateDelta(index:Int):Double = {
    //simga(simga(f)plambda(y|x)exp(delta*flambda#)) = ~p(f)
    // ~p(f) = ~p(x)plambda(y|x)*exp(feature#)
    var delta:Double = 0.0
    println("calulating delta")
    var t = 10000
    while(t > 0){
      t -= 1
    }
    println("calulated delta")
    delta
  }

  def trainiis() {
    var stop = true
    var i = 0
    var t = 100000
    setfeature()
    getword()
    (0 until length).foreach(x=>{
      print(featureset(x))
      print(allwordfreq(x))
      print("  ")
      println(allfeaturefreq(x))
    })
//    do {
//      i =  0
//      t -= 1
//      while(i < length) {
//        println("calculating lambda  %d".format(i))
//        var delta = calculateDelta(i)
//        alambda(i) += delta
//        if(delta == 0)
//          stop = false
//        //println("calculated lambda  %d %f".format(i,delta))
//        i += 1
//      }
//      println("-------------")
//      i = 0
//      while(i < alambda.length) {
//        print("%f  ".format(alambda(i)))
//        i += 1
//      }
//      println("\n-------------")
//    }while(t > 0)

  }
}

