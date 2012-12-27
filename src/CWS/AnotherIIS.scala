package CWS

import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/26/12
 * Time: 8:32 PM
 */
class AnotherIIS (cfeatureset:List[(List[(String,Char,Int)],String)],ctext:Array[Point],clabels:List[String]) {
  var text = ctext
  val featureset = cfeatureset
  var feature = new Array[Feature](featureset.length)
  val length = featureset.length
  var emp_dist = mutable.HashMap[(Char,String),Int]()
  var f_total = mutable.HashMap[(Char,String),Int]()
  var alambda = Array.fill[Double](length){0.0}
  var labels = clabels
  calfeature()
  var m = calM()
  trainiis()

  def calfeature() {
    var i = 0
    while(i < featureset.length) {
      feature(i) = new Feature(featureset(i))
      i += 1
    }
  }
  def zlamdba(index:Int):Double = {
    // = sigmay(e(sigma(lamdbai*fi)))
    var sum:Double = 0
    for (label <- labels) {
      var sum2:Double = 0
      var i = 0
      while(i < alambda.length) {
        if (feature(i).run(text,index) == 1) {
          sum2 += alambda(i)
        }
        i += 1
      }
      sum += math.exp(sum2)
    }
    sum
  }
  def plamdba(fsindex:Int):Double = {
    //plamdba(f) = emp(x)*plambda(x,y)fi(x,y)
    var i = 0
    var sum = 0.0
    println("calulating pl")

    while(i < text.length) {
      //x = a[i]
      if (feature(fsindex).run(text,i) == 1) {
        sum += plambda(i,feature(fsindex).y)
      }
      i += 1
    }
    println("calulated pl")

    sum
  }

  def plambda(index:Int,token:String) :Double = {
    //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
    var z = zlamdba(index)
    var result = 1/z
    var sum = 0.0
    for (i <- 0 until featureset.length) {
      if(featureset(i)._2 == token){
        if (feature(i).run(text,index) == 1) {
          sum += alambda(i)
        }
      }
    }
    result *= math.exp(sum)
    result
  }
  def empirical_p(fsindex:Int):Double = {
    var i = 0
    var sum = 0
    println("calulating emp")
    while(i < text.length) {
      //x = a[i]
        if (feature(fsindex).run(text,i) == 1) {
          sum += 1
        }
      i += 1
    }
    println("calulated emp")
    sum
  }

  def calculateDelta(index:Int):Double = {
    //delta = 1/m * log (p(fi)/plamdba(fi))
    var delta:Double = 1.0
    delta /= m
    println("calulating delta")
    delta *=math.log(empirical_p(index)/plamdba(index))
    println("calulated delta")
    delta
  }
  def calM():Int = {
    //M=sigma(fi(x,y)forallx,y)
    var i = 0
    var sum = 0
    println("calulating calM")
    while(i < text.length) {
      //x = a[i]
      for (featurei <- feature) {
        if (featurei.run(text,i) == 1) {
          sum += 1
        }
      }
      i += 1
    }
    println("calulated calM")
    sum
  }
  def trainiis() {
    var stop = true
    println("training iis")
    var i = 0
    do {
      i = 0
      while(i < length) {
        print("calculating lambda  %d".format(i))
        var delta = calculateDelta(i)
        alambda(i) += delta
        if(delta == 0)
          stop = false
        i += 1
        print("calculated lambda  %d".format(i))
      }
      println("-------------")
      alambda.map(x=>println(x))
      println("-------------")
    }while(!stop)

  }
}

