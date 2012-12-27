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
  val length = featureset.length
  var emp_dist = mutable.HashMap[(Char,String),Int]()
  var f_total = mutable.HashMap[(Char,String),Int]()
  var alambda = Array.fill[Double](length){0.0}
  var labels = clabels
  trainiis()

  def zlamdba(index:Int):Double = {
    // = sigmay(e(sigma(lamdbai*fi)))
    var sum:Double = 0
    for (label <- labels) {
      var sum2:Double = 0
      var i = 0
      while(i < alambda.length) {
        val feature = new Feature(featureset(i))
        if (feature.run(text,index) == 1) {
          sum2 += alambda(i)
        }
        i += 1
      }
      sum += math.exp(sum2)
    }
    sum
  }
  def plamdba(fs: (List[(String, Char, Int)], String)):Double = {
    //plamdba(f) = emp(x)*plambda(x,y)fi(x,y)
    var i = 0
    var sum = 0.0
    while(i < text.length) {
      //x = a[i]
      val feature = new Feature(fs)
      if (feature.run(text,i) == 1) {
        sum += plambda(i,feature.y)
      }
      i += 1
    }
    sum
  }

  def plambda(index:Int,token:String) :Double = {
    //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
    var z = zlamdba(index)
    var result = 1/z
    var sum = 0.0
    for (i <- 0 until featureset.length) {
      if(featureset(i)._2 == token){
        val feature = new Feature(featureset(i))
        if (feature.run(text,index) == 1) {
          sum += alambda(i)
        }
      }
    }
    result *= math.exp(sum)
    result
  }
  def empirical_p(fs: (List[(String, Char, Int)], String)):Double = {
    var i = 0
    var sum = 0
    while(i < text.length) {
      //x = a[i]
        val feature = new Feature(fs)
        if (feature.run(text,i) == 1) {
          sum += 1
        }
      i += 1
    }
    sum
  }

  def calculateDelta(index:Int):Double = {
    //delta = 1/m * log (p(fi)/plamdba(fi))
    var delta:Double = 1.0
    delta /= calM()
    println("calulating delta")
    delta *=math.log(empirical_p(featureset(index))/plamdba(featureset(index)))
    delta
  }
  def calM():Int = {
    //M=sigma(fi(x,y)forallx,y)
    var i = 0
    var sum = 0
    while(i < text.length) {
      //x = a[i]
      for (fs <- featureset) {
        val feature = new Feature(fs)
        if (feature.run(text,i) == 1) {
          sum += 1
        }
      }
      i += 1
    }
    sum
  }
  def trainiis() {
    var stop = true
    println("training iis")
    var i = 0
    do {
      while(i < length) {
        var delta = calculateDelta(i)
        alambda(i) += delta
        if(delta == 0)
          stop = false
        i += 1
        print("calculated lambda  %d".format(i))
      }
      i = 0
      println("-------------")
      alambda.map(x=>println(x))
      println("-------------")
    }while(!stop)

  }
}

