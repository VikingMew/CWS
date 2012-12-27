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
  var alambda = Array.fill[Double](length){1.0}
  var labels = clabels
  calfeature()
  var m = calM()
  var empp = new Array[Int](featureset.length)
  calempp()

  trainiis()

  def calempp() {
    var i = 0
    while(i < featureset.length) {
      empp(i) = empirical_p(i)
      print(empp(i))
      i += 1
    }
  }
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
      println("Z:%d,label:%s lf:%f".format(index,label,sum2))
      sum += math.exp(sum2)
    }
    sum
  }
  def plamdba(fsindex:Int):Double = {
    //plamdba(f) = emp(x)*plambda(x,y)fi(x,y)
    var i = 0
    var sum = 0.0

    while(i < text.length) {
      //x = a[i]
      if (feature(fsindex).run(text,i) == 1) {
        sum += plambda(i,feature(fsindex).y)
      }
      i += 1
    }

    println(sum)
    sum
  }

  def plambda(index:Int,token:String) :Double = {
    //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
    var z = zlamdba(index)
    var result:Double = 0.0
    var sum = 0.0
    var i = 0
    var occurs  = 0
    while(i < featureset.length) {

      if(featureset(i)._2 == token){
        if (feature(i).run(text,index) == 1) {
          occurs += 1
          sum += alambda(i)
        }
      }
      i += 1
    }
    result = math.exp(sum) / z
    print("PLSUM(x:%d,y:%s):%f,Z:%f,occurs:%dResult:".format(index,token,sum,z,occurs))
    println(result)
    result
  }
  def empirical_p(fsindex:Int):Int = {
    //=p(fi)=pxpxyfxy
    var i = 0
    var sum = 0
    while(i < text.length) {
      //x = a[i]
        if (feature(fsindex).run(text,i) == 1) {
          sum += 1
        }
      i += 1
    }
    sum
  }

  def calculateDelta(index:Int):Double = {
    //delta = 1/m * log (p(fi)/plamdba(fi))
    var delta:Double = 1.0
    delta /= m
    println("calulating delta")
    var emp = (empp(index).toDouble)
    var pl =  (plamdba(index))
    println("EMP:%f,PL:%f".format(emp,pl))
    delta *=math.log( emp / pl)
    println("calulated delta")
    delta
  }
  def calM():Int = {
    //M=sigma(fi(x,y)forallx,y)
    var i = 0
    var sum = 0
    while(i < text.length) {
      //x = a[i]
      for (featurei <- feature) {
        if (featurei.run(text,i) == 1) {
          sum += 1
        }
      }
      i += 1
    }
    sum
  }
  def trainiis() {
    var stop = true
    var i = 0
    var t = 2
    do {
      i =  0
      t -= 1
      while(i < length) {
        println("calculating lambda  %d".format(i))
        var delta = calculateDelta(i)
        alambda(i) += delta
        if(delta == 0)
          stop = false
        //println("calculated lambda  %d %f".format(i,delta))
        i += 1
      }
      println("-------------")
      i = 0
      while(i < alambda.length) {
        print("%f  ".format(alambda(i)))
        i += 1
      }
      println("\n-------------")
    }while(t > 0)

  }
}

