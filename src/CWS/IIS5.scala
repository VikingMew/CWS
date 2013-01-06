package CWS

import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/26/12
 * Time: 8:32 PM
 */
class IIS5 (cfeatureset:List[(List[(String,Char,Int)],String)],ctext:List[Array[T.Point]],clabels:List[String]) {
  val text = ctext
  val featureset = cfeatureset.toArray
  val labels = clabels

  val count:Int = text.length

  //  var windowoffset = (-1,1)
  val length = featureset.length
  var features = new Array[Feature](length)
//  var allword = new mutable.HashMap[Int,List[(String,Char,Int)]]()
//  var allwordfeature = new mutable.HashMap[Int,Int]()
  var allfeaturefreq = new mutable.HashMap[Int,Int]()
  var allwordfreq = new mutable.HashMap[Int,Int]()
  var emp_dist = mutable.HashMap[Int,Int]()
//  var f_total = mutable.HashMap[Int,Int]()
  var fsharp = Array.fill[Int](length){0}
  var alambda = Array.fill[Double](length){0.0}
  trainiis()
  var ll = loglihood()
  def loglihood() :Double ={
    var ll:Double = 0.0
    (0 until length).foreach(i =>{
      ll += alambda(i)*allfeaturefreq(i)
    })
    var i = 0
    var set = mutable.HashSet[List[(String,Char,Int)]]()
    while(i < length) {
      if (!(set contains featureset(i)._1)) {
        set.add(featureset(i)._1)
        println("!!!")
        ll -= math.log(zlamdba(i)) * allwordfreq(i)
      }
      i += 1
    }
    ll
  }
//<<<<<<< HEAD
//=======
  def setfeature() {
    var i = 0
    while(i < length) {
      features(i) = new Feature(featureset(i))
      i += 1
    }
  }
//  def getallword() {
//
//  }
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
def getallFSharp() = {
  (0 until length).foreach(i=>{
    fsharp(i) = calFSharp(i,featureset(i)._2)
  })
}
def calFSharp(index:Int,y:String):Int = {
 var sum = 0
 for(i <- 0 until length) {
   if(features(i).run(featureset(index)._1,y) == 1)
     sum += 1
 }
  sum
}
def calpyx(index:Int,y:String): Double = {
  //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
  var result:Double = 1
  var x = featureset(index)._1
//  var y = featureset(index)._2
  var sum = 0.0
  var i = 0
  while(i < length) {
    if (features(i).run(x,y) == 1) {
      sum += alambda(i)
    }
    i += 1
  }
  result *= math.exp(sum)
  result /= zlamdba(index)
  result
}
def zlamdba(index:Int):Double = {
  // = sigmay(e(sigma(lamdbai*fi)))
  var sum:Double = 0.0
  for (label <- labels) {
    var sum2:Double = 0
    var i = 0
    while(i < length) {
      if (features(i).run(featureset(index)._1,label) == 1) {
        sum2 += alambda(i)
      }
      i += 1
    }
//    println("Z:%d,label:%s lf:%f".format(index,label,sum2))
    sum += math.exp(sum2)
  }
  sum
}
def getFormulaPara(index:Int) = {
  var avail = Array.fill[Int](length){0}
  var para = Array.fill[(Int,Int,Double,Int)](length){(-1,0,0,0)}  // x,xfreq,eyx,fsharp
  for(i <- 0 until length) {
    if(features(index).run(featureset(i)._1,featureset(i)._2) == 1) {
      avail(i) = 1
      val emp = allwordfreq(i)
      val fs = fsharp(i)//calFSharp(i,featureset(i)._2)
      var pyx = calpyx(i,featureset(i)._2)
//      println("P y x %f".format(pyx))
      para(i) = (i,emp,pyx,fs)
    }
  }
  (avail,para)
}
  def calculateDelta(index:Int):Double = {
    //simga(simga(f)plambda(y|x)exp(delta*flambda#)) = ~p(f)
    // ~p(f) = ~p(x)plambda(y|x)*exp(feature#)
    var delta:Double = 1
//    println("calulating delta")
    val (avail,para) = getFormulaPara(index)
    var t = 300
    var ffreq = allfeaturefreq(index)
    while(t > 0){
      //delta = delta - g(delta)/g'(delta)
//      | delta[i] -= (ffreq_empirical[i] - sum1[i])/(-sum2[i])
//
//      until convergence, where *sum1* and *sum2* are defined as:
//
//        |    sum1[i](delta) = SUM[fs,l] f[i](fs,l,delta)
//      |    sum2[i](delta) = SUM[fs,l] (f[i](fs,l,delta).nf(feature_vector(fs,l)))
//      |    f[i](fs,l,delta) = (classifier.prob_classify(fs).prob(l) .
//        |                        feature_vector(fs,l)[i] .
//        |                        exp(delta[i] . nf(feature_vector(fs,l))))
      var sum1 = 0.0
      var sum2 = 0.0
      //sum1 = aebx+cedx
      //sum2 = abebx+cdedx
      for(i <- 0 until length) {
        if (avail(i) == 1) {
          //sum1 += aebx
          sum1 += para(i)._2 * para(i)._3 * math.exp(delta * para(i)._4)
          //sum2 += abebx
          sum2 += para(i)._2 * para(i)._3  * para(i)._4 * math.exp(delta * para(i)._4)
        }

      }
      delta -= ((ffreq.toDouble - sum1))/(-sum2)
      if (math.abs((ffreq - sum1)/(-sum2)) < 1e-12)
        t = 0
      t -= 1
    }
//    println("calulated delta %f".format(delta))
    delta
  }

  def trainiis() {
    var i = 0
    setfeature()
    getword()
    (0 until length).foreach(x=>{
      print(featureset(x))
      print(allwordfreq(x))
      print("  ")
      println(allfeaturefreq(x))
    })
    getallFSharp()


    (0 until 100).foreach(y=>{
      (0 until length).foreach(i=>{
        var delta = calculateDelta(i)
        alambda(i) += delta
//        println("calculated lambda  %d %f".format(i,alambda(i)))
      })
            println("-iter %d------------".format(y))
            i = 0
            while(i < alambda.length) {
              println("%f  %f %d".format(alambda(i),allwordfreq(i)*calpyx(i,featureset(i)._2),allfeaturefreq(i)))
              i += 1
            }
            println("\n-------------")
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

