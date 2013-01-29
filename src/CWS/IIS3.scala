package CWS

import collection.mutable
import scala.collection.mutable.{ HashMap, MultiMap, Set }
import org.scalacheck.Prop.True

/**
* Created with IntelliJ IDEA.
* User: vikingmew
* Date: 12/26/12
* Time: 8:32 PM
*/
class IIS3 (cfeatureset:List[(List[(String,Char,Int)],String)],ctext:List[Array[util.Point]],clabels:List[String]) {
  val text = ctext
  val featureset = cfeatureset.toArray
  val length = featureset.length
  val xlist = getx()    //xindex ->x
  val xlength = xlist.length
  def getx() = {
    var l = List[util.Window]()
    for (sentences <- text) {
      var i = 0
      while(i < sentences.length) {
        val t = util.slice(sentences,i)
        if(!(l contains t)) {
          l = t :: l
        }
        i += 1
      }
    }
    l.toArray
  }
  println(xlist.length)


  val ylist = clabels.toArray                                       //yindex -> y
  val ylength = ylist.length
  var xyfreq = Array.fill[Double](xlength,ylength){0}                       //xyindex -> xyfreq
  getxyfreq()
  def getxyfreq() {
    var sum = 0.0
    for (sentences <- text) {
      for (xindex <- 0 until xlength) {
        for (yindex <- 0 until ylength) {
          var i = 0
          while(i < sentences.length) {
            val t = util.slice(sentences,i)
            if (util.comparex(t,xlist(xindex)) && sentences(i)._2 == ylist(yindex))
            {
              xyfreq(xindex)(yindex) += 1
              sum += 1
            }
            i += 1
          }
        }
      }
    }

    for (xindex <- 0 until xlength) {
      for (yindex <- 0 until ylength) {
            xyfreq(xindex)(yindex) /= sum
      }
    }
  }
  var xfreq = Array.fill[Double](xlength){0}                    //xindex ->xfreq
  getxfreq()
  def getxfreq() {
      for (xindex <- 0 until xlength) {
        xfreq(xindex) = xyfreq(xindex).sum
      }
  }
  val xyfeaturemap = new HashMap[Int, Set[(Int,Int)]] with MultiMap[Int, (Int,Int)]
//  var xyfeaturemap = new mutable.HashMap[Int,List((Int,Int))]()
  getxyfeature()
  def getxyfeature() {
    val features = new Array[Feature](length)
    var i = 0
    while(i < length) {
      features(i) = new Feature(featureset(i))
      i += 1
    }
        (0 until length).foreach(findex =>{
          (0 until xlength).foreach(xindex =>{
            (0 until ylength).foreach(yindex =>{
              if (features(findex).run(xlist(xindex)._1.toArray,xlist(xindex)._2,ylist(yindex)) == 1) {
                xyfeaturemap.addBinding(findex,(xindex,yindex))
              }
            })
          })
        })

  }
//  var xyfeaturemap = Array.fill[Int](xlength,ylength,length){0}
//  getxyfeature()
//  def getxyfeature() {
//    val features = new Array[Feature](length)
//    setfeature()
//    def setfeature() {
//      var i = 0
//      while(i < length) {
//        features(i) = new Feature(featureset(i))
//        i += 1
//      }
//    }
//    (0 until length).foreach(findex =>{
//      (0 until xlength).foreach(xindex =>{
//        (0 until ylength).foreach(yindex =>{
//          if (features(findex).run(xlist(xindex)._1.toArray,xlist(xindex)._2,ylist(yindex)) == 1) {
//            xyfeaturemap(xindex)(yindex)(findex) = 1
//          }
//        })
//      })
//    })
//  }

  var featurefreq = Array.fill[Double](length){0}                  //fsindex->fsfreq
  getfeaturefreq()
  def getfeaturefreq() {
    for (findex <- 0 until length) {
      if (!xyfeaturemap.get(findex).isEmpty)
       for ((xindex,yindex) <- xyfeaturemap(findex)) {
         featurefreq(findex) += xyfreq(xindex)(yindex)
       }
    }
  }
  var fsharp = Array.fill[Double](xlength,ylength){0}
  getfsharp()
  def getfsharp() {
    for (findex <- 0 until length) {
      if (!xyfeaturemap.get(findex).isEmpty)
        for ((xindex,yindex) <- xyfeaturemap(findex)) {
          fsharp(xindex)(yindex) += 1
        }
    }
  }

  var alambda = Array.fill[Double](length){0.0}
  var ll :Double = 0.0
  println("miao")
  trainiis()

  def loglihood() :Double ={
    var ll:Double = 0.0
    ll += (alambda,featurefreq).zipped.map((x,y)=>{y * x}).sum
    ll -= (0 until xlength).map(x => math.log(zlamdba(x)) * (xfreq(x))).sum
    ll
  }

//

  def calpyx(xindex:Int,yindex:Int): Double = {
    //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
    var result:Double = 1
    var sum = 0.0
    for (findex <- 0 until length) {
      if (!xyfeaturemap.get(findex).isEmpty)
        if (xyfeaturemap(findex) contains (xindex,yindex)) {
        sum += alambda(findex)
      }
    }
//    sum = (xyfeaturemap(xindex)(yindex),alambda).zipped.map((x,y)=>{(x.toDouble)*y}).sum
    result *= math.exp(sum)
    result /= zlamdba(xindex)
    if(result > 1) print("!!!")
    result
  }

  def zlamdba(xindex:Int):Double = {
    // = sigmay(e(sigma(lamdbai*fi)))
    var sum:Double = 0.0
    for (yindex <- 0 until ylength) {
      var sum2:Double = 0
      for(findex <- 0 until length) {
        if (!xyfeaturemap.get(findex).isEmpty)
          if (xyfeaturemap(findex) contains (xindex,yindex)) {
          sum2 += alambda(findex)
        }
      }
//      sum2 = (xyfeaturemap(xindex)(yindex),alambda).zipped.map((x,y)=>{(x.toDouble)*y}).sum
      sum += math.exp(sum2)
    }
    sum
  }

  def getFormulaPara(findex:Int) = {
    val para = Array.fill[(Double,Double)](xlength){(0.0,0.0)}  // eyx
    for((xindex,yindex) <- xyfeaturemap(findex)) {
      para(xindex) = (calpyx(xindex,yindex),fsharp(xindex)(yindex)) //plambda,fsharp
    }
//    for(xindex <- 0 until xlength) {
//      for (yindex <- 0 until ylength) {
//
//        if (xyfeaturemap(findex) contains (xindex,yindex)) {
//          para(xindex)  = (calpyx(xindex,yindex),fsharp(xindex)(yindex)) //plambda,fsharp
//        }
//      }
//    }
    para
  }
  def calculateDelta(findex:Int):Double = {
    //simga(simga(f)plambda(y|x)exp(delta*flambda#)) = ~p(f)
    // ~p(f) = ~p(x)plambda(y|x)*exp(feature#)
    val ffreq = featurefreq(findex) // sumxfreq
    if(ffreq == 0) {
      return 0
    }
    var delta:Double = 1
    val para = getFormulaPara(findex)
    val para2 = para.map(x=>x._2)
    var t = 300
    var var1 = Array.fill[Double](xlength){0}
    var var2 = Array.fill[Double](xlength){0}
    for(xindex <- 0 until xlength) {
      var1(xindex) = xfreq(xindex) * para(xindex)._1
      var2(xindex) = xfreq(xindex) * para(xindex)._1 * para(xindex)._2
    }
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
      for(xindex <- 0 until xlength) {
          //sum1 += aebx
          sum1 += var1(xindex) * math.exp(delta * para2(xindex)) // sumxfreq
          //sum2 += abebx
          sum2 += var2(xindex) * math.exp(delta * para2(xindex)) // sumxfreq
      }
      delta -= ((ffreq - sum1))/(-sum2)
      if (math.abs((ffreq - sum1)/(-sum2)) < 1e-14)
        t = 0
      t -= 1
    }
    delta
  }

  def trainiis() {
     var i = 0
    var oldll = 0.0
    while(i < 100){
//      var stop = true
      //val j = Random.nextInt(length)
      for(j <- 0 until length)
      {
        var delta = calculateDelta(j)
        if (delta.isNaN)
        {
          printf("!!!!!%d".format(j))
        }

        alambda(j) += delta
//        if (delta > 1e-12)
//          stop = false
      }
      println("-iter %d------------".format(i))
      oldll = ll
      ll= loglihood()
      println(ll)
      println("-iter %d------------".format(i))
//      if (stop)
//        i = 100
      i += 1
    }
    ll= loglihood()
  }

}

