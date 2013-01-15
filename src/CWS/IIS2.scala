package CWS

import collection.mutable

/**
* Created with IntelliJ IDEA.
* User: vikingmew
* Date: 12/26/12
* Time: 8:32 PM
*/
class IIS2 (cfeatureset:List[(List[(String,Char,Int)],String)],ctext:List[Array[util.Point]],clabels:List[String]) {
  val text = ctext
  val count:Int = text.length
  val featureset = cfeatureset.toArray
  val length = featureset.length

  var xlist = getx()    //xindex ->x
  def getx() = {
    var l = List[List[(String,Char,Int)]]()
    for(f <- featureset) {
      if(!(l contains f._1)) {
        l = f._1 :: l
      }
    }
    l.toArray
  }
  println(xlist.length)
  var xfreq = Array.fill[Int](xlist.length){0}                    //xindex ->xfreq
  getxfreq()
  def getxfreq() {
    for (sentences <- text) {
      for (xindex <- 0 until xlist.length) {
        val f = new Feature((xlist(xindex),""))
        var i = 0
        while(i < sentences.length) {
          if (f.checkx(sentences,i) == 1) {
            xfreq(xindex) += 1
          }
          i += 1
        }
      }
    }
  }

  val ylist = clabels.toArray                                       //yindex -> y

//  var fsmap = new Array[(Int,Int)](length)                  //fsindex -> xindex,yindex
//  getfsmap()
//  def getfsmap(){
//    for(findex <- 0 until features.length) {
//
//    }
//  }

  var xyfreq = Array.fill[Int](xlist.length,ylist.length){0}                       //xyindex -> xyfreq
  getxyfreq()
  def getxyfreq() {
    for (sentences <- text) {
      for (xindex <- 0 until xlist.length) {
        for (yindex <- 0 until ylist.length) {
          val f = new Feature((xlist(xindex),ylist(yindex)))
          var i = 0
          while(i < sentences.length) {
            if (f.run(sentences,i,sentences(i)._2) == 1) {
              xyfreq(xindex)(yindex) += 1
            }
            i += 1
          }
        }
      }
    }
  }

  var features = new Array[Feature](length)
  setfeature()
  def setfeature() {
    var i = 0
    while(i < length) {
      features(i) = new Feature(featureset(i))
      i += 1
    }
  }


  var featurefreq = Array.fill[Int](length){0}                  //fsindex->fsfreq
  getfeaturefreq()
  def getfeaturefreq() {
    for (findex <- 0 until length) {
      for(xindex <- 0 until xlist.length) {
        for (yindex <- 0 until ylist.length) {
          if(features(findex).run(xlist(xindex),ylist(yindex)) == 1) {
            featurefreq(findex) += xyfreq(xindex)(yindex)
          }
        }
      }
    }
  }

  var fsharp = Array.fill[Int](xlist.length,ylist.length){0}
  getfsharp()
  def getfsharp() {
    for (findex <- 0 until length) {
      for (xindex <- 0 until xlist.length) {
        for (yindex <- 0 until ylist.length) {
          if(features(findex).run(xlist(xindex),ylist(yindex)) == 1)
            fsharp(xindex)(yindex) += 1
        }
      }
    }
  }
  var alambda = Array.fill[Double](length){0.0}
  var ll :Double = 0.0
  trainiis()
  def loglihood() :Double ={
    var ll:Double = 0.0
    (0 until length).foreach(findex =>{
      ll += alambda(findex)*(featurefreq(findex).toDouble)
    })
    for (xindex <- 0 until xlist.length) {
      ll -= math.log(zlamdba(xindex)) * (xfreq(xindex).toDouble)
    }
    ll
  }


  def calpyx(xindex:Int,yindex:Int): Double = {
    //plamdba(y|x)=(1/Z(lambda,x)) * e(sigma(lambdai*fi))
    var result:Double = 1
    var sum = 0.0
    for (findex <- 0 until length) {
      if (features(findex).run(xlist(xindex),ylist(yindex)) == 1) {
        sum += alambda(findex)
      }
    }
    result *= math.exp(sum)
    result /= zlamdba(xindex)
    result
  }

  def zlamdba(xindex:Int):Double = {
    // = sigmay(e(sigma(lamdbai*fi)))
    var sum:Double = 0.0
    for (yindex <- 0 until ylist.length) {
      var sum2:Double = 0
      for(findex <- 0 until length) {
        if (features(findex).run(xlist(xindex),ylist(yindex)) == 1) {
          sum2 += alambda(findex)
        }
      }
      sum += math.exp(sum2)
    }
    sum
  }

  def getFormulaPara(findex:Int) = {
    val para = Array.fill[(Double,Double)](xlist.length){(0.0,0.0)}  // eyx
    for(xindex <- 0 until xlist.length) {
      for (yindex <- 0 until ylist.length) {
        if(features(findex).run(xlist(xindex),ylist(yindex)) == 1) {
          para(xindex)  = (calpyx(xindex,yindex),fsharp(xindex)(yindex).toDouble) //plambda,fsharp
        }
      }
    }
    para
  }
  def calculateDelta(findex:Int):Double = {
    //simga(simga(f)plambda(y|x)exp(delta*flambda#)) = ~p(f)
    // ~p(f) = ~p(x)plambda(y|x)*exp(feature#)
    var delta:Double = 1
    val para = getFormulaPara(findex)
    var t = 300
    val ffreq = featurefreq(findex)
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
      for(xindex <- 0 until xlist.length) {
          //sum1 += aebx
          sum1 += xfreq(xindex) * para(xindex)._1 * math.exp(delta * para(xindex)._2)
          //sum2 += abebx
          sum2 += xfreq(xindex) * para(xindex)._1 * para(xindex)._2 * math.exp(delta * para(xindex)._2)
      }
      delta -= ((ffreq.toDouble - sum1))/(-sum2)
      if (math.abs((ffreq - sum1)/(-sum2)) < 1e-16)
        t = 0
      t -= 1
    }
    delta
  }

  def trainiis() {
     var i = 0
    while(i < 100){
      var stop = true
      for (j <- 0 until length){
        var delta = calculateDelta(j)
        alambda(j) += delta
        if (delta > 1e-16)
          stop = false
      }
      println("-iter %d------------".format(i))
      println(alambda.mkString(" "))
      ll= loglihood()
      println(ll)
      for (xindex <- 0 until xlist.length) {
        print("%f ".format(zlamdba(xindex)))
      }
      println()
      println("-iter %d------------".format(i))
      if (stop)
        i = 10000
      i += 1
    }
    (0 until xlist.length).foreach(x=>{
      (0 until ylist.length).foreach(y=>{
      println("%d %d %f".format(x,y,calpyx(x,y)))
      })
    })
  }
}

