package CWS

import collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import collection.mutable

//import collection.immutable.HashSet
import collection.mutable.HashSet


/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/22/12
 * Time: 2:01 AM
 * To change this template use File | Settings | File Templates.
 */
//FeatureRun List
//c: character
//t: tag
//p: pos tag
//w: word
//u: punc
//s: single character word (not implemented)
//l: length of word        (not implemented)
//r: repeat char           (not implemented)

//object FeatureRun {
//  //val t = List[Array]();
//  def c(a:Array[Point],index:Int,offset:Int) :(Array[Point],Int)=>Int = {
//    if(index+offset>=0 && index+offset < a.length)
//      (b:Array[Point],index2:Int) => {
//        if(index2+offset < b.length)
//          if(b(index2+offset).c ==  a(index+offset).c) 1 else 0
//        else
//          0
//      }
//    else
//      null
//  }
//  def c(a:Array[Point],index:Int,offset:List[Int]) :(Array[Point],Int)=>  Int = {
//    if(index+offset.min >=0 && index+offset.max < a.length)
//      (b:Array[Point],index2:Int) => {
//        if(index2+offset.max < b.length){
//          offset.map(x => c(a,index,x)(b,index2)).reduceLeft(_&_)
//        }
//        else
//          0
//      }
//    else
//      null
//  }
//  //def createc
//}


class FeatureTemplate(featuretemplateset:List[(Char,Int)]) {
  val template = featuretemplateset
  val maxoffset = template.map(x => x._2).max
  val minoffset = template.map(x => x._2).min

  //var list = new HashMap[List[String],FeatureRun]()
  var list =  new mutable.HashSet[(List[(String,Char,Int)],String)]()
  def createFeature(a:Array[T.Point],offset:Int) = {
    var l = List[(String,Char,Int)]()
      for(x <- template) {
        val p = {
        if(offset + x._2 < 0 || offset + x._2 >= a.length)
          new T.Point(0,"","")
        else
          a(offset + x._2)
        }
        val t = x._1 match {
          case 'c' => (p._1.toString, x._1, x._2)
          case 't' => (p._2.toString, x._1, x._2)
          case 'p' => (p._3.toString, x._1, x._2)
          case 'u' => {
            if((""" ?「」，。《》、：""" contains p._1))
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
          case 's' => {
            if(p._2 == "S")
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
        }
        l = t :: l
      }
      l = l.reverse
      val l1 = (l,a(offset)._2)
      if (!list.contains(l1)) {
        list.add(l1)
      }

    l
  }
  def createFeature(a:Array[T.Point]):List[(List[(String,Char,Int)],String)]= {
    val length = a.length
    var i = 0
    var list2 = List[List[(String,Char,Int)]]()
    while(i < length) {
      createFeature(a,i)
      i += 1
    }
    list.toList
  }
  def createFeature(a:List[Array[T.Point]]) :List[(List[(String,Char,Int)],String)]= {
    a.map(x => createFeature(x)).flatten
    list.toList
  }
  def getFeatureFunc() = {
    Unit
  }
//  def createFeatureAndToken(a:List[Array[Point]]):List[(List[(String,Char,Int)],String)] = {
//    a.map(x => createFeatureAndToken(x)).flatten
//  }
//  def createFeatureAndToken(a:Array[Point]): List[(List[(String,Char,Int)],String)]= {
//    val length = a.length
//    var i = 0
//    var list = List[(List[(String,Char,Int)],String)]()
//    while(i < length) {
//      var x = createFeature(a,i)
//      list = (x,a(i).t) ::list
//      i += 1
//    }
//    list
//  }
}
class Feature(carg:(List[(String,Char,Int)],String)) {
  val arg = carg._1
  val y = carg._2
  def checkx(b:Array[T.Point],index2:Int) :Int = {
    arg.map(x => cal(b,index2,x)).reduce(_&_)
  }
  def checky(y:String) :Int = {
    if(y equals this.y) 1 else 0
  }
  def run(b:Array[T.Point],index2:Int,y:String) :Int = {
    checkx(b,index2) & checky(y)
  }
  def run(x:List[(String,Char,Int)],y:String):Int = {
    var maxoffset = x.map(x=>x._3).max
    var minoffset = x.map(x=>x._3).min
    var arr = Array.fill[T.Point](maxoffset - minoffset + 1){(0,"","" )}
    for (i <- x) {
      val tmp = i._3 - minoffset
      i._2 match {
        case 'c' => {
          arr(tmp) = (i._1.charAt(0),arr(tmp)._2,arr(tmp)._3)
        }
        case 't' => {
          arr(tmp) = (arr(tmp)._1  ,i._1        ,arr(tmp)._3)
        }
        case 'p' => {
          arr(tmp) = (arr(tmp)._1  ,arr(tmp)._2,        i._1)
        }
        case 'u' => {
          if(i._1 == "T") {
            arr(tmp) = ('》',arr(tmp)._2,arr(tmp)._3)
          }
        }
        case 's' => {
          if(i._1 == "T") {
            arr(tmp) = (arr(tmp)._1,"S",arr(tmp)._3)
          }
        }
      }
    }
    return checkx(arr,-minoffset) &checky(y)
  }
  private def cal(b:Array[T.Point],index2:Int,t:(String,Char,Int)):Int = {
    val p = {
      if(index2+t._3 < 0 || index2+t._3 >= b.length)
        new T.Point(0,"","")
      else
        b(index2+t._3)
    }
    t._2 match {
      case 'c' => {
        if(t._1 contains p._1) 1 else 0
      }
      case 't' => {
        if(t._1 == p._2) 1 else 0
      }
      case 'p' => {
        if(t._1 == p._3) 1 else 0
      }
      case 'u' => {
        val v = if(""" ?「」，。《》、：""" contains p._1) 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 's' => {
        val v = if(p._2 == "S") 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
    }
  }
}