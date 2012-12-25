package CWS

import collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
//import collection.immutable.HashSet
import collection.mutable.HashSet


/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/22/12
 * Time: 2:01 AM
 * To change this template use File | Settings | File Templates.
 */
//Feature List
//c: character
//t: tag
//p: pos tag
//w: word
//u: punc
//s: single character word (not implemented)
//l: length of word        (not implemented)
//r: repeat char           (not implemented)

//object Feature {
//  //val t = List[Array]();
//  def c(a:Array[Point2],index:Int,offset:Int) :(Array[Point2],Int)=>Int = {
//    if(index+offset>=0 && index+offset < a.length)
//      (b:Array[Point2],index2:Int) => {
//        if(index2+offset < b.length)
//          if(b(index2+offset).c ==  a(index+offset).c) 1 else 0
//        else
//          0
//      }
//    else
//      null
//  }
//  def c(a:Array[Point2],index:Int,offset:List[Int]) :(Array[Point2],Int)=>  Int = {
//    if(index+offset.min >=0 && index+offset.max < a.length)
//      (b:Array[Point2],index2:Int) => {
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

  //var list = new HashMap[List[String],Feature]()
  var list = new HashSet[List[(String,Char,Int)]]()
  def createFeature(a:Array[Point2],offset:Int) = {
    var l = List[(String,Char,Int)]()
    if (offset + maxoffset < a.length && offset + minoffset >= 0) {
      for(x <- template) {
        val t = x._1 match{
          case 'c' => (a(offset + x._2).c.toString, x._1, x._2)
          case 't' => (a(offset + x._2).t.toString, x._1, x._2)
          case 'p' => (a(offset + x._2).pos.toString, x._1, x._2)
          case 'u' => {
            if(""" ?「」，。《》、：""" contains a(offset + x._2).c)
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
          case 's' => {
            if(a(offset + x._2).t == 'S')
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
        }
        l = t :: l
      }
      l = l.reverse
      if (!list.contains(l))
        list += l

    }
    list
  }
  def createFeature(a:Array[Point2])= {
    val length = a.length
    var i = 0
    var list = List[List[(String,Char,Int)]]()
    while(i < length) {
      var x = createFeature(a,i)
      if (!x.isEmpty && list.contains(x))
        list = x :: list
      i += 1
    }
    list
  }
  def createFeature(a:List[Array[Point2]]) = {
    a.map(x => createFeature(x)).flatten
  }
  def getFeatureFunc() = {
    Unit
  }
  def createFeatureAndToken(a:List[Array[Point2]]) = {
    a.map(x => createFeatureAndToken(x)).flatten
  }
  def createFeatureAndToken(a:Array[Point2]) = {
    val length = a.length
    var i = 0
    var list = List[(List[(String,Char,Int)],String)]()
    while(i < length) {
      var x = createFeature(a,i)
      list = (x,a(i).t) ::list
      i += 1
    }
    list
  }
}
class Feature(carg:List[(String,Char,Int)]) {
  val arg = carg
  def run(b:Array[Point2],index2:Int) = {
    arg.map(x => cal(b,index2,x)).reduce(_&_)
  }
  def cal(b:Array[Point2],index2:Int,t:(String,Char,Int)):Int = {
    t._2 match {
      case 'c' => {
        if(t._1 contains b(index2+t._3).c) 1 else 0
      }
      case 't' => {
        if(t._1 contains b(index2 + t._3).t) 1 else 0
      }
      case 'p' => {
        if(t._1 contains b(index2+t._3).pos) 1 else 0
      }
      case 'u' => {
        val v = if(""" ?「」，。《》、：""" contains b(index2 + t._3).c) 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 's' => {
        val v = if(b(index2 + t._3).t == 'S') 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
    }
  }
}