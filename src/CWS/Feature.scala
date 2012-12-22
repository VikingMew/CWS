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
object Feature {
  //val t = List[Array]();
  def c(a:Array[Point2],index:Int,offset:Int) :(Array[Point2],Int)=>Int = {
    if(index+offset>=0 && index+offset < a.length)
      (b:Array[Point2],index2:Int) => {
        if(index2+offset < b.length)
          if(b(index2+offset).c ==  a(index+offset).c) 1 else 0
        else
          0
      }
    else
      null
  }
  def c(a:Array[Point2],index:Int,offset:List[Int]) :(Array[Point2],Int)=>  Int = {
    if(index+offset.min >=0 && index+offset.max < a.length)
      (b:Array[Point2],index2:Int) => {
        if(index2+offset.max < b.length){
          offset.map(x => c(a,index,x)(b,index2)).reduceLeft(_&_)
        }
        else
          0
      }
    else
      null
  }
  //def createc
}

class FeatureTemplate(ta:List[Tuple2[Char,Int]]) {
  val template = ta
  val maxoffset = template.map(x => x._2).max
  val minoffset = template.map(x => x._2).min

  //var list = new HashMap[List[String],Feature]()
  var list = new HashSet[List[Tuple3[String,Char,Int]]]()
  def createFeature(a:Array[Point2],offset:Int):Unit = {
    if (offset + maxoffset < a.length && offset + minoffset >= 0) {
      var l = List[Tuple3[String,Char,Int]]()
      for(x <- template) {
        val t = x._1 match{
          case 'c' => Tuple3(a(offset + x._2).c.toString, x._1, x._2)
        }
        l = t :: l
//        l = Tuple3(a(offset + x._2),x._1,x._2) :: l
      }
      l = l.reverse
      if (!list.contains(l))
        list += l
     // println(list)
    }
  }
  def createFeature(a:Array[Point2]):Unit = {
    val length = a.length;
    var i = 0;
    while(i < length) {
      createFeature(a,i)
      i += 1
    }
  }
  def createFeature(a:List[Array[Point2]]):Unit = {
    a.map(x => createFeature(x))
    Unit
  }
}
//class Feature(arg:List[Tuple3[String,Char,Int]]) {
//  val a = arg
//  def cal(b:Array[Point2],index2:Int) ={
//    a.map().reduce(_&_)
//  }
//  def c(a:Point2)(b:Point2) = {
//    if (a.c == b.c) 1 else 0
//  }
//  def t(a:Point2)(b:Point2) = {
//    if (a.t == b.t) 1 else 0
//  }
//  def p(a:Point2)(b:Point2) = {
//    if (a.p == b.p) 1 else 0
//  }
//}