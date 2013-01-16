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
  def createFeature(a:Array[util.Point],offset:Int) = {
    var l = List[(String,Char,Int)]()
      for(x <- template) {
        val p = {
        if(offset + x._2 < 0 || offset + x._2 >= a.length)
          new util.Point(0,"","")
        else
          a(offset + x._2)
        }
        val t = x._1 match {
          case 'c' => (p._1.toString, x._1, x._2)
          case 't' => (p._2.toString, x._1, x._2)
          case 'p' =>  {
            var i = 0
            var word = ""
            var cursor = 0
            while(i > x._2)  {
              while((offset + cursor > 0) && !("SB" contains a(offset + cursor)._2))
                cursor -= 1
              i-= 1
            }
            if ((offset + cursor > 0))
              cursor -= 1
            if ((offset + cursor > 0))
              word = a(offset + cursor)._3
            (word, x._1, x._2)
          }

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
          case 'r' => {
            val p2 = {
              if(offset + x._2 - 1< 0 || offset + x._2 - 1 >= a.length)
                new util.Point(0,"","")
              else
                a(offset + x._2 - 1)
            }
            if ((p._1) == (p2._1))
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
          case 'R' => {
            val p2 = {
              if(offset + x._2 - 2< 0 || offset + x._2 - 2 >= a.length)
                new util.Point(0,"","")
              else
                a(offset + x._2 - 2)
            }
            if ((p._1) == (p2._1))
              ("T" , x._1, x._2)
            else
              ("F" , x._1, x._2)
          }
          case 'w' => {
            var i = 0
            var word = ""
            var cursor = 0
            while(i > x._2)  {
              while((offset + cursor > 0) && !("SB" contains a(offset + cursor)._2))
                cursor -= 1
              i-= 1
            }
            if ((offset + cursor > 0))
              cursor -= 1
            if ((offset + cursor > 0))
              word = (a(offset + cursor)._1.toString) + word
             while((offset + cursor > 0)  && !("SB" contains a(offset + cursor)._2)) {
               cursor -= 1
              word = (a(offset + cursor)._1.toString) + word
            }
            (word , x._1, x._2)
          }
          case 'b' => {
            var i = 0
            var word = ""
            var cursor = 0
            while(i > x._2)  {
              while((offset + cursor > 0) && !("SB" contains a(offset + cursor)._2))
                cursor -= 1
              i-= 1
            }
            if ((offset + cursor > 0))
              cursor -= 1
            if ((offset + cursor > 0))
              word = (a(offset + cursor)._1.toString) + word
            while((offset + cursor > 0)  && !("SB" contains a(offset + cursor)._2)) {
              cursor -= 1
              word = (a(offset + cursor)._1.toString) + word
            }
            if (word.length > 0)
              (word.charAt(0).toString , x._1, x._2)
            else
              ("" , x._1, x._2)
          }
          case 'e' => {
            var i = 0
            var word = ""
            var cursor = 0
            while(i > x._2)  {
              while((offset + cursor > 0) && !("SB" contains a(offset + cursor)._2))
                cursor -= 1
              i-= 1
            }
            if ((offset + cursor > 0))
              cursor -= 1
            if ((offset + cursor > 0))
              word = (a(offset + cursor)._1.toString) + word
            while((offset + cursor > 0)  && !("SB" contains a(offset + cursor)._2)) {
              cursor -= 1
              word = (a(offset + cursor)._1.toString) + word
            }
            if (word.length > 0)
              (word.last.toString , x._1, x._2)
            else
              ("" , x._1, x._2)
          }
          case 'l' => {
            var i = 0
            var word = ""
            var cursor = 0
            while(i > x._2)  {
              while((offset + cursor > 0) && !("SB" contains a(offset + cursor)._2))
                cursor -= 1
              i-= 1
            }
            if ((offset + cursor > 0))
              cursor -= 1
            if ((offset + cursor > 0))
              word = (a(offset + cursor)._1.toString) + word
            while((offset + cursor > 0)  && !("SB" contains a(offset + cursor)._2)) {
              cursor -= 1
              word = (a(offset + cursor)._1.toString) + word
            }
            (word.length.toString, x._1, x._2)
          }
//          case 'C' => {    //class:space,punc,num,letter,date,symbol,character,other
//
//          }
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
  def createFeature(a:Array[util.Point]):List[(List[(String,Char,Int)],String)]= {
    val length = a.length
    var i = 0
    var list2 = List[List[(String,Char,Int)]]()
    while(i < length) {
      createFeature(a,i)
      i += 1
    }
    list.toList
  }
  def createFeature(a:List[Array[util.Point]]) :List[(List[(String,Char,Int)],String)]= {
    a.map(x => createFeature(x)).flatten
    list.toList
  }
//  def getFeatureFunc() = {
//    Unit
//  }
}
class Feature(carg:(List[(String,Char,Int)],String)) {
  val arg = carg._1
  val y = carg._2
  def checkx(b:Array[util.Point],index2:Int) :Int = {
    arg.map(x => cal(b,index2,x)).reduce(_&_)
  }
  def checky(y:String) :Int = {
    if(y equals this.y) 1 else 0
  }
  def run(b:Array[util.Point],index2:Int,y:String) :Int = {
    checkx(b,index2) & checky(y)
  }
  def run(x:List[(String,Char,Int)],y:String):Int = {
    var maxoffset = x.map(x=>x._3).max
    var minoffset = x.map(x=>x._3).min
    var arr = Array.fill[util.Point](maxoffset - minoffset + 1){(0,"","" )}
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
  private def cal(b:Array[util.Point],index2:Int,t:(String,Char,Int)):Int = {
    val p = b(index2+t._3)
    t._2 match {
      case 'c' => {
        if(t._1 contains p._1) 1 else 0
      }
      case 't' => {
        if(t._1 == p._2) 1 else 0
      }
      case 'p' => {
        var i = 0
        var word = ""
        var cursor = 0
        while(i > t._3)  {
          while((index2 + cursor > 0) && !("SB" contains b(index2 + cursor)._2))
            cursor -= 1
          i-=1
        }
        if ((index2 + cursor > 0))
          cursor -= 1
        if ((index2 + cursor > 0))
          word = b(index2 + cursor)._3
        if (word eq t._1) 1 else 0
      }
      case 'u' => {
        val v = if(""" ?「」，。《》、：""" contains p._1) 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 's' => {
        val v = if(p._2 == "S") 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 'r' => {
        val p2 = b(index2+t._3 -1)
        val v = if ((p._1) == (p2._1)) 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 'R' => {
        val p2 = b(index2+t._3 -2)
        val v = if ((p._1) == (p2._1)) 1 else 0
        if ("T" contains t._1) v else 1 - v
      }
      case 'w' => {
        var i = 0
        var word = ""
        var cursor = 0
        while(i > t._3)  {
          while((index2 + cursor > 0) && !("SB" contains b(index2 + cursor)._2))
            cursor -= 1
          i-=1
        }
        if ((index2 + cursor > 0))
          cursor -= 1
        if ((index2 + cursor > 0))
          word = (b(index2 + cursor)._1.toString) + word
        while((index2 + cursor > 0)  && !("SB" contains b(index2 + cursor)._2)) {
          cursor -= 1
          word = (b(index2 + cursor)._1.toString) + word
        }
        if (word eq t._1) 1 else 0
      }
      case 'b' => {
        var i = 0
        var word = ""
        var cursor = 0
        while(i > t._3)  {
          while((index2 + cursor > 0) && !("SB" contains b(index2 + cursor)._2))
            cursor -= 1
          i-=1
        }
        if ((index2 + cursor > 0))
          cursor -= 1
        if ((index2 + cursor > 0))
          word = (b(index2 + cursor)._1.toString) + word
        while((index2 + cursor > 0)  && !("SB" contains b(index2 + cursor)._2)) {
          cursor -= 1
          word = (b(index2 + cursor)._1.toString) + word
        }
        if (word.length > 0)
          word =word.charAt(0).toString
        if (word eq t._1) 1 else 0
      }
      case 'e' => {
        var i = 0
        var word = ""
        var cursor = 0
        while(i > t._3)  {
          while((index2 + cursor > 0) && !("SB" contains b(index2 + cursor)._2))
            cursor -= 1
          i-=1
        }
        if ((index2 + cursor > 0))
          cursor -= 1
        if ((index2 + cursor > 0))
          word = (b(index2 + cursor)._1.toString) + word
        while((index2 + cursor > 0)  && !("SB" contains b(index2 + cursor)._2)) {
          cursor -= 1
          word = (b(index2 + cursor)._1.toString) + word
        }
        if (word.length > 0)
          word =word.last.toString
        if (word eq t._1) 1 else 0
      }
      case 'l' => {
        var i = 0
        var word = ""
        var cursor = 0
        while(i > t._3)  {
          while((index2 + cursor > 0) && !("SB" contains b(index2 + cursor)._2))
            cursor -= 1
          i-=1
        }
        if ((index2 + cursor > 0))
          cursor -= 1
        if ((index2 + cursor > 0))
          word = (b(index2 + cursor)._1.toString) + word
        while((index2 + cursor > 0)  && !("SB" contains b(index2 + cursor)._2)) {
          cursor -= 1
          word = (b(index2 + cursor)._1.toString) + word
        }
        word =word.length.toString
        if (word eq t._1) 1 else 0
      }
    }
  }
}