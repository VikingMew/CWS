package CWS
import scala.collection.mutable.{WrappedArray=>WArray}
/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 9:56 PM
 * To change this template use File | Settings | File Templates.
// */
//class Point(cc: Char, ct:String = "",cpos:String = "") {
//  var c = cc
//  var t = ct
//  var pos = cpos
//  override def toString(): String = "(%c,%s,%s)".format(c,t,pos)
//}
//class Seq() {
//  var list = Nil
//  //override def toString(): String = "(%c,%d,%d)".format(c,p,t)
//}

object util {
  type Point = (Char,String,String)
  type Feature = (List[(String,Char,Int)],String)
  type Window = (List[Point],Int)    //0index
  val window = (-2,0)
  def slice(sentence:Array[util.Point],index:Int):Window={
    val t = Array.fill[Point](window._2 - window._1 +1 ){(0,"","")}
    (window._1 until (window._2+1)).foreach(x => {
      var p = {
        if(index + x < 0 || index + x >= sentence.length)
          new util.Point(0,"","")
        else
          sentence(index + x)
      }
      if(x == 0) {
        t(x - window._1) = (p._1,"",p._3) //del t0
      }
        else {
        t(x - window._1) = p
      }
    })
    (t.toList,-window._1)
  }
  def comparex(x:Window,sen:Window):Boolean= {
    return x._1 == sen._1
  }
}