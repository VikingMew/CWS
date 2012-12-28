package CWS

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

package object T {
  type Point = (Char,String,String)
  type Feature = (List[(String,Char,Int)],String)
}