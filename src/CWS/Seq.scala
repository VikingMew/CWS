package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 9:56 PM
 * To change this template use File | Settings | File Templates.
 */
class Point(cc: Char, cp:Int = 0,ct:Int = 0) {
  var c = cc
  var p = cp
  var t = ct
  override def toString(): String = "(%c,%d,%d)".format(c,p,t)
}
class Point2(cc: Char, cp:Char = 0,ct:String = "") {
  var c = cc
  var p = cp
  var t = ct
  override def toString(): String = "(%c,%c,%s)".format(c,p,t)
}
class Seq() {
  var list = Nil
  //override def toString(): String = "(%c,%d,%d)".format(c,p,t)
}

