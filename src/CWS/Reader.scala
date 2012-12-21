package CWS

import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 10:27 PM
 * To change this template use File | Settings | File Templates.
 */
object Reader {
  def segRead(f:String) = {
    val source = scala.io.Source.fromFile(f)
    val lines = source.getLines()
    var a = new ArrayBuffer[List[Point2]]()
    var l = List[Point2]()
    //val P1 = """(.) ([BIES])(?:_([A-Z]{1,3})?)""".r
    for (x <- lines) {
      if(x.isEmpty) {
        l = l.reverse
        //println(l)
        a append l
        l =  List[Point2]()
      } else {
        //val P1(c,p,t) = x
        if(x.length>3)
          l = (new Point2(x.charAt(0),x.charAt(2),x.substring(4))) :: l
        else
          l = (new Point2(x.charAt(0),x.charAt(2),"")) :: l
      }
    }
    source.close ()
    a
  }
}
