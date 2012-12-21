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
    var l = List[Array[Point2]]()
    var a = ArrayBuffer[Point2]()
    for (x <- lines) {
      if(x.isEmpty) {
        l = a.toArray :: l
        a =  new ArrayBuffer[Point2]()
      } else {
        if(x.length>3)
          a.append((new Point2(x.charAt(0),x.charAt(2),x.substring(4))))
        else
          a.append((new Point2(x.charAt(0),x.charAt(2),"")))
      }
    }
    source.close ()
    l.reverse
  }
  def seg2Read(f:String) = {
    val source = scala.io.Source.fromFile(f)
    val lines = source.getLines()
    //var l = List[Array[Point2]]()
    var a = ArrayBuffer[Point2]()
    for (x <- lines) {
      if(!x.isEmpty) {
        if(x.length>3)
          a.append((new Point2(x.charAt(0),x.charAt(2),x.substring(4))))
        else
          a.append((new Point2(x.charAt(0),x.charAt(2),"")))
      }
    }
    source.close()
    a.toArray
  }
}
