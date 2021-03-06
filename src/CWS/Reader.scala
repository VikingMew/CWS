package CWS

import collection.mutable.ArrayBuffer
import collection.mutable

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
    var l = List[Array[util.Point]]()
    var a = ArrayBuffer[util.Point]()
    for (x <- lines) {
      if(x.isEmpty) {
        l = a.toArray :: l
        a =  new ArrayBuffer[util.Point]()
      } else {
        if(x.length>3)
          a.append((new util.Point(x.charAt(0),x.charAt(2).toString,x.substring(4))))
        else
          a.append((new util.Point(x.charAt(0),x.charAt(2).toString,"")))
      }
    }
    source.close ()
    l.reverse
  }
  def seg2Read(f:String) = {
    val source = scala.io.Source.fromFile(f)
    val lines = source.getLines()
    //var l = List[Array[Point]]()
    var a = ArrayBuffer[util.Point]()
    for (x <- lines) {
      if(!x.isEmpty) {
        if(x.length>3)
          a.append(new util.Point(x.charAt(0),x.charAt(2).toString,x.substring(4)))
        else
          a.append(new util.Point(x.charAt(0),x.charAt(2).toString,""))
      }
    }
    source.close()
    a.toArray
  }
  def TagRead(f:String) = {
    val source = scala.io.Source.fromFile(f)
    val lines = source.getLines()
    var pos = mutable.HashSet[String]()
    var tag = mutable.HashSet[String]()
    for (x <- lines) {
      if(!x.isEmpty) {
        if(x.length>3) {
          tag += x.charAt(0).toString()
          pos += x.substring(2)
        }
      }
    }
    source.close()
    (tag,pos)
  }
}
