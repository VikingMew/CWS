package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/22/12
 * Time: 2:01 AM
 * To change this template use File | Settings | File Templates.
 */
object Feature {
  def c(a:Array[Point2],index:Int,offset:Int) = {
    if(index+offset>=0 && index+offset < a.length)
    (b:Array[Point2],index2:Int) =>
      if(index+offset>=0 && index+offset < b.length)
        if(b(index2+offset).c ==  a(index+offset).c) 1 else 0
    else
      null
  }

}
