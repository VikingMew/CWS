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
      (b:Array[Point2],index2:Int) = {
        if(index2+offset < b.length)
          if(b(index2+offset).c ==  a(index+offset).c) 1 else 0
      }
    else
      null
  }
  def c(a:Array[Point2],index:Int,offset:List[Int]) = {
    if(index+offset.min >=0 && index+offset.max < a.length)
      (b:Array[Point2],index2:Int) = {
        if(index2+offset.max < b.length)
           offset.map(x =>c(b,index2,x)).reduce(*)
      }
    else
      null
  }

}
