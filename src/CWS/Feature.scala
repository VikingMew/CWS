package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/22/12
 * Time: 2:01 AM
 * To change this template use File | Settings | File Templates.
 */
object Feature {
      def c0(a:Array[Point2],index:Int) = {
        (b:Array[Point2],index2:Int) => if(b(index2).c ==  a(index).c) 1 else 0

      }
}
