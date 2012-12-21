package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 9:48 PM                                   r
 * To change this template use File | Settings | File Templates.
 */
object main {
  def main(args:Array[String]) = {
    val l = time(m)
    val h = l.head;
    print(Feature.c(h,2,List(1,3))(h,2))
  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now).toDouble / 1000000
    println("%f mseconds".format(micros))
    result
  }
  def m = {
    Reader.segRead("dat/ctb7_mz_seg.utf8")
    Reader.segRead("dat/ctb7_mz_pos.utf8")
  }
}
