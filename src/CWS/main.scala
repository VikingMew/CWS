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
    time(m)
  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now).toDouble / 1000000
    println("%f mseconds".format(micros))
    result
  }
  def m = {
    //println("Hello world")
    //var p = new Point('喵')
    //var q = new Seq('汪',1,2)
    //print(p);
    //print(q);
    Reader.segRead("dat/ctb7_mz_pos.utf8")
    print(Reader.segRead("dat/ctb7_mz_seg.utf8"))

  }
}
