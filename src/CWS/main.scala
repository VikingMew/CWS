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

  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now).toDouble / 1000000
    println("\n%f mseconds".format(micros))
    result
  }
  def m = {
    Reader.segRead("dat/ctb7_mz_seg.utf8")
    val l = Reader.segRead("dat/ctb7_mz_pos.utf8")
    val h = l.head;
    l.map(x =>{
      val t = Feature.c(x,2,List(1,3,5,7))
      if(t != null) t(x,3) else null
    })
    val templatelist = List(('c',0),('c',1),('c',-1))
    var t = new FeatureTemplate(templatelist)
    //t.createFeature(h,2);
    //t.createFeature(h,3);
    //t.createFeature(h,2);
    t.createFeature(l);
    println(t.list)
  }
}