package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 9:48 PM                                   r
 */
object main {
  def main(args:Array[String])  {
    time(m)
  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now).toDouble / 1000000
    println("\n%f mseconds".format(micros))
    result
  }
  def m {
    Reader.segRead("dat/ctb7_mz_seg.utf8")
    val l = Reader.segRead("dat/ctb7_mz_pos.utf8")
    var t = Reader.TagRead("dat/ctb7_mz_pos_tags.utf8")
    val h = l.head
//    l.map(x =>{
//      val t = Feature.c(x,2,List(1,3,5,7))
//      if(t != null) t(x,3) else null
//    })
    val templatelist = List(('c',0),('u',0),('s',0))
    var template = new FeatureTemplate(templatelist)
    template.createFeature(h)
    print(t);
    //println(t.list)
    //t.createFeature(l);
//    t.list.foreach(x =>{
//      val a = new Feature(x)
//
//      print(a.run(h,0))
//    })
    //println(t.list)
  }
}
