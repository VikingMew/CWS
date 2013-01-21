package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/21/12
 * Time: 9:48 PM                                   r
 */


object Miao {
  def main(args:Array[String]) = {
    time(m)
  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now).toDouble / 1000000
    println("\n%f mseconds".format(micros))
    result
  }

  def m = {
    val text = Reader.segRead("dat/ctb7_mz_seg.utf8")
    //val text2 = Reader.seg2Read("dat/ctb7_mz_pos.utf8")
    var tag = Reader.TagRead("dat/ctb7_mz_pos_tags.utf8")
    val head = text.head
    var alltemplate = List[List[(Char,Int)]]()
    alltemplate  = List(('c',-1),('c',1)) :: alltemplate
    //alltemplate  = List(('c',0)) :: alltemplate
    println(alltemplate)
    var featuresets = List[(List[(String,Char,Int)],String)]()

    for(template <- alltemplate ) {
      var ftemplate = new FeatureTemplate(template)
      ftemplate.createFeature(text)
      var featureset = ftemplate.list.toList
      featuresets = featureset ::: featuresets
    }

//    var ftemplate2 = new FeatureTemplate(template2)
//    ftemplate2.createFeature(text)

//    var featuresets2 = ftemplate2.list.toList
   // var featuresets = featuresets1 //::: featuresets2
    println("created,length=%s".format(featuresets.size))
    println(featuresets)
    var iis = new IIS3(featuresets,text,tag._1.toList)
    println(iis.ll)

//    while(!unselected.isEmpty) {
//      //2
//      for (ts <- unselected) {
//        var template = new FeatureTemplate(ts)
//        var featuresets = template.createFeatureAndToken(text)
//        var template2 = new FeatureTemplate(ts)
//        var activefeature = List[Array[List[(List[(String,Char,Int)],String)]]]()
//        for(sentence <- text) {
//          var i = 0
//          while(i < sentence.length) {
//            for(featureset <- featuresets) {
//              var activ e2 = List[(List[(String,Char,Int)],String)]()
//              var f = new FeatureRun(featureset)
//              if (f.run(sentence,i)==1) {
//                active2 = featureset :: active2
//              }
//            }
//            i += 1
//          }
//        }
//      }
//    }
  }
}
