package CWS

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/26/12
 * Time: 12:34 AM
 */
object IIS2 {
  var label = List[String]()
  def train(train_toks:List[(List[(String,Char,Int)],String)]):Map[(String,Char,Int,String),Int] = {
    var mapping = Map[(String,Char,Int,String),Int]()// maps (fname, fval, label) -> fid
    var seen_labels_tag = List[String]()
    var count = Map[(String,Char,Int),Int]()   // maps (fname, fval) -> count
    for (x <- train_toks) {
      for(y <- x._1) {
        var tmp = count.get((y._1,y._2,y._3))
        if(tmp != None)
          count.updated((y._1,y._2,y._3),tmp.get + 1)

        else
          count.updated((y._1,y._2,y._3),tmp.get + 1)
      }
    }
    mapping
  }

  def encode(mapping:Map[(String,Char,Int,String),Int],featureset:List[(String,Char,Int)],label:String):List[(Int,Int)] = {
    var encoding = List[(Int,Int)]()
    for(x <- featureset) {
      if(mapping.contains (x._1, x._2, x._3, label))
        encoding= (mapping((x._1, x._2, x._3, label)), 1) :: encoding
    }
    encoding
  }
}
