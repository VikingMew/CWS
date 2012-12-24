package CWS

import collection.mutable
import collection.mutable.ArrayBuffer
import scala.Math

class IIS {
  def train_with_iis(ctokens:List[Tuple4[String,Char,Int,String]],cencoding:MaxEntEncoder=null, labels:List[String]
                      ){
    var encoding = cencoding
    if(encoding == null) {
      encoding = MaxEntEncoder.train(ctokens, labels=labels)
    }
    var empirical_ffreg //=  (calculate_empirical_fcount(train_toks, encoding) / len(train_toks))
    var nfmap //= calculate_nfmap(train_toks, encoding)
    var nfarray //= numpy.array(sorted(nfmap, key=nfmap.__getitem__), 'd')
    var nftranspose //= numpy.reshape(nfarray, (len(nfarray), 1))
    var unattested // = set(numpy.nonzero(empirical_ffreq==0)[0])
    var weights=Array.fill(0/*len(empirical_ffreq)*/){0.0}

    //for fid in unattested: weights[fid] = numpy.NINF
    var classifier = new MaxEntClassifier(encoding, weights)
    var ll_old = null
    var acc_old = null
    var i = 0
    while(i < 100) {
      ll = log_likelihood(classifier, train_toks)
      acc = accuracy(classifier, train_toks)
      print("     %9d    %14.5f    %9.3f".format(l, ll, acc))
      deltas = calculate_deltas(
        train_toks, classifier, unattested, empirical_ffreq,
        nfmap, nfarray, nftranspose, encoding)

      //# Use the deltas to update our weights.
      weights = classifier.weights()
      weights += deltas
      classifier.weights = weights
      i+=1
    }
    //if trace > 2:
    //  ll = log_likelihood(classifier, train_toks)
    //acc = accuracy(classifier, train_toks)
    //print '         Final    %14.5f    %9.3f' % (ll, acc)
    //# Return the classifier.
    //return classifier
  }
}
//train_toks:List[Tuple2[mutable.HashSet[Tuple3[String,Char,Int]],Int]]
object MaxEntEncoder {
  def train(train_toks:List[Tuple4[String,Char,Int,String]], labels:List[String]) {
    var mapping = new mutable.HashMap[Tuple4[String,Char,Int,String],Int]// maps (fname, fval, label) -> fid
    //var seen_labels_pos = List[String]
    //var seen_labels_pos_tag = List[Tuple2[Char,String)]]
    var seen_labels_tag = List[String]
    var count = new mutable.HashMap[Tuple3[String,Char,Int],Int]   // maps (fname, fval) -> count
    for (x <- train_toks) {
      var tmp = count.get((x._1,x._2,x._3))
      if(tmp != None) {
        count.update((x._1,x._2,x._3),tmp.get + 1)
      } else {
        count.put((x._1,x._2,x._3),1)
      }
    }
    new MaxEntEncoder(labels, mapping)
  }
}
class MaxEntEncoder(clabels:List[String], cmapping:mutable.HashMap[Tuple4[String,Char,Int,String],Int]){

  var labels = clabels
  var mapping = cmapping
  var length = mapping.size

  def encode(featureset:List[Tuple3[String,Char,Int]],label:String):List[Tuple2[Int,Int]] = {
    var encoding = List[Tuple2[Int,Int]]()
    for(x <- featureset) {
      if(mapping.contains (x._1, x._2, x._3, label))
       encoding= (mapping((x._1, x._2, x._3, label)), 1) :: encoding
    }
    encoding
  }

}

class MaxEntClassifier(cencoding:MaxEntEncoder, cweights:Array[Double]){
  var weights = cweights
  var encoding = cencoding
  def classify(featureset:List[Tuple3[String,Char,Int]]) {
    this.prob_classify(featureset).reduce((x,y)=>if(x._2 > y._2) x else y)
  }
  def prob_classify(featureset:List[Tuple3[String,Char,Int]]):mutable.HashMap[String,Double]= {
    var prob_dict = new mutable.HashMap[String,Double]()
    for(label <- encoding.labels) {
      var feature_vector = encoding.encode(featureset, label)
      var prod:Double = 1.0
      for ((fid,fval) <- feature_vector) {
        prod *= math.pow(weights(fid),fval)
      }
      prob_dict(label)=prod
    }
    prob_dict
  }
}

