package CWS

import collection.mutable
import collection.mutable.ArrayBuffer

class IIS {
  def train_with_iis(ctokens:Array[Point2],cencoding:Any=null//, labels=mutable.HashSet[String]
                      ){
    var encoding = cencoding
    if(encoding == null) {
      encoding = MaxEntEncoder.train(train_toks, labels=labels)
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
  def train(train_toks:Array[Point2], labels:List[Char]) {
    var mapping = new mutable.HashMap[Tuple4[String,Char,Int,Tuple2[Char,String]],Int]// maps (fname, fval, label) -> fid
    //var seen_labels_pos = List[String]
    //var seen_labels_pos_tag = List[Tuple2[Char,String)]]
    var seen_labels_tag = List[Char]
    var count = new mutable.HashMap[Tuple3[String,Char,Int],Int]   // maps (fname, fval) -> count
    for (x <- train_toks) {
      if
    }

    // Record each of the features.
    //for (fname, fval) in tok.items():

//    # If a count cutoff is given, then only add a joint
//    # feature once the corresponding (fname, fval, label)
//    # tuple exceeds that cutoff.
    count[fname,fval] += 1
    if count[fname,fval] >= count_cutoff:
    if (fname, fval, label) not in mapping:
      mapping[fname, fval, label] = len(mapping)

    if labels is None: labels = seen_labels
    new MaxEntEncoder(labels, mapping)
  }
}
class MaxEntEncoder(clabels:List[Char], cmapping:mutable.HashMap[Tuple4[String,Char,Int,String],Int]){

  var labels = clabels
  var mapping = cmappings
  var length = len(mapping)

  def encode(featureset:List[Tuple3[String,Char,Int]],label:String){
    var encoding = List[Tuple2[Int,Int]]()
    for(x <- featureset) {
      if(mapping contains (x._1, x._2, x._3, label)) {
        encoding.append((mapping(x._1, x._2, x._3, label), 1))
      }
    }
    return encoding
  }

}

class MaxEntClassifier(cencoding:List[Tuple2[Int,Int]], cweights:Array[Double]){
  var weights = cweights
  var encoding = cencoding
  def classify(featureset:List[Tuple3[String,Char,Int]]) {
    this.prob_classify(featureset).reduce((x,y)=>if(x._2 > y._2) x else y)
  }
  def prob_classify(featureset:List[Tuple3[String,Char,Int]]):mutable.HashMap[Char,Double]= {
    var prob_dict = new mutable.HashMap[Char,Double]()
    prob_dict
  }
}

