package CWS

import collection.mutable

class IIS {
  def train_with_iis(ctokens:Array[Point2],cencoding:Any=null//, labels=None,
                      ){
    var encoding = cencoding
    if(encoding == null) {
      //encoding = BinaryMaxentFeatureEncoding.train(train_toks, labels=labels)
    }
    var empirical_ffreg //=  (calculate_empirical_fcount(train_toks, encoding) / len(train_toks))
    var nfmap //= calculate_nfmap(train_toks, encoding)
    var nfarray //= numpy.array(sorted(nfmap, key=nfmap.__getitem__), 'd')
    var nftranspose //= numpy.reshape(nfarray, (len(nfarray), 1))
    var unattested // = set(numpy.nonzero(empirical_ffreq==0)[0])
    var weights=Array.fill(/*len(empirical_ffreq)*/){0.0}

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
//object MaxEntEncoder {
//  def train(train_toks:Array[Point2],  labels:Tuple2[List[Char],List[String]]) {
//    var mapping = new mutable.HashMap[Tuple4[String,Char,Int,Tuple2[Char,String]],Int]// maps (fname, fval, label) -> fid
//    //var seen_labels_pos = List[String]
//    //var seen_labels_pos_tag = List[Tuple2[Char,String)]]
//    //var seen_labels_tag = List[Char]
//    var count = new mutable.HashMap[Tuple3[String,Char,Int],Int]   // maps (fname, fval) -> count
//    for (x <- train_toks) {
//      if
//    }
//
//    // Record each of the features.
//    //for (fname, fval) in tok.items():
//
////    # If a count cutoff is given, then only add a joint
////    # feature once the corresponding (fname, fval, label)
////    # tuple exceeds that cutoff.
//    count[fname,fval] += 1
//    if count[fname,fval] >= count_cutoff:
//    if (fname, fval, label) not in mapping:
//      mapping[fname, fval, label] = len(mapping)
//
//    if labels is None: labels = seen_labels
//    new MaxEntEncoder(labels, mapping)
//  }
//}
class MaxEntEncoder(clabels, cmapping){

  var labels = clabels

  ///var mapping = mapping
  """dict mapping from (fname,fval,label) -> fid"""

  var length = len(mapping)
  def encode(featureset:List[Tuple3[String,Char,Int]],label:String){
    var encoding = List[Tuple2[Int,Int]]()
    for(x <- featureset)
    # Known feature name & value:
    if (fname, fval, label) in self._mapping:
      encoding.append((self._mapping[fname, fval, label], 1))

    # Otherwise, we might want to fire an "unseen-value feature".
      elif self._unseen:
    # Have we seen this fname/fval combination with any label?
    for label2 in self._labels:
    if (fname, fval, label2) in self._mapping:
      break # we've seen this fname/fval combo
    # We haven't -- fire the unseen-value feature
    else:
    if fname in self._unseen:
      encoding.append((self._unseen[fname], 1))

    # Add always-on features:
    if self._alwayson and label in self._alwayson:
      encoding.append((self._alwayson[label], 1))

    return encoding
  }

}

class MaxEntClassifier(cencoding, cweights:Array[Double]){
  var weights = cweights
  var encoding = cencoding
  def classify(featureset) {
    self.prob_classify(featureset).max()
  }
}

