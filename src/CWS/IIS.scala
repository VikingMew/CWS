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
    //unattested = set(numpy.nonzero(empirical_ffreq==0)[0])
    //  weights = numpy.zeros(len(empirical_ffreq), 'd')
    //for fid in unattested: weights[fid] = numpy.NINF
    //classifier = ConditionalExponentialClassifier(encoding, weights)
    //ll_old = None
    //acc_old = None
    //try:
    //while True:
    //if trace > 2:
    //  ll = cutoffchecker.ll or log_likelihood(classifier, train_toks)
    //acc = cutoffchecker.acc or accuracy(classifier, train_toks)
    //iternum = cutoffchecker.iter
    //print '     %9d    %14.5f    %9.3f' % (iternum, ll, acc)
    //
    //# Calculate the deltas for this iteration, using Newton's method.
    //deltas = calculate_deltas(
    //  train_toks, classifier, unattested, empirical_ffreq,
    //  nfmap, nfarray, nftranspose, encoding)
    //
    //# Use the deltas to update our weights.
    //  weights = classifier.weights()
    //weights += deltas
    //classifier.set_weights(weights)
    //
    //# Check the log-likelihood & accuracy cutoffs.
    //if cutoffchecker.check(classifier, train_toks):
    //  break
    //
    //except KeyboardInterrupt:
    //  print '      Training stopped: keyboard interrupt'
    //except:
    //  raise
    //
    //
    //if trace > 2:
    //  ll = log_likelihood(classifier, train_toks)
    //acc = accuracy(classifier, train_toks)
    //print '         Final    %14.5f    %9.3f' % (ll, acc)
    //
    //# Return the classifier.
    //return classifier
  }
}
object MaxEntEncoder {
  def train(train_toks:Point2,  labels=None) {
    var mapping = new mutable.HashMap[Tuple4[String,Char,Int,Tuple2[Char,String]],Int]// maps (fname, fval, label) -> fid
    var seen_labels = new Tuple2[mutable.HashSet[Char],mutable.HashSet[String]]       // The set of labels we've encountered
    var count = new mutable.HashMap[Tuple3[String,Char,Int],Int]   // maps (fname, fval) -> count
             count.
    for (tok, label) in train_toks:
      seen_labels.add(label)

    # Record each of the features.
    for (fname, fval) in tok.items():

    # If a count cutoff is given, then only add a joint
    # feature once the corresponding (fname, fval, label)
    # tuple exceeds that cutoff.
    count[fname,fval] += 1
    if count[fname,fval] >= count_cutoff:
    if (fname, fval, label) not in mapping:
      mapping[fname, fval, label] = len(mapping)

    if labels is None: labels = seen_labels
    new MaxEntEncoder(labels, mapping)
  }
}
class MaxEntEncoder(clabels, cmapping){

  var labels = clabels

  ///var mapping = mapping
  """dict mapping from (fname,fval,label) -> fid"""

  var length = len(mapping)
  def encode(featureset,label){
    encoding = []

    for fname, fval in featureset.items():
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
