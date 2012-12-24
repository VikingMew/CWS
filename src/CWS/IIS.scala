package CWS
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

class MaxEntEncoder(labels, mapping){

  var labels = labels)
  """A list of attested labels."""

  self._mapping = mapping
  """dict mapping from (fname,fval,label) -> fid"""

  self._length = len(mapping)
  """The length of generated joint feature vectors."""

  self._alwayson = None
  """dict mapping from label -> fid"""

  self._unseen = None
  """dict mapping from fname -> fid"""

  if alwayson_features:
    self._alwayson = dict([(label,i+self._length)
  for (i,label) in enumerate(labels)])
  self._length += len(self._alwayson)

  if unseen_features:
    fnames = set(fname for (fname, fval, label) in mapping)
  self._unseen = dict([(fname, i+self._length)
  for (i, fname) in enumerate(fnames)])
  self._length += len(fnames)
}
