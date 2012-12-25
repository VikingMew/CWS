package CWS

import collection.mutable
import collection.mutable.ArrayBuffer
import scala.Math
import scalala.operators.Implicits._

class IIS {
  def calculate_empirical_fcount(train_toks:List[(List[(String,Char,Int)],String)], encoding:MaxEntEncoder):Array[Double]= {
    var fcount = Array.fill(encoding.length){0.0}
    for ((tok, label) <- train_toks)
      for((index,value) <-encoding.encode(tok, label))
        fcount(index) += 1
    fcount
  }
  def calculate_nfmap(train_toks:List[(List[(String,Char,Int)],String)], encoding:MaxEntEncoder):mutable.Map[Int,Int] = {
    var nfset = mutable.Map[Int,Int]()
    for ((tok,_) <- train_toks)
      for (label <- encoding.labels) {
        nfset ++= encoding.encode(tok,label).groupBy(_._2).map(x=>(x._1 ->x._2.length))
      }
    nfset.map(x=>x.swap)
  }
  def log_likelihood(classifier:MaxEntClassifier, gold:List[(List[(String,Char,Int)],String)]) {
    var results = classifier.batch_prob_classify(gold.map((x=>x._1)))
    var ll =List[Double]()
    for (((fs,l), pdist)  <- (gold zip results)) {
      if (pdist.contains(l))
        ll = math.pow(2,(pdist(l))) :: ll
      else
        ll = 0 :: ll
    }
    math.log(ll.reduce(_+_)/ll.length)
  }
  def calculate_deltas(train_toks:List[(List[(String,Char,Int)],String)],
                       classifier:MaxEntClassifier,
                       ffreq_empirical:Array[Double],
                       nfmap:mutable.Map[Int,Int],
                       nfarray:List[Double],
                       nftranspose:List[List[Double]]
                       , encoding:MaxEntEncoder) {
    val NEWTON_CONVERGE = 1e-12
    val MAX_NEWTON = 300
    var deltas = Array.fill(encoding.length){1.0}
    var A = Array.fill(nfmap.length, encoding.length()){0.0}
    for ((tok,label) <- train_toks) {
      var dist = classifier.prob_classify(tok)
      for(label <- encoding.labels) {
        var feature_vector = encoding.encode(tok,label)
        nf = feature_vector.map(x=>x._2).reduce(_+_)
        for ((id,value) <- feature_vector) {
          A(nfmap(nf), id) += dist.prob(label) * value
        }
      }
    }
    A = A.map(x=>x.map(y=>y/train_toks.length))
    val a_nftranspose = nftranspose.map(x=>x.toArray).toArray
    for (rangenum <- (0 until MAX_NEWTON)) {
      val nf_delta =nfarray.map(x=>deltas.map(y => y * x)).toArray
      val exp_nf_delta = nf_delta.map(x=>x.map(y=>math.pow(2,y)))
      //val nf_exp_nf_delta = a_nftranspose * exp_nf_delta
      //val sum1 = numpy.sum(exp_nf_delta * A, axis=0)
      //val sum2 = numpy.sum(nf_exp_nf_delta * A, axis=0)
      //deltas -= (ffreq_empirical - sum1) / -sum2
    }
    deltas
  }

//  for rangenum in range(MAX_NEWTON):
//    nf_delta = numpy.outer(nfarray, deltas)
//  exp_nf_delta = 2 ** nf_delta
//  nf_exp_nf_delta = nftranspose * exp_nf_delta
//  sum1 = numpy.sum(exp_nf_delta * A, axis=0)
//  sum2 = numpy.sum(nf_exp_nf_delta * A, axis=0)
//
//  # Update the deltas.
//    deltas -= (ffreq_empirical - sum1) / -sum2
//
//  # We can stop once we converge.
//  n_error = (numpy.sum(abs((ffreq_empirical-sum1)))/
//    numpy.sum(abs(deltas)))
//  if n_error < NEWTON_CONVERGE:
//  return deltas
//
//  return deltas
  def train_with_iis(ctokens:List[(List[(String,Char,Int)],String)],
                     labels:List[String]){
    var encoding = MaxEntEncoder.train(ctokens,labels)
    var empirical_ffreq =  calculate_empirical_fcount(ctokens, encoding).map(x => x/ctokens.length)
    var nfmap = calculate_nfmap(ctokens,encoding)
    var nfarray = (nfmap.toList.map(x => x._1.toDouble)).sortBy(_.toDouble)
    var nftranspose = nfarray.map(x=>List(x))
//    var unattested = set(numpy.nonzero(empirical_ffreq==0)[0])
    var weights=Array.fill(empirical_ffreq.length){0.0}

    //for fid in unattested: weights[fid] = numpy.NINF
    var classifier = new MaxEntClassifier(encoding, weights)
    var ll_old = null
    var acc_old = null
    var i = 0
    while(i < 100) {
      var ll = log_likelihood(classifier, train_toks)
      //var acc = accuracy(classifier, train_toks)
      //print("     %9d    %14.5f    %9.3f".format(l, ll, acc))
      prinf("     %9d    %14.5f\n".format(l, ll))
      deltas = calculate_deltas(
        ctokens, classifier, unattested, empirical_ffreq,
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
  def train(train_toks:List[(List[(String,Char,Int)],String)], labels:List[String]):MaxEntEncoder= {
    var mapping = new mutable.HashMap[(String,Char,Int,String),Int]// maps (fname, fval, label) -> fid
    //var seen_labels_pos = List[String]
    //var seen_labels_pos_tag = List[Tuple2[Char,String)]]
    var seen_labels_tag = List[String]
    var count = new mutable.HashMap[(String,Char,Int),Int]   // maps (fname, fval) -> count
    for (x <- train_toks) {
      for(y <- x._1) {
      var tmp = count.get((y._1,y._2,y._3))
      if(tmp != None)
        count.update((y._1,y._2,y._3),tmp.get + 1)
       else
        count.put((y._1,y._2,y._3),1)
      }
    }
    var t = new MaxEntEncoder(labels, mapping)
    return t
  }
}
class MaxEntEncoder(clabels:List[String], cmapping:mutable.HashMap[(String,Char,Int,String),Int]){

  var labels = clabels
  var mapping = cmapping
  var length = mapping.size

  def encode(featureset:List[(String,Char,Int)],label:String):List[(Int,Int)] = {
    var encoding = List[(Int,Int)]()
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
  def prob_classify(featureset:List[(String,Char,Int)]):mutable.HashMap[String,Double]= {
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
  def batch_prob_classify( featuresets:List[List[(String,Char,Int)]]):List[mutable.HashMap[String,Double]] = {
    featuresets.map(fs => this.classify(fs))
  }
}

