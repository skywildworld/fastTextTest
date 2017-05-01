package com.fastText

import scala.collection.mutable

sealed trait ModelName {val modelName: String}
case object Cbow extends ModelName {val modelName = "cbow"} // what does this mean
case object Sg extends ModelName {val modelName = "sg"}
case object Sup extends ModelName {val modelName = "sup"}

sealed trait LossFunc {val loss: String}
case object Hs extends LossFunc {val loss = "hs"}
case object Ns extends LossFunc {val loss = "ns"}
case object Softmax extends LossFunc {val loss = "softmax"}

class GeneralArgParser(args: Array[String]) {
  val argsMap = new mutable.HashMap[String, String]() 

  for (arg <- args) {
    val arr = arg.split("=")
    if (arr.size > 1) {
      argsMap.put(arr(0).trim, arr(1).trim)
    }
  }
  
  def getStringValue(argName: String, defaultVal: String=""): String = {
    argsMap.getOrElse(argName, defaultVal) 
  }
  def getIntValue(argName: String, defaultVal: Int=0): Int = {
    val value = argsMap.get(argName)
    value match {
      case Some(v) => v.toInt
      case None => defaultVal
    }
  }
  def getDoubleValue(argName: String, defaultVal: Double=0.0): Double = {
    val value = argsMap.get(argName)
    if (value.isEmpty) 
      defaultVal
    else value.get.toDouble
  }
  def getCommaSplitArrayValue(argName: String, defaultVal: Array[String]=Array[String]()): 
    Array[String] = {
    val value = argsMap.get(argName)
    if (value.isEmpty) 
      defaultVal
    else value.get.split(",")
  }
}


case class Args(
  var input: String,
  var test: String = "",
  var output: String,
  var lr: Double = 0.05,
  var lrUpdateRate: Int = 100,
  var dim: Int = 100,
  var ws: Int = 5,
  var epoch: Int = 5,
  var minCount: Int = 5,
  var minCountLabel: Int = 0,
  var neg: Int = 5, 
  var wordNgrams: Int = 1,
  var loss: Option[LossFunc] = Some(Ns),
  var model: Option[ModelName] = Some(Sg),
  var bucket: Int = 2000000,
  var minn: Int = 3,
  var maxn: Int = 6,
  var thread: Int = 12, // maybe not need any more
  var t: Double = 1e-4,
  var label: String = "__label__",
  var verbose: Int = 2,
  var pretrainedVectors: String = "",
  var saveOutput: Int = 0
) {
  def parseArgs(args: Array[String]) : Unit = {
    val cmdArgs = new GeneralArgParser(args)
    val modelName = cmdArgs.getStringValue("model", "sg").toLowerCase
    if (modelName == "sup") {
      model = Some(Sup)
      loss = Some(Softmax)
      minCount = 1
      minn = 0
      maxn = 0
      lr = 0.1
    } else if(modelName == "cbow") {
      model = Some(Cbow)
    } else if(modelName == "sg") {
      model = Some(Sg)
    } else None
    input = cmdArgs.getStringValue("input")  // default is ""
    test = cmdArgs.getStringValue("test") 
    output = cmdArgs.getStringValue("output")  
    lr = cmdArgs.getDoubleValue("lr", lr) // design problem: If not exists, then Should not change its default value
    lrUpdateRate = cmdArgs.getIntValue("lrUpdateRate", lrUpdateRate) 
    dim = cmdArgs.getIntValue("dim", dim) 
    ws = cmdArgs.getIntValue("ws", ws) 
    epoch = cmdArgs.getIntValue("epoch", epoch) 
    minCount = cmdArgs.getIntValue("minCount", minCount) 
    minCountLabel = cmdArgs.getIntValue("minCountLabel", minCountLabel) 
    neg = cmdArgs.getIntValue("neg", neg) 
    wordNgrams = cmdArgs.getIntValue("wordNgrams", wordNgrams) 
    val lossName = cmdArgs.getStringValue("loss").toLowerCase
    loss = lossName match {
      case "hs" =>  Some(Hs)
      case "ns" => Some(Ns)
      case "softmax" => Some(Softmax)
      case _ => {
        println(s"Unknow loss: $lossName")
        printHelp()
        None
      }
    }
    
    bucket = cmdArgs.getIntValue("bucket", bucket) 
    minn = cmdArgs.getIntValue("minn", minn) 
    maxn = cmdArgs.getIntValue("maxn", maxn) 
    thread = cmdArgs.getIntValue("thread", thread) 
    t = cmdArgs.getDoubleValue("t", t) 
    label = cmdArgs.getStringValue("label", label) 
    verbose = cmdArgs.getIntValue("verbose", verbose) 
    pretrainedVectors = cmdArgs.getStringValue("pretrainedVectors", pretrainedVectors) 
    saveOutput = cmdArgs.getIntValue("saveOutput", saveOutput) 
    if (input == "" || output == "" || loss.isEmpty || model.isEmpty) {
      println("Empty input or output path.")
      printHelp() 
      System.exit(1)
    }
  }

  def printHelp() {
    val lname = loss match {
      case Some(Hs) => "hs"
      case Some(Softmax) => "softmax"
      case Some(Ns) => "ns"
      case _ => ""
    }
    val outline = s"""
      |
      |The following arguments are mandatory:
      |  input              training file path
      |  output             output file path
      |The following arguments are optional:
      |  lr                 learning rate $lr 
      |  lrUpdateRate       change the rate of updates for the learning rate $lrUpdateRate
      |  dim                size of word vectors $dim
      |  ws                 size of the context window $ws
      |  epoch              number of epochs $epoch 
      |  minCount           minimal number of word occurences $minCount
      |  minCountLabel      minimal number of label occurences $minCountLabel
      |  neg                number of negatives sampled $neg
      |  wordNgrams         max length of word ngram $wordNgrams
      |  loss               loss function {ns, hs, softmax} [ns]
      |  bucket             number of buckets bucket
      |  minn               min length of char ngram $minn
      |  maxn               max length of char ngram $maxn
      |  thread             number of threads $thread
      |  t                  sampling threshold $t
      |  label              labels prefix $label
      |  verbose            verbosity level $verbose
      |  pretrainedVectors  pretrained word vectors for supervised learning []"
      |  saveOutput         whether output params should be saved $saveOutput
      """.stripMargin
      println(outline)
  }
    
}


