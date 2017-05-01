import scala.io.source
import scala.collection.mutable.ArrayBuffer

import java.io._


type Int IdType
sealed trait EntryType  {val value: Int}
case object Word extends EntryType {val value = 0}
case object Label extends EntryType {val value = 1}

case class Entry(word:String, count:Long, type: EntryType, subwords: Array[Int])

class Dictionary() {
  var args: Args
  var size = 0
  var nwords = 0
  var nlabels = 0
  var ntokens = 0
  var word2Int = ArrayBuffer.fill[Int](MAX_VOCAB_SIZE)(-1)
  var words: ArrayBuffer[Entry]
  var pdiscard: ArrayBuffer[Double]

  def load(s: Source) {
    words.clean() 
    word2Int = ArrayBuffer.fill[Int](MAX_VOCAB_SIZE)(-1)

  }
  def save(out: FileOutputStream) {
      out.write(size)
      out.write(nwords)
      out.write(nlabels)
      out.write(ntokens)
      words.foreach( item => {
        val Entry(word, count, type, subwords) = item

      }
  }
  

  private def find(w: String): Int = {
    var h = hash(w)  % MAX_VOCAB_SIZE      
    while(word2Int(h) != -1 && words
  }
  private def hash(w: String) : Int = {
    var h = 2166136261;
    for (int i = 0; i < w.length; ++i) {
      h = h ^ w(i).toInt
      h = h * 16777619
    }
    h
  }
  
  


}


object Dictionary {
  private[this] val MAX_VOCAB_SIZE = 30000000
  private[this] val MAX_LINE_SIZE = 1024;
  
  val EOS = "</s>"
  val BOW = "<"
  val EOW = ">"

}
