package com.fastText

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import java.io._


sealed trait EntryType  {val value: Int}
case object Word extends EntryType {val value = 0}
case object Label extends EntryType {val value = 1}

case class Entry(word:String, var count:Long, typeE: EntryType, var subwords: Array[Int])

class Dictionary() {

  private val MAX_VOCAB_SIZE = 30000000
  private val MAX_LINE_SIZE = 1024
  
  private val EOS = "</s>"
  private val BOW = "<"
  private val EOW = ">"

  type IdType = Int
  var args: Args = _
  var size = 0
  var nwords = 0
  var nlabels = 0
  var ntokens = 0
  var word2Int = ArrayBuffer.fill[Int](MAX_VOCAB_SIZE)(-1)
  var words: ArrayBuffer[Entry] = _ 
  var pdiscard: ArrayBuffer[Double] = _

  def add(w: String) {
    val h = find(w)
    ntokens += 1
    if (word2int(h) == -1) {
      val word = w
      val count = 1
      val typeE = if (w.contains(args_->label)) Label else Word 
      words.append(Entry(word, count, typeE, Array[Int]()))
    } else {
      words(word2int(h)).count += 1
    }
  }

  def nwords = nwords
  def nlabels = nlabels
  def ntokens = ntokens
  
  def getNgrams(i: Int): Array[Int] = {
    require(i >= 0)
    require(i < nwords)
    words(i).subwords
  }

  def getNgrams(word: String): Array[Int] = {
    val i = getId(word)
    if (i >= 0) {
      getNgrams(i)
    } else {
      
      
    }
  }

  def getId(w: String) : Int = {
    val h = find(w)
    word2int(h)
  }

  def load(in: DataInputStream) {
    words.clear() 
    word2Int = ArrayBuffer.fill[Int](MAX_VOCAB_SIZE)(-1)
    size = in.readInt()
    nwords = in.readInt()
    nlabels = in.readInt()
    ntokens = in.readInt()
    for (index <- 0 until size)  {
      val word = in.readUTF()
      in.readInt()
      val count = in.readInt()
      val typeValue = in.readInt() 
      val typeE = if (typeValue == 0) Word else Label 
      words.append(Entry(word, count, typeE, Array[Int]()))
      word2Int(find(word)) = index
    } 
  }
  def save(out: DataOutputStream) {
      out.writeInt(size)
      out.writeInt(nwords)
      out.writeInt(nlabels)
      out.writeInt(ntokens)
      words.foreach(item => {
        val Entry(word, count, typeE, subwords) = item
        out.writeBytes(word)
        out.write(0) 
        out.writeLong(count)
        out.writeInt(typeE.value)
      })
  }
  

  private def find(w: String): Int = {
    var h = (hash(w)  % MAX_VOCAB_SIZE).toInt
    while(word2Int(h) != -1 && words(word2Int(h)).word != w) {
      h = (h + 1) % MAX_VOCAB_SIZE
    }
    h.toInt
  }
  private def hash(w: String) : Long = {
    var h = 2166136261L
    for (i <- 0 until w.length) {
      h = h ^ w(i).toInt
      h = h * 16777619
    }
    h
  }
  
}

object Dictionary {
  def main(args: Array[String]) : Unit  {
  }  

}
