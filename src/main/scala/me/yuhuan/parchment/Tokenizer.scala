package me.yuhuan.parchment

import java.io._

import edu.stanford.nlp.ling._
import edu.stanford.nlp.process._
import scala.collection.JavaConversions._

/**
 * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 11/17/15.
 */
object Tokenizer {
  def tokenizeSentence(sentence: String) = new PTBTokenizer[CoreLabel](
    new StringReader(sentence),
    new CoreLabelTokenFactory(),
    "invertible"
  ).toList

  def tokenizeDocument(rawDoc: String): Iterator[Iterator[String]] = {
    tokenizeDocument(new StringReader(rawDoc))
  }

  def tokenizeDocument(reader: Reader): Iterator[Iterator[String]] = {
    val p = new DocumentPreprocessor(reader)
    p.iterator().map(words ⇒ words.iterator().map(_.toString))
  }
}
