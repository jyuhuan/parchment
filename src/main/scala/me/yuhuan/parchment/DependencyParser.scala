package me.yuhuan.parchment

import edu.stanford.nlp.ling._
import edu.stanford.nlp.parser.{nndep => s}
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import me.yuhuan.parchment.JavaConversions._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object DependencyParser {
  val tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")
  val parser = s.DependencyParser.loadFromModelFile(s.DependencyParser.DEFAULT_MODEL)

  def parseHasWords(hasWords: Seq[HasWord]): DependencyTree = {
    val javaListOfHasWords: java.util.List[HasWord] = hasWords.asJava
    val taggedWords = tagger.tagSentence(javaListOfHasWords)
    val parse = parser.predict(taggedWords)
    parse.typedDependencies.map(d => d.gov() -> d.dep())
    new DependencyTree(hasWords.map(_.word()), parse.typedDependencies.toSeq)
  }

  def parse(tokens: Seq[String]): DependencyTree = {
     parseHasWords(StanfordSentence(tokens))
  }

  def parse(sentence: String): DependencyTree = {
    parseHasWords(Tokenizer.tokenizeSentence(sentence))
  }
}
