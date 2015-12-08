package me.yuhuan.parchment

import edu.stanford.nlp.ling._
import edu.stanford.nlp.parser.{nndep => s}
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import me.yuhuan.parchment.DependencyTree._
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import me.yuhuan.parchment.JavaConversions._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object NeuralNetworkDependencyParser {
  val tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")
  val parser = s.DependencyParser.loadFromModelFile("edu/stanford/nlp/models/parser/nndep/english_UD.gz")

  def parseHasWords(hasWords: Seq[HasWord]): DependencyTree = {
    val javaListOfHasWords: java.util.List[HasWord] = hasWords.asJava
    val taggedWords = tagger.tagSentence(javaListOfHasWords)
    val parse = parser.predict(taggedWords)
    new DependencyTree(hasWords.map(_.word()), parse.typedDependenciesCollapsed().map(td => DependencyRelation(td.gov.index, td.dep.index, td.reln)).toSeq)
  }

  def parse(tokens: Seq[String]): DependencyTree = {
     parseHasWords(StanfordSentence(tokens))
  }

  def parse(sentence: String): DependencyTree = {
    parseHasWords(Tokenizer.tokenizeSentence(sentence))
  }
}

private object NeuralNetworkDependencyParserTest extends App {
  val sentence = "My dog also likes eating sausage ."
  val tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")
  val parser = s.DependencyParser.loadFromModelFile("edu/stanford/nlp/models/parser/nndep/english_UD.gz")

  val tokens = sentence.split(' ')



  val bp = 0
}
