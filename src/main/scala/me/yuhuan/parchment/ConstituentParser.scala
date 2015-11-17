package me.yuhuan.parchment

import java.io.StringReader

import edu.stanford.nlp.ling._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.process._

import scala.collection.JavaConversions._
import me.yuhuan.parchment.JavaConversions.StanfordSentence

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object ConstituentParser {
  private[parchment] val lexParser =
    LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  def parse(tokens: Seq[String]): ConstituentTree = parseHasWords(StanfordSentence(tokens))
  def parse(sentence: String): ConstituentTree = parseCoreLabels(Tokenizer.tokenizeSentence(sentence))

  def parseCoreLabels(coreLabels: Seq[CoreLabel]): ConstituentTree = {
    ConstituentTree(lexParser.parse(coreLabels))
  }

  def parseHasWords(hasWords: Seq[HasWord]): ConstituentTree = ConstituentTree(lexParser.parse(hasWords))
}
