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
object DependencyParser {

  val parser = ConstituentParser.parser.treebankLanguagePack().grammaticalStructureFactory()


  def parseFromConstituentTree(constituentTree: ConstituentTree): DependencyTree = {
    new DependencyTree(
      constituentTree.tokens.map(_.label),
      parser.newGrammaticalStructure(constituentTree.tree)
            .typedDependenciesCCprocessed()
            .map(td => DependencyRelation(td.gov.index, td.dep.index, td.reln))
    )
  }

  def parse(tokens: Seq[String]): DependencyTree = parseHasWords(StanfordSentence(tokens))
  def parse(sentence: String): DependencyTree = parseCoreLabels(Tokenizer.tokenizeSentence(sentence))

  def parseCoreLabels(coreLabels: Seq[CoreLabel]): DependencyTree = {
    parseFromConstituentTree(ConstituentParser.parseCoreLabels(coreLabels))
  }

  def parseHasWords(hasWords: Seq[HasWord]): DependencyTree = {
    parseFromConstituentTree(ConstituentParser.parseHasWords(hasWords))
  }

}

object DependencyParserTest extends App {
  val lexParse = ConstituentParser.parse("My dog also likes eating sausage.")

  val depParse = DependencyParser.parse("My dog also likes eating sausage.")

  val bp = 0
}
