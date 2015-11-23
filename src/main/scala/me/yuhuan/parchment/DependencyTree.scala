package me.yuhuan.parchment

import edu.stanford.nlp.international._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.trees._
import me.yuhuan.parchment.DependencyTree._
import me.yuhuan.reparo.Graph
import me.yuhuan.reparo.mut.AdjacencyMapGraph
import me.yuhuan.marauder._

/**
 * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 11/17/15.
 */
class DependencyTree(val tokens: Seq[String], dependencies: Seq[DependencyRelation]) {
  private val edges: Seq[(Int, Int, GrammaticalRelation)] = dependencies.map { case DependencyRelation(f, t, r) => {
    (f, t, r)
  }}

  private val nodes: Seq[(Int, String)] = ("ROOT" +: tokens).zipWithIndex.map{case (t, i) => i -> t}

  private val g: AdjacencyMapGraph[Int, String, GrammaticalRelation] = AdjacencyMapGraph(nodes: _*)(edges: _*)


  def relations: Seq[DependencyRelation] = edges.map { case (i, j, gr) =>
    DependencyRelation(i, j, gr)
  }

  def pathBetween(tokenIdxI: Int, tokenIdxJ: Int): Path[Int, GrammaticalRelation] = {
    g.pathBetween(tokenIdxI, tokenIdxJ)
  }

  /**
   *
   * @param tokenIdx Notice that ROOT has index 0. Thus, the index of the non-ROOT words in the
   *                 sentence begin at 1.
   * @param relation
   * @return
   */
  def outgoingTokenOf(tokenIdx: Int, relation: GrammaticalRelation): Option[Int] = {
    g.outgoingEdgesOf(tokenIdx).find(_.data == relation).map(_.id2)
  }

  def outgoingTokenOf(tokenIdx: Int, relationStr: String): Option[Int] = {
    outgoingTokenOf(tokenIdx, GrammaticalRelation.valueOf(Language.UniversalEnglish, relationStr))
  }


  private val edgesReversed = edges.map{case (gov, dep, rel) => (dep, gov, rel)}
  private val undirectedG: AdjacencyMapGraph[Int, String, GrammaticalRelation] =
    AdjacencyMapGraph(nodes: _*)(edgesReversed ++ edges: _*)

  def undirectedPathBetween(tokenIdxI: Int, tokenIdxJ: Int): Path[Int, GrammaticalRelation] = {
    val StateSpace = new StateSpaceWithLazyAction[Int, GrammaticalRelation] {
      def succ(s: Int, a: GrammaticalRelation): Seq[Int] = undirectedG.outgoingEdgesOf(s).find(_.data == a).map(_.vj.id).toSeq
      def succAction(s: Int): Seq[GrammaticalRelation] = undirectedG.outgoingEdgesOf(s).map(_.data).toSeq
    }
    GraphSearch.depthFirstWithLazyAction(tokenIdxI, (idx: Int) => idx == tokenIdxJ)(StateSpace)
  }

}

object DependencyTree {

  case class DependencyRelation(from: Int, to: Int, relation: GrammaticalRelation)

  def parseRelation(s: String): DependencyRelation = {
    val parts = s.split("""\|\|\|""")
    val i = parts(0).toInt
    val j = parts(1).toInt
    val r = GrammaticalRelation.valueOf(Language.UniversalEnglish, parts(2))
    DependencyRelation(i, j, r)
  }

  def ofString(s: String, tokens: Seq[String]): DependencyTree = {
    val rels = s.split('\n').map(parseRelation)
    new DependencyTree(tokens, rels)
  }
}