package me.yuhuan.parchment

import edu.stanford.nlp.ling.HasIndex
import edu.stanford.nlp.trees._
import me.yuhuan.marauder._
import scala.collection.JavaConversions._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class ConstituentTree private[parchment](val root: Tree, val tree: Tree) { t =>
  def label: String = t.tree.label().value()

  def parent: Option[ConstituentTree] = {
    val p = t.tree.parent(t.root)
    if (p != null) Some(new ConstituentTree(t.root, p)) else None
  }

  def children: Seq[ConstituentTree] = t.tree.children().map(c => new ConstituentTree(t.root, c))

  def isToken: Boolean = t.tree.isLeaf
  def isNonterminal = !isToken
  def hasParent: Boolean = parent.nonEmpty
  def hasNoParent = !hasParent

  def tokens: Seq[ConstituentTree] = t.tree.getLeaves[Tree].map(l => new ConstituentTree(t.root, l))
  def tokenLabels = tokens.map(_.label)

  def headToken: Option[ConstituentTree] = tokens.headOption
  def lastToken: Option[ConstituentTree] = tokens.lastOption

  def index: Int = t.tree.label().asInstanceOf[HasIndex].index()
  def tokenIndices: Seq[Int] = t.tokens.map(_.index)

  def tokenRange: Range = {
    val li = tokenIndices
    Range(li.head, li.last + 1)
  }

  def indexInSiblings: Option[Int] = t.parent.map(_.children.indexOf(t))

  def siblings: Seq[ConstituentTree] = t.parent.toSeq.flatMap(_.children).filterNot(_ == t)

  def leftSiblings: Seq[ConstituentTree] = indexInSiblings match {
    case Some(i) => siblings.slice(0, i)
    case None => Nil
  }

  def rightSiblings: Seq[ConstituentTree] = indexInSiblings match {
    case Some(i) => siblings.slice(i, siblings.length)
    case None => Nil
  }

  def leftSibling: Option[ConstituentTree] = leftSiblings.lastOption
  def rightSibling: Option[ConstituentTree] = rightSiblings.headOption

  def tokenBefore: Option[ConstituentTree] = leftSibling.flatMap(_.lastToken)
  def tokenAfter: Option[ConstituentTree] = rightSibling.flatMap(_.headToken)

  def rightMost(p: ConstituentTree => Boolean): Option[ConstituentTree] = {
    def RightMostStateSpace: StateSpace[ConstituentTree] = new StateSpace[ConstituentTree] {
      def succ(s: ConstituentTree): Seq[ConstituentTree] = s.children
    }
    Iterable.depthFirst(t)(RightMostStateSpace).takeWhile(p).lastOption
  }


  def rightAdjacentNodes: Iterable[ConstituentTree] = new Iterable[ConstituentTree] {
    def iterator: Iterator[ConstituentTree] = new Iterator[ConstituentTree] {
      var cur = t.tokenAfter
      def hasNext: Boolean = cur match {
        case Some(c) => !t.parent.contains(c)
        case None => false
      }
      def next(): ConstituentTree = cur match {
        case Some(l) => {
          cur = l.parent
          l
        }
      }
    }
  }

  def ancestors: Iterable[ConstituentTree] = new Iterable[ConstituentTree] {
    def iterator: Iterator[ConstituentTree] = new Iterator[ConstituentTree] {
      var cur: ConstituentTree = _
      var first: Boolean = true
      def hasNext: Boolean = {
        if (first) true
        else cur.hasParent
      }

      def next(): ConstituentTree = {
        if (first) {
          cur = t
          first = false
          cur
        }
        else {
          val Some(p) = cur.parent
          cur = p
          cur
        }
      }
    }
  }

  def lowestCommonAncestor(that: ConstituentTree): ConstituentTree = {
    val thisAncestors = t.ancestors
    val thatAncestors = that.ancestors
    if (thisAncestors.isEmpty) t
    else if (thatAncestors.isEmpty) that
    else (thisAncestors.toSeq intersect thatAncestors.toSeq).head
  }

  def subsumes(n: ConstituentTree): Boolean = t.tokens.toSet.contains(n)


  def syntacticHead(implicit hf: HeadFinder): Option[ConstituentTree] = {
    val h = t.tree.headTerminal(hf)
    if (h != null) Some(new ConstituentTree(t.root, h)) else None
  }

  def semanticHead(implicit shf: SemanticHeadFinder): Option[ConstituentTree] = {
    val sh = t.tree.headTerminal(shf)
    if (sh != null) Some(new ConstituentTree(t.root, sh)) else None
  }

  def nonterminalThatSubsumes(nodes: Seq[ConstituentTree]): ConstituentTree = nodes.reduce(_ lowestCommonAncestor _)

  def nonterminalThatSubsumesTokens(tokenIndices: Seq[Int]): ConstituentTree = {
    require(tokenIndices.nonEmpty)
    if (tokenIndices.length == 1) t.tokens(tokenIndices.head).parent.get
    else {
      val ts = tokenIndices.map(i => t.tokens(i))
      nonterminalThatSubsumes(ts)
    }
  }

  def belongsToNullCategory: Boolean = {
    if (t.isToken) t.label == "-NONE-"
    else t.children.forall(_.belongsToNullCategory)
  }

  def cfgRule: Option[String] = {
    for {
      p <- t.parent
      c = p.children.map(_.label).mkString(" ")
    } yield s"${p.label} -> $c"
  }

  def pathBetween(i: ConstituentTree, j: ConstituentTree): Path[ConstituentTree, String] = {
    GraphSearch.depthFirstWithAction(i, (x: ConstituentTree) => x == j)(
      ConstituentTree.TreeNodeStateSpaceWithAction
    )
  }

  override def toString = tree.label().value()
  override def hashCode() = 17 + root.hashCode() * 23 + tree.hashCode() * 23
  override def equals(that: Any) = that match {
    case that: ConstituentTree => this.root == that.root && this.tree == that.tree
    case _ => throw new Exception("You can't compare a pig with a cat, can you? ")
  }
}

object ConstituentTree {
  /**
   * Creates a wrapped version of the Stanford constituent parse tree.
   * @note Will re-index leaves.
   * @param _tree Root stanford [[Tree]] node.
   * @return
   */
  def apply(_tree: Tree): ConstituentTree = {
    _tree.indexLeaves(true)
    new ConstituentTree(_tree, _tree)
  }

  /**
   * Creates a wrapped version of the Stanford constituent parse tree from bracketed string
   * representation of the parse tree.
   * @param s A bracketed string representation like the following:
   *
   *             (ROOT
   *               (S
   *                 (NP (PRP$ My) (NN dog))
   *                 (ADVP (RB also))
   *                 (VP (VBZ likes)
   *                   (S
   *                     (VP (VBG eating)
   *
   * @return The wrapped [[ConstituentTree]]
   */
  def ofBracketedString(s: String) = ConstituentTree(Tree.valueOf(s))

  implicit object StanfordTreeStateSpace extends StateSpace[ConstituentTree] {
    def succ(s: ConstituentTree): Seq[ConstituentTree] = s.children
  }

  def TreeNodeStateSpaceWithAction[X]: StateSpaceWithAction[ConstituentTree, String] = new StateSpaceWithAction[ConstituentTree, String] {
    def actions = Seq("↑", "↓")
    def succ(s: ConstituentTree, a: String) = {
      if (a == "↑") s.parent.toSeq
      else if (a == "↓") s.children
      else Nil
    }
  }
}
