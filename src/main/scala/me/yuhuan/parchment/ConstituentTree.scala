package me.yuhuan.parchment

import edu.stanford.nlp._
import edu.stanford.nlp.ling.HasIndex
import edu.stanford.nlp.trees._
import me.yuhuan.marauder._
import scala.collection.JavaConversions._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  *         sdaf
  */
class ConstituentTree private[parchment](val _root: Tree, val tree: Tree) { t =>
  def label: String = t.tree.label().value()

  def parent: Option[ConstituentTree] = {
    val p = t.tree.parent(t._root)
    if (p != null) Some(new ConstituentTree(t._root, p)) else None
  }

  def children: Seq[ConstituentTree] = t.tree.children().map(c => new ConstituentTree(t._root, c))

  def root: ConstituentTree = new ConstituentTree(_root, _root)

  def isToken: Boolean = t.tree.isLeaf
  def isNonterminal = !isToken
  def hasParent: Boolean = parent.nonEmpty
  def hasNoParent = !hasParent

  def tokens: Seq[ConstituentTree] = t.tree.getLeaves[Tree].map(l => new ConstituentTree(t._root, l))
  def tokenLabels = tokens.map(_.label)

  def headToken: Option[ConstituentTree] = tokens.headOption
  def lastToken: Option[ConstituentTree] = tokens.lastOption

  /**
   * The index in the whole sentence (with ROOT at 0).
   * Notice that, even if `this` is a constituent that covers only a substring of the whole
   * sentence, the index will still be with respect to the whole sentence.
   */
  def indexWithRoot: Int = {
    if (!t.isToken) throw new Exception("You can't expect non-terminals to have indices, can you?")
    else t.tree.label().asInstanceOf[HasIndex].index()
  }

  def index = indexWithRoot - 1

  /**
    * If the tree looks like:
    *
    *           S
    *          / \
    *        NP   VP
    *       / \   / \
    *     DT  NN V  PP
    *               / \
    *              P  NP
    *
    * and suppose the current constituent (`this`) is P, then this method should return:
    * `Seq(1, 1, 0)`
    */
  def numPathFrom(node: ConstituentTree): Array[Int] = {
    var cur = this
    val result = scala.collection.mutable.ArrayBuffer[Int]()
    while (cur != node) {
      val Some(i) = cur.indexInSiblings
      result += i
      cur = cur.parent.get
    }
    result.reverse.toArray
  }

  def numPathFromRoot: Array[Int] = numPathFrom(root)

  /**
    * The node obtained by following the children indices in the provided sequence.
 *
    * @param numPath A sequence of integers each representing the index of the child to follow.
    */
  def followNumPath(numPath: Seq[Int]): Option[ConstituentTree] = {
    var cur = root
    try {
      for (i <- numPath) {
        cur = cur.children(i)
      }
      Some(cur)
    }
    catch {
      case e: Exception => None
    }
  }

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

  def tokenBefore: Option[ConstituentTree] = t.headToken.flatMap { ht =>
    val i = ht.indexWithRoot - 2
    if (i < 0 || i >= t.root.tokens.length) None
    else Some(t.root.tokens(i))
  }

  def tokenAfter: Option[ConstituentTree] = t.lastToken.flatMap { ht =>
    val i = ht.indexWithRoot
    if (i < 0 || i >= t.root.tokens.length) None
    else Some(t.root.tokens(i))
  }

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

  def subsumes(n: ConstituentTree): Boolean = {
    val Some(head1) = t.headToken
    val Some(head2) = n.headToken
    val Some(last1) = t.lastToken
    val Some(last2) = n.lastToken

    head1.indexWithRoot <= head2.indexWithRoot && last2.indexWithRoot <= last1.indexWithRoot
  }


  def syntacticHead: Option[ConstituentTree] = {
    val hf = new CollinsHeadFinder
    val h = t.tree.headTerminal(hf)
    if (h != null) Some(new ConstituentTree(t._root, h)) else None
  }

  def syntacticPreHead: Option[ConstituentTree] = {
    val hf = new CollinsHeadFinder
    val h = t.tree.headPreTerminal(hf)
    if (h != null) Some(new ConstituentTree(t._root, h)) else None
  }

  def semanticHead: Option[ConstituentTree] = {
    val shf = new SemanticHeadFinder
    val sh = t.tree.headTerminal(shf)
    if (sh != null) Some(new ConstituentTree(t._root, sh)) else None
  }

  def semanticPreHead: Option[ConstituentTree] = {
    val shf = new SemanticHeadFinder
    val sh = t.tree.headPreTerminal(shf)
    if (sh != null) Some(new ConstituentTree(t._root, sh)) else None
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

  def entityThatSubsumesTokens(tokenIndices: Range): Option[ConstituentTree] = {
    val u = t.nonterminalThatSubsumesTokens(tokenIndices)

    def isWantedNonterminal(n: ConstituentTree): Boolean = {
      val conditions = Seq(
        Set('V') contains n.label(0),
        Set("WHNP", "NP", "SBAR") contains n.label
      )
      conditions contains true
    }

    def biggerProjectionOf(NP: ConstituentTree): Option[ConstituentTree] = {
      require(NP.label == "NP")
      if (!NP.parent.exists(_.label == "NP")) Some(NP)
      else NP.parent.flatMap(biggerProjectionOf)
    }

    def go(t: ConstituentTree): Option[ConstituentTree] = {
      if (isWantedNonterminal(u)) Some(u)
      else (u.syntacticPreHead ++ u.parent ++ u.parent.flatMap(_.syntacticPreHead)).find(x => isWantedNonterminal(x))
    }

    val result = go(u)
    result.flatMap(x => if (x.label == "VP") x.semanticPreHead else Some(x))
          .flatMap(x => if (x.label == "NP") biggerProjectionOf(x) else Some(x))
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
    GraphSearch.breadthFirstWithAction(i, (x: ConstituentTree) => x == j)(
      ConstituentTree.TreeNodeStateSpaceWithAction
    )
  }

  def str = s"[${tree.label.value}]${tokens.mkString(" ")}"

  override def toString = tree.label.value
  override def hashCode() = 17 + _root.hashCode() * 23 + tree.hashCode() * 23
  override def equals(that: Any) = that match {
    case that: ConstituentTree => this._root == that._root && this.tree == that.tree
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
