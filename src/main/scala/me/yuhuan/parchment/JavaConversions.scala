package me.yuhuan.parchment

import edu.stanford.nlp.ling._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object JavaConversions {
  def StanfordSentence(words: Seq[String]): Seq[HasWord] = {
    words.map(w ⇒ {
      new HasWord { inner ⇒
        var word = w
        def setWord(word: String): Unit = inner.word = word
      }
    })
  }
}
