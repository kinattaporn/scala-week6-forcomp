package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val ww = w.toList.map(x => x.toLower)
//    println(ww)                             // List(r, o, b, e, r, t)
    val gb = ww.groupBy(x => x)
//    println(gb)                             // HashMap(e -> List(e), t -> List(t), b -> List(b), r -> List(r, r), o -> List(o))
    val m = gb.map(x => (x._1, x._2.size))
//    println(m)                              // HashMap(e -> 1, t -> 1, b -> 1, r -> 2, o -> 1)
    val l = m.toList
//    println(l)                              // List((e,1), (t,1), (b,1), (r,2), (o,1))
//    println(l.sorted)                       // List((b,1), (e,1), (o,1), (r,2), (t,1))
    l.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val w = s.mkString("")
//    println(w)                              // abcde
    wordOccurrences(w)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    Dictionary.loadDictionary
      .groupBy(x => wordOccurrences(x))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(                         a, b
   * List(),                       0, 0
   * List(('a', 1)),               1, 0
   * List(('a', 2)),               2, 0
   * List(('b', 1)),               0, 1
   * List(('a', 1), ('b', 1)),     1, 1
   * List(('a', 2), ('b', 1)),     2, 1
   * List(('b', 2)),               0, 2
   * List(('a', 1), ('b', 2)),     1, 2
   * List(('a', 2), ('b', 2))      2, 2
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.length == 0) List(List())
    else if (occurrences.length == 1) combinations1(occurrences.head).map(x => x.filter(x => x._2 != 0))
    else combinations2(occurrences.head, combinations(occurrences.tail)).map(x => x.filter(x => x._2 != 0))
  }
  def combinations1(x: (Char, Int)): List[List[(Char, Int)]] = {
    val forCombinations = for {
      i <- 0 to x._2
    } yield List((x._1, i))
    forCombinations.toList
  }
  def combinations2(x: (Char, Int), l: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
    val forCombinations = for {
      i <- 0 to x._2
      j <- l
    } yield (x._1, i) +: j
    forCombinations.toList
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xy = x ::: y.map(x => (x._1, x._2 * -1))
//    println(xy)                                           // List((a,2), (b,3), (c,4), (a,-2), (c,-1))
    val gb = xy.groupBy(x => x._1)
//    println(gb)                                           // HashMap(a -> List((a,2), (a,-2)), b -> List((b,3)), c -> List((c,4), (c,-1)))
    val m = gb.map(x => (x._1, x._2.map(y => y._2).sum))
//    println(m)                                            // HashMap(a -> 0, b -> 3, c -> 3)
    val l = m.toList
//    println(l)                                            // List((a,0), (b,3), (c,3))
    val f = l.filter(x => x._2 != 0)
//    println(f)                                            // List((b,3), (c,3))
//    println(f.sorted)                                     // List((b,3), (c,3))
    f.sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.length == 0) List()
    else List ()
  }
  def remainAnagramsRecursive(o: Occurrences, wordList: List[Option[List[String]]]): List[(Occurrences, List[Option[List[String]]], Occurrences)] = {
    if (remainAnagrams(o, wordList).length == 0) remainAnagrams(o, wordList)
    else remainAnagrams(remainAnagramsRecursive(o, wordList)(0)._3, remainAnagramsRecursive(o, wordList)(0)._2)
//    else remainAnagrams(o, List()).foreach(x => remainAnagramsRecursive(x._3, x._2))
  }
  def remainAnagrams(o: Occurrences, wordList: List[Option[List[String]]]): List[(Occurrences, List[Option[List[String]]], Occurrences)] = {
    println("------------", o)
    combinations(o)
      .map(x => {
        val possibleWord = dictionaryByOccurrences.get(x)
        val remain = {
          if (possibleWord == None) List()
          else subtract(o, wordOccurrences(possibleWord.get(0)))
        }
        (x, wordList ::: List(possibleWord), remain)
      })
      .filter(x => !x._2.contains(None))
  }
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
