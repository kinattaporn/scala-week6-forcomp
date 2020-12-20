package forcomp

import forcomp.Anagrams.sentenceOccurrences
import org.junit._
import org.junit.Assert.assertEquals


class AnagramsSuite {
  import Anagrams._

  @Test def `wordOccurrences: abcd (3pts)`: Unit =
    assertEquals(List(('a', 1), ('b', 1), ('c', 1), ('d', 1)), wordOccurrences("abcd"))

  @Test def `wordOccurrences: Robert (3pts)`: Unit =
    assertEquals(List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)), wordOccurrences("Robert"))


  @Test def `sentenceOccurrences: abcd e (5pts)`: Unit =
    assertEquals(List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)), sentenceOccurrences(List("abcd", "e")))


  @Test def `dictionaryByOccurrences.get: eat (10pts)`: Unit = {
    println(dictionaryByOccurrences(List(('a', 1), ('e', 1), ('t', 1))))
    assertEquals(Some(Set("ate", "eat", "tea")), dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet))
  }


  @Test def `wordAnagrams married (2pts)`: Unit =
    assertEquals(Set("married", "admirer"), wordAnagrams("married").toSet)

  @Test def `wordAnagrams player (2pts)`: Unit =
    assertEquals(Set("parley", "pearly", "player", "replay"), wordAnagrams("player").toSet)


  @Test def `combinations test`: Unit = {
    val c1 = combinations2(('b', 3), combinations1(('c', 4)))
    val c2 = combinations2(('a', 2), combinations2(('b', 3), combinations1(('c', 4))))
    val o1 = List(('c', 4))
    val o2 = List(('b', 3), ('c', 4))
    val o3 = List(('a', 2), ('b', 3), ('c', 4))
    println("---------- c1")
    c1.foreach(println)
    println("---------- c2")
    c2.foreach(println)
    println("---------- o1")
    combinations(o1).foreach(println)
    println("---------- o2")
    combinations(o2).foreach(println)
    println("---------- o3")
    combinations(o3).foreach(println)
  }

  @Test def `combinations: [] (8pts)`: Unit =
    assertEquals(List(Nil), combinations(Nil))

  @Test def `combinations: abba (8pts)`: Unit = {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(abbacomb.toSet, combinations(abba).toSet)
  }


  @Test def `subtract test`: Unit = {
    subtract(List(('a', 2), ('b', 3), ('c', 4)), List(('a', 2), ('c', 1)))
  }

  @Test def `subtract: lard - r (10pts)`: Unit = {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(lad, subtract(lard, r))
  }


  @Test def `sentence anagrams: [] (10pts)`: Unit = {
    val sentence = List()
    assertEquals(List(Nil), sentenceAnagrams(sentence))
  }

  @Test def `sentence anagrams: Linux rulez (10pts)`: Unit = {
    val sentence = List("Linux", "rulez")
    val possibleWord = findPossibleWord(sentence)
    println("----- possibleWord")
    possibleWord.foreach(println)

    val list1 = List("Lin", "nil")
    val list2 = List("run", "urn")
    val list3 = List("lien", "line", "Neil", "Nile")
    val list4 = List("lure", "rule")
    val list1234 = List(list1, list2, list3, list4)
    val cw34 = combinationsWord2(list3, list4)
    val cw234 = combinationsWord3(list2, combinationsWord2(list3, list4))
    val cw1234 = combinationsWord3(list1, combinationsWord3(list2, combinationsWord2(list3, list4)))
    val cwr = combinationsWord(list1234)
    val cwr2 = combinationsWord(possibleWord)
//    println("----- cw34", cw34)
//    cw34.foreach(println)
//    println("----- cw234", cw234)
//    cw234.foreach(println)
//    println("----- cw1234", cw1234)
//    cw1234.foreach(println)
//    println("----- cwr", cwr)
//    cwr.foreach(println)
//    println("----- cwr2", cwr2)
//    cwr2.foreach(println)

    val sa = sentenceAnagrams(sentence)
    println("----- sa", sa)
    sa.foreach(println)

    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
//    assertEquals(anas.toSet, sentenceAnagrams(sentence).toSet)
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(1000 * 1000)
}
