package common

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

/**
  * Created by alvaro on 7/06/16.
  */
@RunWith(classOf[JUnitRunner])
class HMapTest extends FlatSpec {

  object stringK extends HKey[String]

  object intK extends HKey[Int]

  object seqIntK extends HKey[Seq[Int]]


  "HMap" should "be created empty" in {
    val map = HMap()
    assert(map.keys.size == 0)
  }

  "HMap" should "store a new key" in {
    val map = HMap()
    val map2 = map(stringK -> "hola")
    assert(map2.keys.size == 1)
    assert(map2(stringK).get == "hola")
  }

  "HMap" should "store three keys" in {
    val map = HMap()
    val map2 = map(stringK -> "hola")(intK -> 1)(seqIntK -> Seq(1, 2, 3))
    assert(map2.keys.size == 3)
    assert(map2(stringK).get == "hola")
    assert(map2(intK).get == 1)
    assert(map2(seqIntK).get == Seq(1, 2, 3))
  }


  "HMap" should "replace a key" in {
    val map = HMap()
    val map2 = map(stringK -> "hola")
    val map3 = map2(stringK -> "adios")
    assert(map3.keys.size == 1)
    assert(map3(stringK).get == "adios")
  }

  "HMap" should "replace two keys" in {
    val map = HMap()
    val map2 = map(stringK -> "hola")(intK -> 1)(seqIntK -> Seq(1, 2, 3))
    val map3 = map2(stringK -> "adios")(intK -> 2)
    println(s"$map2 $map3")
    assert(map3.keys.size == 3)
    assert(map3(stringK).get == "adios")
    assert(map3(intK).get == 2)
    assert(map3(seqIntK).get == Seq(1, 2, 3))
  }

  "HMap" should "replace three keys" in {
    val map = HMap()
    val map2 = map(stringK -> "hola")(intK -> 1)(seqIntK -> Seq(1, 2, 3))
    val map3 = map2(stringK -> "adios")(intK -> 2)(seqIntK -> Seq(4, 5, 6))
    println(s"$map2 $map3")
    assert(map3.keys.size == 3)
    assert(map3(stringK).get == "adios")
    assert(map3(intK).get == 2)
    assert(map3(seqIntK).get == Seq(4, 5, 6))
  }

}
