import scala.collection.immutable.SortedMap

/**
 * Created by alvaro on 22/06/15.
 */
object NumerosRomanosScala extends App {

  object cantidades {

    private val initialMap = SortedMap(
      1 -> "i",
      4 -> "iv",
      9 -> "ix",
      10 -> "x",
      29 -> "ix",
      40 -> "xl",
      50 -> "l",
      90 -> "xc",
      100 -> "c",
      400 -> "cd",
      500 -> "d",
      900 -> "cm",
      1000 -> "m")

    private val map = initialMap ++ initialMap.map {
      case (i, s) => i * 1000 -> s.toUpperCase
    }

    private val valores = map.keys.toList.reverse

    def apply(i: Int) = map(i)

    def mayorValorQuePuedoRestarDe(n: Int) = valores.find(_ <= n).get

  }

  def aRomano(n: Int): String = {
    if (n <= 0) {
      ""
    }
    else {
      val i = cantidades.mayorValorQuePuedoRestarDe(n)
      cantidades(i) + aRomano(n - i)
    }
  }

  println(aRomano(111111))

}
