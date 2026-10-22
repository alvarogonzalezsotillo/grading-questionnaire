package webserver.json

/**
  * A minimal JSON writer.
  *
  * The web server only needs to serialize (never to parse) JSON, so instead of adding a new
  * dependency to the build a very small writer is enough.
  */
object Json {

  def str(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"' => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case c if c < ' ' => sb.append("\\u%04x".format(c.toInt))
      case c => sb.append(c)
    }
    sb.append('"').toString()
  }

  def num(n: Int): String = n.toString

  def num(n: Long): String = n.toString

  def num(n: Double): String = n.toString

  def bool(b: Boolean): String = b.toString

  def nullValue: String = "null"

  def optStr(o: Option[String]): String = o.map(str).getOrElse(nullValue)

  def optNum(o: Option[Int]): String = o.map(num).getOrElse(nullValue)

  /** Builds a JSON object. The values must be already serialized */
  def obj(fields: (String, String)*): String =
    fields.map { case (k, v) => str(k) + ":" + v }.mkString("{", ",", "}")

  /** Builds a JSON array. The items must be already serialized */
  def arr(items: Seq[String]): String = items.mkString("[", ",", "]")

}
