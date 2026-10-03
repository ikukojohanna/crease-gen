package origami.studio

/** Just enough JSON to answer the studio page. */
private[studio] object Json:

  def obj(fields: (String, String)*): String = fields.map((k, v) => s"${str(k)}:$v").mkString("{", ",", "}")

  def arr(items: Seq[String]): String = items.mkString("[", ",", "]")

  def str(s: String): String =
    val escaped = s.flatMap:
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    "\"" + escaped + "\""

  def num(x: Double): String = if x.isFinite then f"$x%.4f" else "null"

  def bool(b: Boolean): String = b.toString

  val nul: String = "null"
