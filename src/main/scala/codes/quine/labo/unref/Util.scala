package codes.quine.labo.unref

object Util {
  def escape(c: Char): String = c match {
    case '|'  => "\\|"
    case '*'  => "\\*"
    case '('  => "\\("
    case ')'  => "\\)"
    case '\\' => "\\\\"
    case c    => c.toString
  }
}
