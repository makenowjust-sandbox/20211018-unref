package codes.quine.labo.unref

/** Util provides simple utilities. */
object Util:
  /** Returns the escaped character as string. */
  def escape(c: Char): String = c match
    case '|'  => "\\|"
    case '*'  => "\\*"
    case '('  => "\\("
    case ')'  => "\\)"
    case '\\' => "\\\\"
    case c    => c.toString
