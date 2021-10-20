package codes.quine.labo.unref

/** BRE is a regular expression with back-reference. */
sealed abstract class BRE

object BRE {
  final case class Lit(c: Char) extends BRE
  final case class Cat(bs: BRE*) extends BRE
  final case class Alt(bs: BRE*) extends BRE
  final case class Star(b: BRE) extends BRE
  final case class Cap(i: Int, b: BRE) extends BRE
  final case class Ref(i: Int) extends BRE

  /** Returns a set of capture index numbers in the given regular expression. */
  def caps(b: BRE): Set[Int] = b match {
    case Lit(_)       => Set.empty
    case Cat(bs @ _*) => bs.iterator.flatMap(caps).toSet
    case Alt(bs @ _*) => bs.iterator.flatMap(caps).toSet
    case Star(b)      => caps(b)
    case Cap(i, b)    => Set(i) | caps(b)
    case Ref(_)       => Set.empty
  }

  /** Returns a set of back-reference index numbers in the given regular expression.
    */
  def refs(b: BRE): Set[Int] = b match {
    case Lit(_)       => Set.empty
    case Cat(bs @ _*) => bs.iterator.flatMap(refs).toSet
    case Alt(bs @ _*) => bs.iterator.flatMap(refs).toSet
    case Star(b)      => refs(b)
    case Cap(_, b)    => refs(b)
    case Ref(i)       => Set(i)
  }

  /** Parses the given string as BRE. */
  def parse(s: String): Option[BRE] = {
    val reserved = "|*()\\"

    def alt(pos0: Int, index0: Int): Option[(Int, Int, BRE)] = {
      var pos = pos0
      var index = index0
      val bs = Seq.newBuilder[BRE]

      cat(pos, index) match {
        case Some((pos1, index1, b)) =>
          pos = pos1
          index = index1
          bs.addOne(b)
        case None => return None
      }

      while (pos < s.length && s(pos) == '|') {
        cat(pos + 1, index) match {
          case Some((pos1, index1, b)) =>
            pos = pos1
            index = index1
            bs.addOne(b)
          case None => return None
        }
      }

      bs.result() match {
        case Seq(b) => Some((pos, index, b))
        case bs     => Some((pos, index, Alt(bs: _*)))
      }
    }

    def cat(pos0: Int, index0: Int): Option[(Int, Int, BRE)] = {
      var pos = pos0
      var index = index0
      val bs = Seq.newBuilder[BRE]

      while (pos < s.length && s(pos) != '|' && s(pos) != ')') {
        star(pos, index) match {
          case Some((pos1, index1, b)) =>
            pos = pos1
            index = index1
            bs.addOne(b)
          case None => return None
        }
      }

      bs.result() match {
        case Seq(b) => Some((pos, index, b))
        case bs     => Some((pos, index, Cat(bs: _*)))
      }
    }

    def star(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      paren(pos0, index0).map { case (pos, index, b) =>
        if (pos < s.length && s(pos) == '*') (pos + 1, index, Star(b))
        else (pos, index, b)
      }

    def paren(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      if (s.startsWith("(?:", pos0))
        alt(pos0 + 3, index0).flatMap { case (pos1, index1, b) =>
          if (pos1 < s.length && s(pos1) == ')') Some((pos1 + 1, index1, b))
          else None
        }
      else if (pos0 < s.length && s(pos0) == '(')
        alt(pos0 + 1, index0 + 1).flatMap { case (pos1, index1, b) =>
          if (pos1 < s.length && s(pos1) == ')') Some((pos1 + 1, index1, Cap(index0, b)))
          else None
        }
      else lit(pos0).map { case (pos1, b) => (pos1, index0, b) }

    def lit(pos0: Int): Option[(Int, BRE)] =
      if (pos0 < s.length && s(pos0) == '\\')
        if (pos0 + 1 < s.length && s(pos0 + 1).isDigit) {
          val n = s.drop(pos0 + 1).takeWhile(_.isDigit)
          Some((pos0 + 1 + n.length, Ref(n.toInt)))
        } else if (pos0 + 1 < s.length) Some((pos0 + 2, Lit(s(pos0 + 1))))
        else None
      else if (pos0 < s.length && !reserved.contains(s(pos0))) Some((pos0 + 1, Lit(s(pos0))))
      else None

    alt(0, 1).flatMap { case (pos1, _, b) =>
      if (pos1 == s.length) Some(b)
      else None
    }
  }
}
