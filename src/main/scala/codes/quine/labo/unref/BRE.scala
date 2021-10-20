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
}
