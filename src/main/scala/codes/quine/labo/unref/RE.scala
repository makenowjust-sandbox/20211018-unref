package codes.quine.labo.unref

/** RE is an usual regular expression. */
sealed abstract class RE

object RE {
  final case class Lit(c: Char) extends RE
  final case class Cat(rs: RE*) extends RE
  final case class Alt(rs: RE*) extends RE
  final case class Star(r: RE) extends RE
}
