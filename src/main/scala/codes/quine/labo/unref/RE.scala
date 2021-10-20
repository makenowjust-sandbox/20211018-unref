package codes.quine.labo.unref

/** RE is an usual regular expression. */
sealed abstract class RE

object RE {
  final case class Lit(c: Char) extends RE {
    override def toString: String = Util.escape(c)
  }

  final case class Cat(rs: RE*) extends RE {
    override def toString: String = {
      val ss = rs.map {
        case r: Cat => s"(?:$r)"
        case r: Alt => s"(?:$r)"
        case r      => r.toString
      }
      ss.mkString
    }
  }

  final case class Alt(rs: RE*) extends RE {
    override def toString: String = {
      val ss = rs.map {
        case r: Alt => s"(?:$r)"
        case r      => r.toString
      }
      ss.mkString("|")
    }
  }

  final case class Star(r: RE) extends RE {
    override def toString: String =
      r match {
        case r: Cat  => s"(?:$r)*"
        case r: Alt  => s"(?:$r)*"
        case r: Star => s"(?:$r)*"
        case r       => s"$r*"
      }
  }
}
