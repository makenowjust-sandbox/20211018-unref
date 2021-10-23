package codes.quine.labo.unref

/** RE is an usual regular expression. */
enum RE:
  case Lit(c: Char)
  case Assert(k: AssertKind)
  case Cat(rs: RE*)
  case Alt(rs: RE*)
  case PosLA(r: RE)
  case NegLA(r: RE)
  case Star(r: RE)

  override def toString: String = this match
    case Lit(c) => Util.escape(c)
    case Assert(k) =>
      k match
        case AssertKind.InputBegin      => "^"
        case AssertKind.InputEnd        => "$"
        case AssertKind.WordBoundary    => "\\b"
        case AssertKind.NotWordBoundary => "\\B"
    case Cat(rs @ _*) =>
      val ss = rs.map {
        case r: Cat => s"(?:$r)"
        case r: Alt => s"(?:$r)"
        case r      => r.toString
      }
      ss.mkString
    case Alt() => "(?!)"
    case Alt(rs @ _*) =>
      val ss = rs.map {
        case r: Alt if r.rs.size >= 2 => s"(?:$r)"
        case r                        => r.toString
      }
      ss.mkString("|")
    case PosLA(r) => s"(?=$r)"
    case NegLA(r) => s"(?!$r)"
    case Star(r) =>
      r match
        case r: Cat  => s"(?:$r)*"
        case r: Alt  => s"(?:$r)*"
        case r: Star => s"(?:$r)*"
        case r       => s"$r*"
