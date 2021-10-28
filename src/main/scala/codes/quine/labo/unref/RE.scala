package codes.quine.labo.unref

/** RE is an ordinary regular expression with no back-references. */
enum RE:
  case Lit(c: Char)
  case Assert(k: AssertKind)
  case Cat(rs: RE*)
  case Alt(rs: RE*)
  case PosLA(r: RE)
  case NegLA(r: RE)
  case Rep(r: RE, q: Quantifier, greedy: Boolean)

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
    case Rep(r, q, greedy) =>
      val s1 = r match
        case r: Cat => s"(?:$r)"
        case r: Alt => s"(?:$r)"
        case r: Rep => s"(?:$r)"
        case r      => r.toString
      val s2 = q match
        case Quantifier.Star            => "*"
        case Quantifier.Plus            => "+"
        case Quantifier.Question        => "?"
        case Quantifier.Exact(n)        => s"{$n}"
        case Quantifier.Unbounded(n)    => s"{$n,}"
        case Quantifier.Bounded(n1, n2) => s"{$n1,$n2}"
      val s3 = if greedy then "?" else ""
      s1 ++ s2 ++ s3
