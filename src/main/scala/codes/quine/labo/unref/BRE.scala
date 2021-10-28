package codes.quine.labo.unref

/** BRE is a regular expression with back-reference. */
enum BRE:
  case Lit(c: Char)
  case Assert(k: AssertKind)
  case Cat(bs: BRE*)
  case Alt(bs: BRE*)
  case Rep(b: BRE, q: Quantifier, greedy: Boolean)
  case PosLA(b: BRE)
  case NegLA(b: BRE)
  case Cap(i: Int, b: BRE)
  case Ref(i: Int)

object BRE:

  /** Returns a set of capture index numbers in the given regular expression. */
  def caps(b: BRE): Set[Int] = b match
    case Lit(_)       => Set.empty
    case Assert(_)    => Set.empty
    case Cat(bs @ _*) => bs.iterator.flatMap(caps).toSet
    case Alt(bs @ _*) => bs.iterator.flatMap(caps).toSet
    case Rep(b, _, _) => caps(b)
    case PosLA(b)     => caps(b)
    case NegLA(_)     => Set.empty // Because captures in negative look-ahead are never captured.
    case Cap(i, b)    => Set(i) | caps(b)
    case Ref(_)       => Set.empty

  /** Returns a set of back-reference index numbers in the given regular expression. */
  def refs(b: BRE): Set[Int] = b match
    case Lit(_)       => Set.empty
    case Assert(_)    => Set.empty
    case Cat(bs @ _*) => bs.iterator.flatMap(refs).toSet
    case Alt(bs @ _*) => bs.iterator.flatMap(refs).toSet
    case Rep(b, _, _) => refs(b)
    case PosLA(b)     => refs(b)
    case NegLA(b)     => refs(b)
    case Cap(_, b)    => refs(b)
    case Ref(i)       => Set(i)

  /** Parses the given string as BRE. */
  def parse(s: String): Option[BRE] =
    val reserved = "|*+?{}()\\^$"

    def alt(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      var pos = pos0
      var index = index0
      val bs = Seq.newBuilder[BRE]

      cat(pos, index) match
        case Some((pos1, index1, b)) =>
          pos = pos1
          index = index1
          bs.addOne(b)
        case None => return None

      while pos < s.length && s(pos) == '|' do
        cat(pos + 1, index) match
          case Some((pos1, index1, b)) =>
            pos = pos1
            index = index1
            bs.addOne(b)
          case None => return None

      bs.result() match
        case Seq(b) => Some((pos, index, b))
        case bs     => Some((pos, index, Alt(bs: _*)))

    def cat(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      var pos = pos0
      var index = index0
      val bs = Seq.newBuilder[BRE]

      while pos < s.length && s(pos) != '|' && s(pos) != ')' do
        rep(pos, index) match
          case Some((pos1, index1, b)) =>
            pos = pos1
            index = index1
            bs.addOne(b)
          case None => return None

      bs.result() match
        case Seq(b) => Some((pos, index, b))
        case bs     => Some((pos, index, Cat(bs: _*)))

    def rep(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      paren(pos0, index0)
        .flatMap { case (pos, index, b) =>
          if pos < s.length && s(pos) == '*' then Some((pos + 1, index, Rep(b, Quantifier.Star, true)))
          else if pos < s.length && s(pos) == '+' then Some((pos + 1, index, Rep(b, Quantifier.Plus, true)))
          else if pos < s.length && s(pos) == '?' then Some((pos + 1, index, Rep(b, Quantifier.Question, true)))
          else if pos < s.length && s(pos) == '{' then
            val n1 = s.drop(pos + 1).takeWhile(_.isDigit)
            val pos1 = pos + 1 + n1.length
            if n1.nonEmpty && pos1 < s.length && s(pos1) == '}' then
              Some((pos1 + 1, index, Rep(b, Quantifier.Exact(n1.toInt), true)))
            else if n1.nonEmpty && pos1 < s.length && s(pos1) == ',' then
              if pos1 + 1 < s.length && s(pos1 + 1) == '}' then
                Some((pos1 + 2, index, Rep(b, Quantifier.Unbounded(n1.toInt), true)))
              else
                val n2 = s.drop(pos1 + 1).takeWhile(_.isDigit)
                val pos2 = pos1 + 1 + n2.length
                if n2.nonEmpty && pos2 < s.length && s(pos2) == '}' then
                  Some((pos2 + 1, index, Rep(b, Quantifier.Bounded(n1.toInt, n2.toInt), true)))
                else None
            else None
          else Some((pos, index, b))
        }
        .map {
          case (pos, index, Rep(b, q, _)) if pos < s.length && s(pos) == '?' =>
            (pos + 1, index, Rep(b, q, false))
          case (pos, index, b) => (pos, index, b)
        }

    def paren(pos0: Int, index0: Int): Option[(Int, Int, BRE)] =
      if s.startsWith("(?:", pos0) then
        alt(pos0 + 3, index0).flatMap { case (pos1, index1, b) =>
          if pos1 < s.length && s(pos1) == ')' then Some((pos1 + 1, index1, b))
          else None
        }
      else if s.startsWith("(?=", pos0) then
        alt(pos0 + 3, index0).flatMap { case (pos1, index1, b) =>
          if pos1 < s.length && s(pos1) == ')' then Some((pos1 + 1, index1, PosLA(b)))
          else None
        }
      else if s.startsWith("(?!", pos0) then
        alt(pos0 + 3, index0).flatMap { case (pos1, index1, b) =>
          if pos1 < s.length && s(pos1) == ')' then Some((pos1 + 1, index1, NegLA(b)))
          else None
        }
      else if pos0 < s.length && s(pos0) == '(' then
        alt(pos0 + 1, index0 + 1).flatMap { case (pos1, index1, b) =>
          if pos1 < s.length && s(pos1) == ')' then Some((pos1 + 1, index1, Cap(index0, b)))
          else None
        }
      else lit(pos0).map { case (pos1, b) => (pos1, index0, b) }

    def lit(pos0: Int): Option[(Int, BRE)] =
      if pos0 < s.length && s(pos0) == '\\' then
        if pos0 + 1 < s.length && s(pos0 + 1).isDigit then
          val n = s.drop(pos0 + 1).takeWhile(_.isDigit)
          Some((pos0 + 1 + n.length, Ref(n.toInt)))
        else if pos0 + 1 < s.length && s(pos0 + 1) == 'b' then Some((pos0 + 2, Assert(AssertKind.WordBoundary)))
        else if pos0 + 1 < s.length && s(pos0 + 1) == 'B' then Some((pos0 + 2, Assert(AssertKind.NotWordBoundary)))
        else if pos0 + 1 < s.length then Some((pos0 + 2, Lit(s(pos0 + 1))))
        else None
      else if pos0 < s.length && s(pos0) == '^' then Some((pos0 + 1, Assert(AssertKind.InputBegin)))
      else if pos0 < s.length && s(pos0) == '$' then Some((pos0 + 1, Assert(AssertKind.InputEnd)))
      else if pos0 < s.length && !reserved.contains(s(pos0)) then Some((pos0 + 1, Lit(s(pos0))))
      else None

    alt(0, 1).flatMap { case (pos1, _, b) =>
      if pos1 == s.length then Some(b)
      else None
    }
