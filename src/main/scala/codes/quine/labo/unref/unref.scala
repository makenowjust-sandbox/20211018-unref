package codes.quine.labo.unref

import scala.annotation.tailrec

/** unref is a function that returns a regular expression that resolves backreferences in the regular expression (BRE).
  *
  * The data types and functions for the implementation of unref are also defined in this object.
  */
object unref extends (BRE => RE):

  /** C represents a character in a capture.
    *
    * This may include nested captures, unresolved back references, and zero-width assertions, in addition to the usual
    * characters.
    */
  enum C:
    case Lit(c: Char)
    case Assert(k: AssertKind)
    case PosLA0(s: S)
    case PosLA1(t: T)
    case NegLA(s: S)
    case Scoped(w: W)
    case Cap(i: Int, w: W)
    case Ref(i: Int)

    override def toString: String = this match
      case Lit(c)    => s"C.Lit('$c')"
      case Assert(k) => s"C.Assert($k)"
      case PosLA0(s) => s"C.PosLA0($s)"
      case PosLA1(t) => s"C.PosLA1($t)"
      case NegLA(s)  => s"C.NegLA($s)"
      case Scoped(w) => s"C.Scoped($w)"
      case Cap(i, w) => s"C.Cap($i, $w)"
      case Ref(i)    => s"C.Ref($i)"

  object C:

    /** Returns a set of capture index numbers in the given capture character. */
    def caps(c: C): Set[Int] = c match
      case Lit(_)    => Set.empty
      case Assert(_) => Set.empty
      case PosLA0(_) => Set.empty
      case PosLA1(t) => T.caps(t)
      case NegLA(_)  => Set.empty
      case Scoped(w) => W.caps(w)
      case Cap(i, w) => Set(i) | W.caps(w)
      case Ref(_)    => Set.empty

    /** Returns a set of back-reference index numbers in the given capture character. */
    def refs(c: C): Set[Int] = c match
      case Lit(_)    => Set.empty
      case Assert(_) => Set.empty
      case PosLA0(s) => S.refs(s)
      case PosLA1(t) => T.refs(t)
      case NegLA(s)  => S.refs(s)
      case Scoped(w) => W.refs(w)
      case Cap(_, w) => W.refs(w)
      case Ref(i)    => Set(i)

  /** W represents a word in a capture. */
  final case class W(cs: C*):

    /** Concatenates with two words into one. */
    def ++(w: W): W = W(cs ++ w.cs: _*)

    override def toString: String = cs.mkString("W(", ", ", ")")

  object W:

    /** Returns a set of capture index numbers in the given capture word. */
    def caps(w: W): Set[Int] = w.cs.iterator.flatMap(C.caps).toSet

    /** Returns a set of back-reference index numbers in the given capture word. */
    def refs(w: W): Set[Int] = w.cs.iterator.flatMap(C.refs).toSet

    def repeat(ws: Seq[W], n: Int): Seq[W] =
      @tailrec
      def loop(acc: Seq[W], n: Int): Seq[W] =
        if n == 0 then acc
        else if n == 1 then acc.flatMap(w1 => ws.map(w2 => w1 ++ w2))
        else loop(acc.flatMap(w1 => ws.map(w2 => w1 ++ W(C.Scoped(w2)))), n - 1)
      loop(Seq(W()), n)

  /** T is a regular expression having no capture ambiguity. */
  enum T:
    case Cat(ts: T*)
    case PosLA(t: T)
    case NonEmpty(t: T)
    case NoCap(n: N)
    case Cap(i: Int, w: W)

    /** Concatenates with two regular expressions into sequential one. */
    def ++(that: T): T = (this, that) match
      case (Cat(), t)                     => t
      case (t, Cat())                     => t
      case (Cat(ts1 @ _*), Cat(ts2 @ _*)) => Cat(ts1 ++ ts2: _*)
      case (Cat(ts @ _*), t)              => Cat(ts :+ t: _*)
      case (t, Cat(ts @ _*))              => Cat(t +: ts: _*)
      case (NoCap(n1), NoCap(n2))         => NoCap(n1 ++ n2)
      case (t1, t2)                       => Cat(t1, t2)

    override def toString: String = this match
      case Cat(ts @ _*) => ts.mkString("T.Cat", ", ", ")")
      case PosLA(t)     => s"T.PosLA($t)"
      case NonEmpty(t)  => s"T.NonEmpty($t)"
      case NoCap(n)     => s"T.NoCap($n)"
      case Cap(i, w)    => s"T.Cap($i, $w)"

  object T:

    /** Returns a set of capture index numbers in the given T. */
    def caps(t: T): Set[Int] = t match
      case Cat(ts @ _*) => ts.iterator.flatMap(caps).toSet
      case PosLA(t)     => caps(t)
      case NonEmpty(t)  => caps(t)
      case NoCap(_)     => Set.empty
      case Cap(i, w)    => Set(i) | W.caps(w)

    /** Returns a set of back-reference index numbers in the given T. */
    def refs(t: T): Set[Int] = t match
      case Cat(ts @ _*) => ts.iterator.flatMap(refs).toSet
      case PosLA(t)     => refs(t)
      case NonEmpty(t)  => refs(t)
      case NoCap(n)     => N.refs(n)
      case Cap(_, w)    => W.refs(w)

  /** N is a regular expression. It looks like BRE, but it has no capture. */
  enum N:
    case Lit(c: Char)
    case Assert(k: AssertKind)
    case PosLA(s: S)
    case NegLA(s: S)
    case Cat(ns: N*)
    case Alt(ns: N*)
    case Rep(s: S, q: Q)
    case Ref(i: Int)

    /** Concatenates with two regular expressions into sequential one. */
    def ++(that: N): N = (this, that) match
      case (Cat(), n)                     => n
      case (n, Cat())                     => n
      case (Cat(ns1 @ _*), Cat(ns2 @ _*)) => Cat(ns1 ++ ns2: _*)
      case (Cat(ns @ _*), n)              => Cat(ns :+ n: _*)
      case (n, Cat(ns @ _*))              => Cat(n +: ns: _*)
      case (n1, n2)                       => Cat(n1, n2)

    /** Combines with two regular expressions into alternative one. */
    def |(that: N): N = (this, that) match
      case (Alt(), n)                     => n
      case (n, Alt())                     => n
      case (Alt(ns1 @ _*), Alt(ns2 @ _*)) => Alt(ns1 ++ ns2: _*)
      case (Alt(ns @ _*), n)              => Alt(ns :+ n: _*)
      case (n, Alt(ns @ _*))              => Alt(n +: ns: _*)
      case (n1, n2)                       => Alt(n1, n2)

    override def toString: String = this match
      case Lit(c)       => s"N.Lit('$c')"
      case Assert(k)    => s"N.Assert($k)"
      case PosLA(s)     => s"N.PosLA($s)"
      case NegLA(s)     => s"N.NegLA($s)"
      case Cat(ns @ _*) => ns.mkString("N.Cat", ", ", ")")
      case Alt(ns @ _*) => ns.mkString("N.Alt", ", ", ")")
      case Rep(s, q)    => s"N.Rep($s, $q)"
      case Ref(i)       => s"N.Ref($i)"

  object N:

    /** Returns a set of back-reference index numbers in the given N. */
    def refs(n: N): Set[Int] = n match
      case Lit(_)       => Set.empty
      case Assert(_)    => Set.empty
      case PosLA(s)     => S.refs(s)
      case NegLA(s)     => S.refs(s)
      case Cat(ns @ _*) => ns.iterator.flatMap(refs).toSet
      case Alt(ns @ _*) => ns.iterator.flatMap(refs).toSet
      case Rep(s, q)    => S.refs(s)
      case Ref(i)       => Set(i)

  /** P is a pure regular expression.
    *
    * Note that it is the same structure as RE.
    */
  enum P:
    case Lit(c: Char)
    case Assert(k: AssertKind)
    case PosLA(p: P)
    case NegLA(p: P)
    case Cat(ps: P*)
    case Alt(ps: P*)
    case Rep(p: P, q: Q)

    /** Concatenates with two pure regular expressions into one. */
    def ++(that: P): P = (this, that) match
      case (Alt(), _)                     => Alt()
      case (_, Alt())                     => Alt()
      case (Cat(), p)                     => p
      case (p, Cat())                     => p
      case (Cat(ps1 @ _*), Cat(ps2 @ _*)) => Cat(ps1 ++ ps2: _*)
      case (Cat(ps @ _*), p)              => Cat(ps :+ p: _*)
      case (p, Cat(ps @ _*))              => Cat(p +: ps: _*)
      case (p1, p2)                       => Cat(p1, p2)

    /** Combines with two pure regular expressions into one. */
    def |(that: P): P = (this, that) match
      case (Alt(), p)                     => p
      case (p, Alt())                     => p
      case (Alt(ps1 @ _*), Alt(ps2 @ _*)) => Alt(ps1 ++ ps2: _*)
      case (Alt(ps @ _*), p)              => Alt(ps :+ p: _*)
      case (p, Alt(ps @ _*))              => Alt(p +: ps: _*)
      case (p1, p2)                       => Alt(p1, p2)

    /** Converts this into N. */
    def toN: N = this match
      case Lit(c)       => N.Lit(c)
      case Assert(k)    => N.Assert(k)
      case PosLA(p)     => N.PosLA(S(Seq.empty, p))
      case NegLA(p)     => N.NegLA(S(Seq.empty, p))
      case Cat(ps @ _*) => N.Cat(ps.map(_.toN): _*)
      case Alt(ps @ _*) => N.Alt(ps.map(_.toN): _*)
      case Rep(p, q)    => N.Rep(S(Seq.empty, p), q)

    /** Converts this into RE. */
    def toRE: RE = this match
      case Lit(c)                              => RE.Lit(c)
      case Assert(k)                           => RE.Assert(k)
      case PosLA(p)                            => RE.PosLA(p.toRE)
      case NegLA(p)                            => RE.NegLA(p.toRE)
      case Cat(ps @ _*)                        => RE.Cat(ps.map(_.toRE).filter(_ != RE.Cat()): _*)
      case Alt(ps @ _*)                        => RE.Alt(ps.map(_.toRE): _*)
      case Rep(_, Q(0, Some(0)))               => RE.Cat()
      case Rep(p, Q(1, Some(1)))               => p.toRE
      case Rep(p, Q(0, None))                  => RE.Rep(p.toRE, Quantifier.Star)
      case Rep(p, Q(1, None))                  => RE.Rep(p.toRE, Quantifier.Plus)
      case Rep(p, Q(0, Some(1)))               => RE.Rep(p.toRE, Quantifier.Question)
      case Rep(p, Q(n1, Some(n2))) if n1 == n2 => RE.Rep(p.toRE, Quantifier.Exact(n1))
      case Rep(p, Q(n1, Some(n2)))             => RE.Rep(p.toRE, Quantifier.Bounded(n1, n2))
      case Rep(p, Q(n, None))                  => RE.Rep(p.toRE, Quantifier.Unbounded(n))

    override def toString: String = this match
      case Lit(c)       => s"P.Lit('$c')"
      case Assert(k)    => s"P.Assert($k)"
      case PosLA(p)     => s"P.PosLA($p)"
      case NegLA(p)     => s"P.NegLA($p)"
      case Cat(ps @ _*) => ps.mkString("P.Cat", ", ", ")")
      case Alt(ps @ _*) => ps.mkString("P.Alt", ", ", ")")
      case Rep(p, q)    => s"P.Rep($p, $q)"

  /** Q is a generic quantity specifier for repeats. */
  final case class Q(min: Int, max: Option[Int])

  /** A is an alternative form of regular expression. */
  final case class A(ts: T*):

    /** Combines with two alternative forms into one. */
    def |(that: A): A =
      if ts.isEmpty then that
      else if that.ts.isEmpty then this
      else
        (ts.last, that.ts.head) match
          case (T.NoCap(n1), T.NoCap(n2)) =>
            A(ts.init ++ Seq(T.NoCap(n1 | n2)) ++ that.ts.tail: _*)
          case _ => A(ts ++ that.ts: _*)

    override def toString: String = ts.mkString("A(", ", ", ")")

  object A:

    /** Selects any one of regular expressions contained in the given A, and returns a sequence containing regular
      * expressions concatenated with them.
      */
    def selects(as: Seq[A]): Seq[T] =
      as.foldLeft(Seq(T.Cat(): T))((ts, a) => ts.flatMap(t => a.ts.map(t ++ _)))

    /** Returns a set of capture index numbers in the given A. */
    def caps(a: A): Set[Int] = a.ts.iterator.flatMap(T.caps).toSet

    /** Returns a set of back-reference index numbers in the given A. */
    def refs(a: A): Set[Int] = a.ts.iterator.flatMap(T.refs).toSet

  /** S is a sequential form of regular expression. */
  final case class S(as: Seq[A], p: P):

    /** Concatenates with two sequential forms into one. */
    def ++(that: S): S =
      if p == P.Cat() then S(as ++ that.as, that.p)
      else S(as ++ Seq(A(T.NoCap(p.toN))) ++ that.as, that.p)

    /** Checks that this is pure. */
    def isPure: Boolean = as.isEmpty

    /** Converts this into an alternative form. */
    def toA: A =
      val ts = A.selects(as)
      A(ts.map(_ ++ T.NoCap(p.toN)): _*)

    override def toString: String = s"S(${as.mkString("Seq(", ", ", ")")}, $p)"

  object S:

    /** Returns a set of back-reference index numbers in the given S. */
    def refs(s: S): Set[Int] = s.as.iterator.flatMap(A.refs).toSet

  /** E is a wrapper for P with emptiness information. */
  enum E:
    case Empty(p: P)
    case NonEmpty(p: P)

    override def toString: String = this match
      case Empty(p)    => s"E.Empty($p)"
      case NonEmpty(p) => s"E.NonEmpty($p)"

  object E:

    /** Combines two alternatives of E into one. */
    def alt(es1: Seq[E], es2: Seq[E]): Seq[E] =
      (es1.lastOption, es2.headOption) match
        case (Some(Empty(p1)), Some(Empty(p2))) =>
          es1.init ++ Seq(Empty(p1 | p2)) ++ es2.tail
        case (Some(NonEmpty(p1)), Some(NonEmpty(p2))) =>
          es1.init ++ Seq(NonEmpty(p1 | p2)) ++ es2.tail
        case (_, _) => es1 ++ es2

    /** Concatenates two alternatives of E into one. */
    def cat(es1: Seq[E], es2: Seq[E]): Seq[E] =
      val es = for
        e1 <- es1
        e2 <- es2
      yield (e1, e2) match
        case (Empty(p1), Empty(p2))       => Empty(p1 ++ p2)
        case (Empty(p1), NonEmpty(p2))    => NonEmpty(p1 ++ p2)
        case (NonEmpty(p1), Empty(p2))    => NonEmpty(p1 ++ p2)
        case (NonEmpty(p1), NonEmpty(p2)) => NonEmpty(p1 ++ p2)
      es.foldLeft(Seq.empty[E]) {
        case (Seq(), e)                         => Seq(e)
        case (es :+ Empty(p1), Empty(p2))       => es :+ Empty(p1 | p2)
        case (es :+ NonEmpty(p1), NonEmpty(p2)) => es :+ NonEmpty(p1 | p2)
        case (es, e)                            => es :+ e
      }

    /** Like `alt2`, but the first argument is `Seq[P]`. */
    private def alt2(ps: Seq[P], es: Seq[E]): Seq[E] = ps match
      case Seq()  => es
      case Seq(p) => alt(Seq(NonEmpty(p)), es)
      case ps     => alt(Seq(NonEmpty(P.Alt(ps: _*))), es)

    /** Returns a sequence of regular expressions separating empty and non-empty parts. */
    def separate(p: P): Seq[E] = p match
      case P.Lit(c)    => Seq(NonEmpty(P.Lit(c)))
      case P.Assert(k) => Seq(Empty(P.Assert(k)))
      case P.PosLA(p)  => Seq(Empty(P.PosLA(p)))
      case P.NegLA(p)  => Seq(Empty(P.NegLA(p)))
      case P.Cat(ps @ _*) =>
        ps.foldLeft(Seq(Empty(P.Cat()): E))((es, p) => cat(es, separate(p)))
      case P.Alt(ps @ _*) =>
        ps.foldLeft(Seq.empty[E])((es, p) => alt(es, separate(p)))
      case P.Rep(p, q) =>
        val es = separate(p)
        q match
          case Q(0, Some(0)) => Seq(Empty(P.Cat()))
          case Q(0, Some(n)) =>
            val ps = es.collect { case NonEmpty(p1) => P.Rep(p, Q(0, Some(n - 1))) ++ p1 }
            alt2(ps, Seq(NonEmpty(P.Cat())))
          case Q(n1, Some(n2)) if n1 == n2 =>
            es.map {
              case Empty(p1)    => Empty(P.Rep(p, Q(n1 - 1, Some(n2 - 1))) ++ p1)
              case NonEmpty(p1) => NonEmpty(P.Rep(p, Q(n1 - 1, Some(n2 - 1))) ++ p1)
            }
          case Q(n1, Some(n2)) =>
            val ps = es.collect { case NonEmpty(p1) => P.Rep(p, Q(n1, Some(n2 - 1))) ++ p1 }
            val es1 = es.map {
              case Empty(p1)    => Empty(P.Rep(p, Q(n1 - 1, Some(n1 - 1))) ++ p1)
              case NonEmpty(p1) => NonEmpty(P.Rep(p, Q(n1 - 1, Some(n1 - 1))) ++ p1)
            }
            alt2(ps, es1)
          case Q(0, None) =>
            val ps = es.collect { case NonEmpty(p1) => P.Rep(p, Q(0, None)) ++ p1 }
            alt2(ps, Seq(NonEmpty(P.Cat())))
          case Q(n, None) =>
            val ps = es.collect { case NonEmpty(p1) => P.Rep(p, Q(n, None)) ++ p1 }
            val es1 = es.map {
              case Empty(p1)    => Empty(P.Rep(p, Q(n - 1, Some(n - 1))) ++ p1)
              case NonEmpty(p1) => NonEmpty(P.Rep(p, Q(n - 1, Some(n - 1))) ++ p1)
            }
            alt2(ps, es1)

  /** Returns a language (a word set) of the given BRE on the given continuations.
    *
    * Note that the given BRE should have finite language. In other words, it must not contain a star.
    */
  def language(b: BRE, ks: Seq[BRE]): Seq[W] = b match
    case BRE.Lit(c)    => Seq(W(C.Lit(c)))
    case BRE.Assert(k) => Seq(W(C.Assert(k)))
    case BRE.PosLA(b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)
      if (krefs & bcaps).nonEmpty then s.toA.ts.map(t => W(C.PosLA1(t)))
      else Seq(W(C.PosLA0(s)))
    case BRE.NegLA(b) => Seq(W(C.NegLA(convert(b, ks))))
    case BRE.Cat(bs @ _*) =>
      bs.zipWithIndex.foldLeft(Seq(W())) { case (ws, (b, i)) =>
        val ws1 = language(b, ks ++ bs.drop(i + 1))
        ws.flatMap(w => ws1.map(w ++ _))
      }
    case BRE.Alt(bs @ _*) => bs.flatMap(language(_, ks))
    case BRE.Rep(b, q) =>
      val ws = language(b, ks)
      (q.min, q.max) match
        case (n1, Some(n2)) =>
          (n2 to n1 by -1).iterator.flatMap(n => W.repeat(ws, n)).toSeq
        case (_, None) =>
          throw new IllegalArgumentException("Impossible to unref when capture has infinite repetition")
    case BRE.Cap(i, b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val ws = language(b, ks)
      if krefs.contains(i) then ws.map(w => W(C.Cap(i, w)))
      else ws
    case BRE.Ref(i) => Seq(W(C.Ref(i)))

  /** Converts the given BRE into a sequential form on the given continuations. */
  def convert(b: BRE, ks: Seq[BRE]): S = b match
    case BRE.Lit(c)    => S(Seq.empty, P.Lit(c))
    case BRE.Assert(k) => S(Seq.empty, P.Assert(k))
    case BRE.PosLA(b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)
      if (krefs & bcaps).nonEmpty then
        val a = s.toA
        S(Seq(A(a.ts.map(t => T.PosLA(t)): _*)), P.Cat())
      else if s.isPure then S(Seq.empty, P.PosLA(s.p))
      else S(Seq(A(T.NoCap(N.PosLA(s)))), P.Cat())
    case BRE.NegLA(b) =>
      // Captures in negative look-ahead are discarded, thus the continuations should be empty here.
      val s = convert(b, Seq.empty)
      if s.isPure then S(Seq.empty, P.NegLA(s.p))
      else S(Seq(A(T.NoCap(N.NegLA(s)))), P.Cat())
    case BRE.Cat(bs @ _*) =>
      val ss = bs.zipWithIndex.map { case (b, i) =>
        convert(b, bs.drop(i + 1) ++ ks)
      }
      ss.foldLeft(S(Seq.empty, P.Cat()))((s1, s2) => s1 ++ s2)
    case BRE.Alt(bs @ _*) =>
      val ss = bs.map(convert(_, ks))
      if ss.forall(_.isPure) then S(Seq.empty, P.Alt(ss.map(_.p): _*))
      else S(Seq(ss.foldLeft(A())((a, s) => a | s.toA)), P.Cat())
    case BRE.Rep(b, q) =>
      val min = q.min
      val max = q.max
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)

      if (krefs & bcaps).nonEmpty then
        // To simplify the repeating part of the resulting regular expression, we will perform
        // the transformation without passing a continuation. This is not a problem since the
        // capturing of the repeating part is never referenced outside.
        val s0 = convert(b, Seq.empty)
        val a = s.toA
        val ts = (min, max) match
          case (0, Some(0)) =>
            Seq.empty[T]
          case (0, Some(n)) =>
            a.ts.map(t => T.NoCap(N.Rep(s0, Q(0, Some(n - 1)))) ++ T.NonEmpty(t)) :+ T.Cat()
          case (n1, Some(n2)) if n1 == n2 =>
            a.ts.map(t => T.NoCap(N.Rep(s0, Q(n1 - 1, Some(n2 - 1)))) ++ t)
          case (n1, Some(n2)) =>
            val ts1 = a.ts.map(t => T.NoCap(N.Rep(s0, Q(n1, Some(n2 - 1)))) ++ T.NonEmpty(t))
            val ts2 = a.ts.map(t => T.NoCap(N.Rep(s0, Q(n1 - 1, Some(n1 - 1)))) ++ t)
            ts1 ++ ts2
          case (0, None) =>
            a.ts.map(t => T.NoCap(N.Rep(s0, Q(0, None))) ++ T.NonEmpty(t)) :+ T.Cat()
          case (n, None) =>
            val ts1 = a.ts.map(t => T.NoCap(N.Rep(s0, Q(n, None))) ++ T.NonEmpty(t))
            val ts2 = a.ts.map(t => T.NoCap(N.Rep(s0, Q(n - 1, Some(n - 1)))) ++ t)
            ts1 ++ ts2
        S(Seq(A(ts: _*)), P.Cat())
      else if s.isPure then S(Seq.empty, P.Rep(s.p, Q(min, max)))
      else S(Seq(A(T.NoCap(N.Rep(s, Q(min, max))))), P.Cat())
    case BRE.Cap(i, b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      if krefs.contains(i) then S(Seq(A(language(b, ks).map(T.Cap(i, _)): _*)), P.Cat())
      else convert(b, ks)
    case BRE.Ref(i) => S(Seq(A(T.NoCap(N.Ref(i)))), P.Cat())

  /** Returns a regular expression replaced back-references from the given S. */
  def exec(s: S, m: Map[Int, Seq[Char]]): P =
    // Extracts dependencies between captures and back-references. Finally, it creates the sequence
    // named `intervals`. Each element of them is a pair of index numbers on `s.as`: the first value
    // is an index capture is appeared, and the second is an index back-reference is appeared at last.
    val caps = s.as.zipWithIndex.flatMap { case (a, i) =>
      A.caps(a).map(_ -> i)
    }.toMap
    val refs = s.as.zipWithIndex.flatMap { case (a, i) =>
      A.refs(a).map(_ -> i)
    }.toMap
    val intervals = caps.toSeq.flatMap { case i -> x => refs.get(i).map((x, _)) }.sorted

    // Collapses overlapped intervals.
    val flatIntervals = intervals.foldLeft(Seq.empty[(Int, Int)]) {
      case (Seq(), (y1, y2))                                    => Seq((y1, y2))
      case (is :+ ((x1, x2)), (y1, y2)) if x1 <= y1 && y1 <= x2 => is :+ (x1, Math.max(x2, y2))
      case (is, (y1, y2))                                       => is :+ (y1, y2)
    }

    // Executes each collapsed intervals. Noting that intervals are independent on captures and back-references,
    // we do not need to take care of matches between intervals.
    val (_, p) = (flatIntervals.map(_._2 + 1) :+ s.as.size).foldLeft((0, P.Cat(): P)) { case ((x, p0), y) =>
      val as = s.as.slice(x, x + y - x)
      if as.nonEmpty then
        val (as1, as2) = as.span(_.ts.size == 1)
        val (p1, m1) = exec(as1.map(_.ts.head), m)

        if as2.isEmpty then (y, p0 ++ p1)
        else
          val ts = A.selects(as2)
          val ps = ts.map(t => exec(t, m1)._1).flatMap {
            case P.Alt(ps @ _*) => ps
            case p              => Seq(p)
          }
          (y, p0 ++ p1 ++ P.Alt(ps: _*))
      else (y, p0)
    }

    // Finally, it appends the last part of the sequence to the result.
    p ++ s.p

  /** Records captures in the given sequence of T, and returns a regular expression replaced back-references and the
    * matches.
    */
  def exec(ts: Seq[T], m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]]) =
    ts.foldLeft((P.Cat(): P, m)) { case ((p1, m1), t) =>
      val (p2, m2) = exec(t, m1)
      (p1 ++ p2, m2)
    }

  /** Records captures in the given T, and returns a regular expression replaced back-references and the matches. */
  def exec(t: T, m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]]) = t match
    case T.Cat(ts @ _*) => exec(ts, m)
    case T.PosLA(t) =>
      val (p, m1) = exec(t, m)
      (P.PosLA(p), m1)
    case T.NonEmpty(t) =>
      val (p, m1) = exec(t, m)
      val ps = E.separate(p).collect { case E.NonEmpty(p) => p }
      ps match
        case Seq(p) => (p, m1)
        case ps     => (P.Alt(ps: _*), m1)
    case T.NoCap(n) => (exec(n, m), m)
    case T.Cap(i, w) =>
      val (p, cs, m1) = exec(w, m)
      (p, m1 ++ Map(i -> cs))

  /** Returns a regular expression replaced back-references from the given N. */
  def exec(n: N, m: Map[Int, Seq[Char]]): P = n match
    case N.Lit(c)       => P.Lit(c)
    case N.Assert(k)    => P.Assert(k)
    case N.PosLA(s)     => P.PosLA(exec(s, m))
    case N.NegLA(s)     => P.NegLA(exec(s, m))
    case N.Cat(ns @ _*) => P.Cat(ns.map(exec(_, m)): _*)
    case N.Alt(ns @ _*) => P.Alt(ns.map(exec(_, m)): _*)
    case N.Rep(s, q)    => P.Rep(exec(s, m), q)
    case N.Ref(i) =>
      val cs = m.getOrElse(i, Seq.empty)
      cs match
        case Seq(c) => P.Lit(c)
        case _      => P.Cat(cs.map(P.Lit(_)): _*)

  /** Returns a regular expression replaces back-references, the matche, and the updated matches. */
  def exec(w: W, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]]) =
    w.cs.foldLeft((P.Cat(): P, Seq.empty[Char], m)) { case ((p1, cs1, m1), c) =>
      val (p2, cs2, m2) = exec(c, m1)
      (p1 ++ p2, cs1 ++ cs2, m2)
    }

  /** Returns a regular expression replaces back-references, the matche, and the updated matches. */
  def exec(c: C, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]]) = c match
    case C.Lit(c)    => (P.Lit(c), Seq(c), m)
    case C.Assert(k) => (P.Assert(k), Seq.empty, m)
    case C.PosLA0(s) => (P.PosLA(exec(s, m)), Seq.empty, m)
    case C.PosLA1(t) =>
      val (p, m1) = exec(t, m)
      (P.PosLA(p), Seq.empty, m1)
    case C.NegLA(s) => (P.NegLA(exec(s, m)), Seq.empty, m)
    case C.Scoped(w) =>
      val (p, cs, _) = exec(w, m)
      (p, cs, m)
    case C.Cap(i, w) =>
      val (p, cs, m1) = exec(w, m)
      (p, cs, m1 ++ Map(i -> cs))
    case C.Ref(i) =>
      val cs = m.getOrElse(i, Seq.empty)
      cs match
        case Seq(c) => (P.Lit(c), cs, m)
        case _      => (P.Cat(cs.map(P.Lit(_)): _*), cs, m)

  /** Returns a regular expression resolved back-references. */
  def apply(b: BRE): RE =
    val s = convert(b, Seq.empty)
    exec(s, Map.empty).toRE
