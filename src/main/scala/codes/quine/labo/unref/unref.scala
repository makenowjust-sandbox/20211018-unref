package codes.quine.labo.unref

/** unref is the function to return a regular expression resolved back-references. */
object unref extends (BRE => RE) {

  /** C is a character in a capture.
    *
    * It is not an usual character because captures can be nested and back-references are not resolved here.
    */
  sealed abstract class C

  object C {
    final case class Lit(c: Char) extends C {
      override def toString: String = s"C.Lit('$c')"
    }
    final case class Assert(k: AssertKind) extends C {
      override def toString: String = s"C.Assert($k)"
    }
    final case class PosLA0(s: S) extends C {
      override def toString: String = s"C.PosLA0($s)"
    }
    final case class PosLA1(t: T) extends C {
      override def toString: String = s"C.PosLA1($t)"
    }
    final case class NegLA(s: S) extends C {
      override def toString: String = s"C.NegLA($s)"
    }
    final case class Cap(i: Int, w: W) extends C {
      override def toString: String = s"C.Cap($i, $w)"
    }
    final case class Ref(i: Int) extends C {
      override def toString: String = s"C.Ref($i)"
    }

    /** Returns a set of capture index numbers in the given capture character. */
    def caps(c: C): Set[Int] = c match {
      case Lit(_)    => Set.empty
      case Assert(_) => Set.empty
      case PosLA0(_) => Set.empty
      case PosLA1(t) => T.caps(t)
      case NegLA(_)  => Set.empty
      case Cap(i, w) => Set(i) | W.caps(w)
      case Ref(_)    => Set.empty
    }

    /** Returns a set of back-reference index numbers in the given capture character. */
    def refs(c: C): Set[Int] = c match {
      case Lit(_)    => Set.empty
      case Assert(_) => Set.empty
      case PosLA0(s) => S.refs(s)
      case PosLA1(t) => T.refs(t)
      case NegLA(s)  => S.refs(s)
      case Cap(_, w) => W.caps(w)
      case Ref(i)    => Set(i)
    }
  }

  /** W is a word of a capture. */
  final case class W(cs: C*) {

    /** Concatenates with two words into one. */
    def ++(w: W): W = W(cs ++ w.cs: _*)

    override def toString: String = cs.mkString("W(", ", ", ")")
  }

  object W {

    /** Returns a set of capture index numbers in the given capture word. */
    def caps(w: W): Set[Int] = w.cs.iterator.flatMap(C.caps).toSet

    /** Returns a set of back-reference index numbers in the given capture word. */
    def refs(w: W): Set[Int] = w.cs.iterator.flatMap(C.refs).toSet
  }

  /** T is a regular expression having no capture ambiguity. */
  sealed abstract class T {

    /** Concatenates with two regular expressions into sequential one. */
    def ++(that: T): T = (this, that) match {
      case (T.Cat(), t)                       => t
      case (t, T.Cat())                       => t
      case (T.Cat(ts1 @ _*), T.Cat(ts2 @ _*)) => T.Cat(ts1 ++ ts2: _*)
      case (T.Cat(ts @ _*), t)                => T.Cat(ts :+ t: _*)
      case (t, T.Cat(ts @ _*))                => T.Cat(t +: ts: _*)
      case (T.NoCap(n1), T.NoCap(n2))         => T.NoCap(n1 ++ n2)
      case (t1, t2)                           => T.Cat(t1, t2)
    }
  }

  object T {
    final case class Cat(ts: T*) extends T {
      override def toString: String = ts.mkString("T.Cat(", ", ", ")")
    }
    final case class PosLA(t: T) extends T {
      override def toString: String = s"T.PosLA($t)"
    }
    final case class NonEmpty(t: T) extends T {
      override def toString: String = s"T.NonEmpty($t)"
    }
    final case class NoCap(n: N) extends T {
      override def toString: String = s"T.NoCap($n)"
    }
    final case class Cap(i: Int, w: W) extends T {
      override def toString: String = s"T.Cap($i, $w)"
    }

    /** Returns a set of capture index numbers in the given T. */
    def caps(t: T): Set[Int] = t match {
      case Cat(ts @ _*) => ts.iterator.flatMap(caps).toSet
      case PosLA(t)     => caps(t)
      case NonEmpty(t)  => caps(t)
      case NoCap(_)     => Set.empty
      case Cap(i, w)    => Set(i) | W.caps(w)
    }

    /** Returns a set of back-reference index numbers in the given T. */
    def refs(t: T): Set[Int] = t match {
      case Cat(ts @ _*) => ts.iterator.flatMap(refs).toSet
      case PosLA(t)     => refs(t)
      case NonEmpty(t)  => refs(t)
      case NoCap(n)     => N.refs(n)
      case Cap(_, w)    => W.refs(w)
    }
  }

  /** N is a regular expression. It looks like BRE, but it has no capture. */
  sealed abstract class N {

    /** Concatenates with two regular expressions into sequential one. */
    def ++(that: N): N = (this, that) match {
      case (N.Cat(), n)                       => n
      case (n, N.Cat())                       => n
      case (N.Cat(ns1 @ _*), N.Cat(ns2 @ _*)) => N.Cat(ns1 ++ ns2: _*)
      case (N.Cat(ns @ _*), n)                => N.Cat(ns :+ n: _*)
      case (n, N.Cat(ns @ _*))                => N.Cat(n +: ns: _*)
      case (n1, n2)                           => N.Cat(n1, n2)
    }

    /** Combines with two regular expressions into alternative one. */
    def |(that: N): N = (this, that) match {
      case (N.Alt(), n)                       => n
      case (n, N.Alt())                       => n
      case (N.Alt(ns1 @ _*), N.Alt(ns2 @ _*)) => N.Alt(ns1 ++ ns2: _*)
      case (N.Alt(ns @ _*), n)                => N.Alt(ns :+ n: _*)
      case (n, N.Alt(ns @ _*))                => N.Alt(n +: ns: _*)
      case (n1, n2)                           => N.Alt(n1, n2)
    }
  }

  object N {
    final case class Lit(c: Char) extends N {
      override def toString: String = s"N.Lit('$c')"
    }
    final case class Assert(k: AssertKind) extends N {
      override def toString: String = s"N.Assert($k)"
    }
    final case class PosLA(s: S) extends N {
      override def toString: String = s"N.PosLA($s)"
    }
    final case class NegLA(s: S) extends N {
      override def toString: String = s"N.NegLA($s)"
    }
    final case class Cat(ns: N*) extends N {
      override def toString: String = ns.mkString("N.Cat(", ", ", ")")
    }
    final case class Alt(ns: N*) extends N {
      override def toString: String = ns.mkString("N.Alt(", ", ", ")")
    }
    final case class Star(s: S) extends N {
      override def toString: String = s"N.Star($s)"
    }
    final case class Ref(i: Int) extends N {
      override def toString: String = s"N.Ref($i)"
    }

    /** Returns a set of back-reference index numbers in the given N. */
    def refs(n: N): Set[Int] = n match {
      case Lit(_)       => Set.empty
      case Assert(_)    => Set.empty
      case PosLA(s)     => S.refs(s)
      case NegLA(s)     => S.refs(s)
      case Cat(ns @ _*) => ns.iterator.flatMap(refs).toSet
      case Alt(ns @ _*) => ns.iterator.flatMap(refs).toSet
      case Star(s)      => S.refs(s)
      case Ref(i)       => Set(i)
    }
  }

  /** P is a pure regular expression.
    *
    * Note that it is the same structure as RE.
    */
  sealed abstract class P {

    /** Concatenates with two pure regular expressions into one. */
    def ++(that: P): P = (this, that) match {
      case (P.Alt(), _)                       => P.Alt()
      case (_, P.Alt())                       => P.Alt()
      case (P.Cat(), p)                       => p
      case (p, P.Cat())                       => p
      case (P.Cat(ps1 @ _*), P.Cat(ps2 @ _*)) => P.Cat(ps1 ++ ps2: _*)
      case (P.Cat(ps @ _*), p)                => P.Cat(ps :+ p: _*)
      case (p, P.Cat(ps @ _*))                => P.Cat(p +: ps: _*)
      case (p1, p2)                           => P.Cat(p1, p2)
    }

    /** Combines with two pure regular expressions into one. */
    def |(that: P): P = (this, that) match {
      case (P.Alt(), p)                       => p
      case (p, P.Alt())                       => p
      case (P.Alt(ps1 @ _*), P.Alt(ps2 @ _*)) => P.Alt(ps1 ++ ps2: _*)
      case (P.Alt(ps @ _*), p)                => P.Alt(ps :+ p: _*)
      case (p, P.Alt(ps @ _*))                => P.Alt(p +: ps: _*)
      case (p1, p2)                           => P.Alt(p1, p2)
    }

    /** Converts this into N. */
    def toN: N = this match {
      case P.Lit(c)       => N.Lit(c)
      case P.Assert(k)    => N.Assert(k)
      case P.PosLA(p)     => N.PosLA(S(Seq.empty, p))
      case P.NegLA(p)     => N.NegLA(S(Seq.empty, p))
      case P.Cat(ps @ _*) => N.Cat(ps.map(_.toN): _*)
      case P.Alt(ps @ _*) => N.Alt(ps.map(_.toN): _*)
      case P.Star(p)      => N.Star(S(Seq.empty, p))
    }

    /** Converts this into RE. */
    def toRE: RE = this match {
      case P.Lit(c)       => RE.Lit(c)
      case P.Assert(k)    => RE.Assert(k)
      case P.PosLA(p)     => RE.PosLA(p.toRE)
      case P.NegLA(p)     => RE.NegLA(p.toRE)
      case P.Cat(ps @ _*) => RE.Cat(ps.map(_.toRE): _*)
      case P.Alt(ps @ _*) => RE.Alt(ps.map(_.toRE): _*)
      case P.Star(p)      => RE.Star(p.toRE)
    }
  }

  object P {
    final case class Lit(c: Char) extends P {
      override def toString: String = s"P.Lit('$c')"
    }
    final case class Assert(k: AssertKind) extends P {
      override def toString: String = s"P.Assert($k)"
    }
    final case class PosLA(p: P) extends P {
      override def toString: String = s"P.PosLA($p)"
    }
    final case class NegLA(p: P) extends P {
      override def toString: String = s"P.NegLA($p)"
    }
    final case class Cat(ps: P*) extends P {
      override def toString: String = ps.mkString("P.Cat(", ", ", ")")
    }
    final case class Alt(ps: P*) extends P {
      override def toString: String = ps.mkString("P.Alt(", ", ", ")")
    }
    final case class Star(p: P) extends P {
      override def toString: String = s"P.Star($p)"
    }
  }

  /** A is an alternative form of regular expression. */
  final case class A(ts: T*) {

    /** Combines with two alternative forms into one. */
    def |(that: A): A =
      if (ts.isEmpty) that
      else if (that.ts.isEmpty) this
      else {
        (ts.last, that.ts.head) match {
          case (T.NoCap(n1), T.NoCap(n2)) =>
            A(ts.init ++ Seq(T.NoCap(n1 | n2)) ++ that.ts.tail: _*)
          case _ => A(ts ++ that.ts: _*)
        }
      }

    override def toString: String = ts.mkString("A(", ", ", ")")
  }

  object A {

    /** Selects any one of regular expressions contained in the given A, and returns a sequence containing regular
      * expressions concatenated with them.
      */
    def selects(as: Seq[A]): Seq[T] =
      as.foldLeft(Seq(T.Cat(): T))((ts, a) => ts.flatMap(t => a.ts.map(t ++ _)))

    /** Returns a set of capture index numbers in the given A. */
    def caps(a: A): Set[Int] = a.ts.iterator.flatMap(T.caps).toSet

    /** Returns a set of back-reference index numbers in the given A. */
    def refs(a: A): Set[Int] = a.ts.iterator.flatMap(T.refs).toSet
  }

  /** S is a sequential form of regular expression. */
  final case class S(as: Seq[A], p: P) {

    /** Concatenates with two sequential forms into one. */
    def ++(that: S): S =
      if (p == P.Cat()) S(as ++ that.as, that.p)
      else S(as ++ Seq(A(T.NoCap(p.toN))) ++ that.as, that.p)

    /** Checks that this is pure. */
    def isPure: Boolean = as.isEmpty

    /** Converts this into an alternative form. */
    def toA: A = {
      val ts = A.selects(as)
      A(ts.map(_ ++ T.NoCap(p.toN)): _*)
    }

    override def toString: String = s"S(${as.mkString("Seq(", ", ", ")")}, $p)"
  }

  object S {

    /** Returns a set of back-reference index numbers in the given S. */
    def refs(s: S): Set[Int] = s.as.iterator.flatMap(A.refs).toSet
  }

  /** E is a wrapper for P with emptiness information. */
  sealed abstract class E

  object E {
    final case class Empty(p: P) extends E {
      override def toString: String = s"E.Empty($p)"
    }
    final case class NonEmpty(p: P) extends E {
      override def toString: String = s"E.NonEmpty($p)"
    }

    /** Combines two alternatives of E into one. */
    def alt(es1: Seq[E], es2: Seq[E]): Seq[E] =
      (es1.lastOption, es2.headOption) match {
        case (Some(Empty(p1)), Some(Empty(p2))) =>
          es1.init ++ Seq(Empty(p1 | p2)) ++ es2.tail
        case (Some(NonEmpty(p1)), Some(NonEmpty(p2))) =>
          es1.init ++ Seq(NonEmpty(p1 | p2)) ++ es2.tail
        case (_, _) => es1 ++ es2
      }

    /** Concatenates two alternatives of E into one. */
    def cat(es1: Seq[E], es2: Seq[E]): Seq[E] = {
      val es = for {
        e1 <- es1
        e2 <- es2
      } yield (e1, e2) match {
        case (Empty(p1), Empty(p2))       => Empty(p1 ++ p2)
        case (Empty(p1), NonEmpty(p2))    => NonEmpty(p1 ++ p2)
        case (NonEmpty(p1), Empty(p2))    => NonEmpty(p1 ++ p2)
        case (NonEmpty(p1), NonEmpty(p2)) => NonEmpty(p1 ++ p2)
      }
      es.foldLeft(Seq.empty[E]) {
        case (Seq(), e)                         => Seq(e)
        case (es :+ Empty(p1), Empty(p2))       => es :+ Empty(p1 | p2)
        case (es :+ NonEmpty(p1), NonEmpty(p2)) => es :+ NonEmpty(p1 | p2)
        case (es, e)                            => es :+ e
      }
    }

    /** Returns a sequence of regular expressions separating empty and non-empty parts. */
    def separate(p: P): Seq[E] = p match {
      case P.Lit(c)    => Seq(NonEmpty(P.Lit(c)))
      case P.Assert(k) => Seq(Empty(P.Assert(k)))
      case P.PosLA(p)  => Seq(Empty(P.PosLA(p)))
      case P.NegLA(p)  => Seq(Empty(P.NegLA(p)))
      case P.Cat(ps @ _*) =>
        ps.foldLeft(Seq(Empty(P.Cat()): E))((es, p) => cat(es, separate(p)))
      case P.Alt(ps @ _*) =>
        ps.foldLeft(Seq.empty[E])((es, p) => alt(es, separate(p)))
      case P.Star(p) =>
        val es = separate(p)
        val ps = es.collect { case NonEmpty(p1) => P.Star(p) ++ p1 }
        ps match {
          case Seq()  => Seq(Empty(P.Cat()))
          case Seq(p) => Seq(NonEmpty(p), Empty(P.Cat()))
          case ps     => Seq(NonEmpty(P.Alt(ps: _*)), Empty(P.Cat()))
        }
    }
  }

  /** Returns a language (a word set) of the given BRE on the given continuations.
    *
    * Note that the given BRE should have finite language. In other words, it must not contain a star.
    */
  def language(b: BRE, ks: Seq[BRE]): Seq[W] = b match {
    case BRE.Lit(c)    => Seq(W(C.Lit(c)))
    case BRE.Assert(k) => Seq(W(C.Assert(k)))
    case BRE.PosLA(b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)
      if ((krefs & bcaps).nonEmpty) s.toA.ts.map(t => W(C.PosLA1(t)))
      else Seq(W(C.PosLA0(s)))
    case BRE.NegLA(b) => Seq(W(C.NegLA(convert(b, ks))))
    case BRE.Cat(bs @ _*) =>
      bs.zipWithIndex.foldLeft(Seq(W())) { case (ws, (b, i)) =>
        val ws1 = language(b, ks ++ bs.drop(i + 1))
        ws.flatMap(w => ws1.map(w ++ _))
      }
    case BRE.Alt(bs @ _*) => bs.flatMap(language(_, ks))
    case BRE.Star(_)      => throw new IllegalArgumentException
    case BRE.Cap(i, b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val ws = language(b, ks)
      if (krefs.contains(i)) ws.map(w => W(C.Cap(i, w)))
      else ws
    case BRE.Ref(i) => Seq(W(C.Ref(i)))
  }

  /** Converts the given BRE into a sequential form on the given continuations. */
  def convert(b: BRE, ks: Seq[BRE]): S = b match {
    case BRE.Lit(c)    => S(Seq.empty, P.Lit(c))
    case BRE.Assert(k) => S(Seq.empty, P.Assert(k))
    case BRE.PosLA(b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)
      if ((krefs & bcaps).nonEmpty) {
        val a = s.toA
        S(Seq(A(a.ts.map(t => T.PosLA(t)): _*)), P.Cat())
      } else {
        if (s.isPure) S(Seq.empty, P.PosLA(s.p))
        else S(Seq(A(T.NoCap(N.PosLA(s)))), P.Cat())
      }
    case BRE.NegLA(b) =>
      // Captures in negative look-ahead are discarded, thus the continuations should be empty here.
      val s = convert(b, Seq.empty)
      if (s.isPure) S(Seq.empty, P.NegLA(s.p))
      else S(Seq(A(T.NoCap(N.NegLA(s)))), P.Cat())
    case BRE.Cat(bs @ _*) =>
      val ss = bs.zipWithIndex.map { case (b, i) =>
        convert(b, bs.drop(i + 1) ++ ks)
      }
      ss.foldLeft(S(Seq.empty, P.Cat()))((s1, s2) => s1 ++ s2)
    case BRE.Alt(bs @ _*) =>
      val ss = bs.map(convert(_, ks))
      if (ss.forall(_.isPure)) S(Seq.empty, P.Alt(ss.map(_.p): _*))
      else S(Seq(ss.foldLeft(A())((a, s) => a | s.toA)), P.Cat())
    case BRE.Star(b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      val bcaps = BRE.caps(b)
      val s = convert(b, ks)
      if ((krefs & bcaps).nonEmpty) {
        val a = A(s.toA.ts.map(t => T.NoCap(N.Star(s)) ++ T.NonEmpty(t)) :+ T.Cat(): _*)
        S(Seq(a), P.Cat())
      } else {
        if (s.isPure) S(Seq.empty, P.Star(s.p))
        else S(Seq(A(T.NoCap(N.Star(s)))), P.Cat())
      }
    case BRE.Cap(i, b) =>
      val krefs = ks.iterator.flatMap(BRE.refs).toSet
      if (krefs.contains(i))
        S(Seq(A(language(b, ks).map(T.Cap(i, _)): _*)), P.Cat())
      else convert(b, ks)
    case BRE.Ref(i) => S(Seq(A(T.NoCap(N.Ref(i)))), P.Cat())
  }

  /** Returns a regular expression replaced back-references from the given S. */
  def exec(s: S, m: Map[Int, Seq[Char]]): P = {
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
      if (as.nonEmpty) {
        val (as1, as2) = as.span(_.ts.size == 1)
        val (p1, m1) = exec(as1.map(_.ts.head), m)

        if (as2.isEmpty) (y, p0 ++ p1)
        else {
          val ts = A.selects(as2)
          val ps = ts.map(t => exec(t, m1)._1).flatMap {
            case P.Alt(ps @ _*) => ps
            case p              => Seq(p)
          }
          (y, p0 ++ p1 ++ P.Alt(ps: _*))
        }
      } else (y, p0)
    }

    p ++ s.p
  }

  /** Records captures in the given sequence of T, and returns a regular expression replaced back-references and the
    * matches.
    */
  def exec(ts: Seq[T], m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]]) =
    ts.foldLeft((P.Cat(): P, m)) { case ((p1, m1), t) =>
      val (p2, m2) = exec(t, m1)
      (p1 ++ p2, m2)
    }

  /** Records captures in the given T, and returns a regular expression replaced back-references and the matches. */
  def exec(t: T, m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]]) = t match {
    case T.Cat(ts @ _*) => exec(ts, m)
    case T.PosLA(t) =>
      val (p, m1) = exec(t, m)
      (P.PosLA(p), m1)
    case T.NonEmpty(t) =>
      val (p, m1) = exec(t, m)
      val ps = E.separate(p).collect { case E.NonEmpty(p) => p }
      ps match {
        case Seq(p) => (p, m1)
        case ps     => (P.Alt(ps: _*), m1)
      }
    case T.NoCap(n) => (exec(n, m), m)
    case T.Cap(i, w) =>
      val (p, cs, m1) = exec(w, m)
      (p, m1 ++ Map(i -> cs))
  }

  /** Returns a regular expression replaced back-references from the given N. */
  def exec(n: N, m: Map[Int, Seq[Char]]): P = n match {
    case N.Lit(c)       => P.Lit(c)
    case N.Assert(k)    => P.Assert(k)
    case N.PosLA(s)     => P.PosLA(exec(s, m))
    case N.NegLA(s)     => P.NegLA(exec(s, m))
    case N.Cat(ns @ _*) => P.Cat(ns.map(exec(_, m)): _*)
    case N.Alt(ns @ _*) => P.Alt(ns.map(exec(_, m)): _*)
    case N.Star(s)      => P.Star(exec(s, m))
    case N.Ref(i) =>
      val cs = m.getOrElse(i, Seq.empty)
      cs match {
        case Seq(c) => P.Lit(c)
        case _      => P.Cat(cs.map(P.Lit(_)): _*)
      }
  }

  /** Returns a regular expression replaces back-references, the matche, and the updated matches. */
  def exec(w: W, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]]) =
    w.cs.foldLeft((P.Cat(): P, Seq.empty[Char], m)) { case ((p1, cs1, m1), c) =>
      val (p2, cs2, m2) = exec(c, m1)
      (p1 ++ p2, cs1 ++ cs2, m2)
    }

  /** Returns a regular expression replaces back-references, the matche, and the updated matches. */
  def exec(c: C, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]]) =
    c match {
      case C.Lit(c)    => (P.Lit(c), Seq(c), m)
      case C.Assert(k) => (P.Assert(k), Seq.empty, m)
      case C.PosLA0(s) => (P.PosLA(exec(s, m)), Seq.empty, m)
      case C.PosLA1(t) =>
        val (p, m1) = exec(t, m)
        (P.PosLA(p), Seq.empty, m1)
      case C.NegLA(s) => (P.NegLA(exec(s, m)), Seq.empty, m)
      case C.Cap(i, w) =>
        val (p, cs, m1) = exec(w, m)
        (p, cs, m1 ++ Map(i -> cs))
      case C.Ref(i) =>
        val cs = m.getOrElse(i, Seq.empty)
        cs match {
          case Seq(c) => (P.Lit(c), cs, m)
          case _      => (P.Cat(cs.map(P.Lit(_)): _*), cs, m)
        }
    }

  /** Returns a regular expression resolved back-references. */
  def apply(b: BRE): RE = {
    val s = convert(b, Seq.empty)
    exec(s, Map.empty).toRE
  }
}
