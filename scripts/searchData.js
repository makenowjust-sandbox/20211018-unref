pages = [{"l":"api/index.html","n":"unref","t":"unref","d":"","k":"static"},
{"l":"api/codes/quine/labo/unref.html","n":"codes.quine.labo.unref","t":"package codes.quine.labo.unref","d":"codes/quine/labo/unref","k":"package"},
{"l":"api/codes/quine/labo/unref/AssertKind.html","n":"AssertKind","t":"enum AssertKind","d":"codes/quine/labo/unref/AssertKind","k":"enum"},
{"l":"api/codes/quine/labo/unref/AssertKind.html","n":"InputBegin","t":"case InputBegin extends AssertKind","d":"codes/quine/labo/unref/AssertKind","k":"case"},
{"l":"api/codes/quine/labo/unref/AssertKind.html","n":"InputEnd","t":"case InputEnd extends AssertKind","d":"codes/quine/labo/unref/AssertKind","k":"case"},
{"l":"api/codes/quine/labo/unref/AssertKind.html","n":"WordBoundary","t":"case WordBoundary extends AssertKind","d":"codes/quine/labo/unref/AssertKind","k":"case"},
{"l":"api/codes/quine/labo/unref/AssertKind.html","n":"NotWordBoundary","t":"case NotWordBoundary extends AssertKind","d":"codes/quine/labo/unref/AssertKind","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE.html","n":"BRE","t":"enum BRE","d":"codes/quine/labo/unref/BRE","k":"enum"},
{"l":"api/codes/quine/labo/unref/BRE$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Cat.html","n":"Cat","t":"case Cat(bs: BRE*)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Alt.html","n":"Alt","t":"case Alt(bs: BRE*)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Star.html","n":"Star","t":"case Star(b: BRE)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$PosLA.html","n":"PosLA","t":"case PosLA(b: BRE)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$NegLA.html","n":"NegLA","t":"case NegLA(b: BRE)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Cap.html","n":"Cap","t":"case Cap(i: Int, b: BRE)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/BRE","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/BRE$$Lit","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/BRE$$Assert","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Cat.html","n":"Cat","t":"case Cat(bs: BRE*)","d":"codes/quine/labo/unref/BRE$$Cat","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Alt.html","n":"Alt","t":"case Alt(bs: BRE*)","d":"codes/quine/labo/unref/BRE$$Alt","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Star.html","n":"Star","t":"case Star(b: BRE)","d":"codes/quine/labo/unref/BRE$$Star","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$PosLA.html","n":"PosLA","t":"case PosLA(b: BRE)","d":"codes/quine/labo/unref/BRE$$PosLA","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$NegLA.html","n":"NegLA","t":"case NegLA(b: BRE)","d":"codes/quine/labo/unref/BRE$$NegLA","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Cap.html","n":"Cap","t":"case Cap(i: Int, b: BRE)","d":"codes/quine/labo/unref/BRE$$Cap","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/BRE$$Ref","k":"case"},
{"l":"api/codes/quine/labo/unref/BRE$.html","n":"BRE","t":"object BRE","d":"codes/quine/labo/unref/BRE$","k":"object"},
{"l":"api/codes/quine/labo/unref/BRE$.html","n":"caps","t":"def caps(b: BRE): Set[Int]","d":"codes/quine/labo/unref/BRE$","k":"def"},
{"l":"api/codes/quine/labo/unref/BRE$.html","n":"parse","t":"def parse(s: String): Option[BRE]","d":"codes/quine/labo/unref/BRE$","k":"def"},
{"l":"api/codes/quine/labo/unref/BRE$.html","n":"refs","t":"def refs(b: BRE): Set[Int]","d":"codes/quine/labo/unref/BRE$","k":"def"},
{"l":"api/codes/quine/labo/unref/RE.html","n":"RE","t":"enum RE","d":"codes/quine/labo/unref/RE","k":"enum"},
{"l":"api/codes/quine/labo/unref/RE$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Cat.html","n":"Cat","t":"case Cat(rs: RE*)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Alt.html","n":"Alt","t":"case Alt(rs: RE*)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$PosLA.html","n":"PosLA","t":"case PosLA(r: RE)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$NegLA.html","n":"NegLA","t":"case NegLA(r: RE)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Star.html","n":"Star","t":"case Star(r: RE)","d":"codes/quine/labo/unref/RE","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/RE$$Lit","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/RE$$Assert","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Cat.html","n":"Cat","t":"case Cat(rs: RE*)","d":"codes/quine/labo/unref/RE$$Cat","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Alt.html","n":"Alt","t":"case Alt(rs: RE*)","d":"codes/quine/labo/unref/RE$$Alt","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$PosLA.html","n":"PosLA","t":"case PosLA(r: RE)","d":"codes/quine/labo/unref/RE$$PosLA","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$NegLA.html","n":"NegLA","t":"case NegLA(r: RE)","d":"codes/quine/labo/unref/RE$$NegLA","k":"case"},
{"l":"api/codes/quine/labo/unref/RE$$Star.html","n":"Star","t":"case Star(r: RE)","d":"codes/quine/labo/unref/RE$$Star","k":"case"},
{"l":"api/codes/quine/labo/unref/Util$.html","n":"Util","t":"object Util","d":"codes/quine/labo/unref/Util$","k":"object"},
{"l":"api/codes/quine/labo/unref/Util$.html","n":"escape","t":"def escape(c: Char): String","d":"codes/quine/labo/unref/Util$","k":"def"},
{"l":"api/codes/quine/labo/unref/exports$.html","n":"exports","t":"object exports","d":"codes/quine/labo/unref/exports$","k":"object"},
{"l":"api/codes/quine/labo/unref/exports$.html","n":"unrefJS","t":"def unrefJS(s: String): String","d":"codes/quine/labo/unref/exports$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"unref","t":"object unref extends BRE => RE","d":"codes/quine/labo/unref/unref$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"apply","t":"def apply(b: BRE): RE","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"convert","t":"def convert(b: BRE, ks: Seq[BRE]): S","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(s: S, m: Map[Int, Seq[Char]]): P","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(ts: Seq[T], m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]])","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(t: T, m: Map[Int, Seq[Char]]): (P, Map[Int, Seq[Char]])","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(n: N, m: Map[Int, Seq[Char]]): P","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(w: W, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]])","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"exec","t":"def exec(c: C, m: Map[Int, Seq[Char]]): (P, Seq[Char], Map[Int, Seq[Char]])","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$.html","n":"language","t":"def language(b: BRE, ks: Seq[BRE]): Seq[W]","d":"codes/quine/labo/unref/unref$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$A.html","n":"A","t":"class A(ts: T*)","d":"codes/quine/labo/unref/unref$$A","k":"class"},
{"l":"api/codes/quine/labo/unref/unref$$A.html","n":"|","t":"def |(that: A): A","d":"codes/quine/labo/unref/unref$$A","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$A$.html","n":"A","t":"object A","d":"codes/quine/labo/unref/unref$$A$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$A$.html","n":"caps","t":"def caps(a: A): Set[Int]","d":"codes/quine/labo/unref/unref$$A$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$A$.html","n":"refs","t":"def refs(a: A): Set[Int]","d":"codes/quine/labo/unref/unref$$A$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$A$.html","n":"selects","t":"def selects(as: Seq[A]): Seq[T]","d":"codes/quine/labo/unref/unref$$A$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$C.html","n":"C","t":"enum C","d":"codes/quine/labo/unref/unref$$C","k":"enum"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$PosLA0.html","n":"PosLA0","t":"case PosLA0(s: S)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$PosLA1.html","n":"PosLA1","t":"case PosLA1(t: T)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$NegLA.html","n":"NegLA","t":"case NegLA(s: S)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Cap.html","n":"Cap","t":"case Cap(i: Int, w: W)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/unref$$C","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$C$$Lit","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$C$$Assert","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$PosLA0.html","n":"PosLA0","t":"case PosLA0(s: S)","d":"codes/quine/labo/unref/unref$$C$$PosLA0","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$PosLA1.html","n":"PosLA1","t":"case PosLA1(t: T)","d":"codes/quine/labo/unref/unref$$C$$PosLA1","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$NegLA.html","n":"NegLA","t":"case NegLA(s: S)","d":"codes/quine/labo/unref/unref$$C$$NegLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Cap.html","n":"Cap","t":"case Cap(i: Int, w: W)","d":"codes/quine/labo/unref/unref$$C$$Cap","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/unref$$C$$Ref","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$C$.html","n":"C","t":"object C","d":"codes/quine/labo/unref/unref$$C$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$C$.html","n":"caps","t":"def caps(c: C): Set[Int]","d":"codes/quine/labo/unref/unref$$C$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$C$.html","n":"refs","t":"def refs(c: C): Set[Int]","d":"codes/quine/labo/unref/unref$$C$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$E.html","n":"E","t":"enum E","d":"codes/quine/labo/unref/unref$$E","k":"enum"},
{"l":"api/codes/quine/labo/unref/unref$$E$$Empty.html","n":"Empty","t":"case Empty(p: P)","d":"codes/quine/labo/unref/unref$$E","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$E$$NonEmpty.html","n":"NonEmpty","t":"case NonEmpty(p: P)","d":"codes/quine/labo/unref/unref$$E","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$E$$Empty.html","n":"Empty","t":"case Empty(p: P)","d":"codes/quine/labo/unref/unref$$E$$Empty","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$E$$NonEmpty.html","n":"NonEmpty","t":"case NonEmpty(p: P)","d":"codes/quine/labo/unref/unref$$E$$NonEmpty","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$E$.html","n":"E","t":"object E","d":"codes/quine/labo/unref/unref$$E$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$E$.html","n":"alt","t":"def alt(es1: Seq[E], es2: Seq[E]): Seq[E]","d":"codes/quine/labo/unref/unref$$E$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$E$.html","n":"cat","t":"def cat(es1: Seq[E], es2: Seq[E]): Seq[E]","d":"codes/quine/labo/unref/unref$$E$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$E$.html","n":"separate","t":"def separate(p: P): Seq[E]","d":"codes/quine/labo/unref/unref$$E$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$N.html","n":"N","t":"enum N","d":"codes/quine/labo/unref/unref$$N","k":"enum"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$PosLA.html","n":"PosLA","t":"case PosLA(s: S)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$NegLA.html","n":"NegLA","t":"case NegLA(s: S)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Cat.html","n":"Cat","t":"case Cat(ns: N*)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Alt.html","n":"Alt","t":"case Alt(ns: N*)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Star.html","n":"Star","t":"case Star(s: S)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/unref$$N","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$N$$Lit","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$N$$Assert","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$PosLA.html","n":"PosLA","t":"case PosLA(s: S)","d":"codes/quine/labo/unref/unref$$N$$PosLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$NegLA.html","n":"NegLA","t":"case NegLA(s: S)","d":"codes/quine/labo/unref/unref$$N$$NegLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Cat.html","n":"Cat","t":"case Cat(ns: N*)","d":"codes/quine/labo/unref/unref$$N$$Cat","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Alt.html","n":"Alt","t":"case Alt(ns: N*)","d":"codes/quine/labo/unref/unref$$N$$Alt","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Star.html","n":"Star","t":"case Star(s: S)","d":"codes/quine/labo/unref/unref$$N$$Star","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$$Ref.html","n":"Ref","t":"case Ref(i: Int)","d":"codes/quine/labo/unref/unref$$N$$Ref","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$N$.html","n":"N","t":"object N","d":"codes/quine/labo/unref/unref$$N$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$N$.html","n":"refs","t":"def refs(n: N): Set[Int]","d":"codes/quine/labo/unref/unref$$N$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$P.html","n":"P","t":"enum P","d":"codes/quine/labo/unref/unref$$P","k":"enum"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$PosLA.html","n":"PosLA","t":"case PosLA(p: P)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$NegLA.html","n":"NegLA","t":"case NegLA(p: P)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Cat.html","n":"Cat","t":"case Cat(ps: P*)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Alt.html","n":"Alt","t":"case Alt(ps: P*)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Star.html","n":"Star","t":"case Star(p: P)","d":"codes/quine/labo/unref/unref$$P","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Lit.html","n":"Lit","t":"case Lit(c: Char)","d":"codes/quine/labo/unref/unref$$P$$Lit","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Assert.html","n":"Assert","t":"case Assert(k: AssertKind)","d":"codes/quine/labo/unref/unref$$P$$Assert","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$PosLA.html","n":"PosLA","t":"case PosLA(p: P)","d":"codes/quine/labo/unref/unref$$P$$PosLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$NegLA.html","n":"NegLA","t":"case NegLA(p: P)","d":"codes/quine/labo/unref/unref$$P$$NegLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Cat.html","n":"Cat","t":"case Cat(ps: P*)","d":"codes/quine/labo/unref/unref$$P$$Cat","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Alt.html","n":"Alt","t":"case Alt(ps: P*)","d":"codes/quine/labo/unref/unref$$P$$Alt","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$P$$Star.html","n":"Star","t":"case Star(p: P)","d":"codes/quine/labo/unref/unref$$P$$Star","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$S.html","n":"S","t":"class S(as: Seq[A], p: P)","d":"codes/quine/labo/unref/unref$$S","k":"class"},
{"l":"api/codes/quine/labo/unref/unref$$S.html","n":"++","t":"def ++(that: S): S","d":"codes/quine/labo/unref/unref$$S","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$S.html","n":"isPure","t":"def isPure: Boolean","d":"codes/quine/labo/unref/unref$$S","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$S.html","n":"toA","t":"def toA: A","d":"codes/quine/labo/unref/unref$$S","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$S$.html","n":"S","t":"object S","d":"codes/quine/labo/unref/unref$$S$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$S$.html","n":"refs","t":"def refs(s: S): Set[Int]","d":"codes/quine/labo/unref/unref$$S$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$T.html","n":"T","t":"enum T","d":"codes/quine/labo/unref/unref$$T","k":"enum"},
{"l":"api/codes/quine/labo/unref/unref$$T$$Cat.html","n":"Cat","t":"case Cat(ts: T*)","d":"codes/quine/labo/unref/unref$$T","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$PosLA.html","n":"PosLA","t":"case PosLA(t: T)","d":"codes/quine/labo/unref/unref$$T","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$NonEmpty.html","n":"NonEmpty","t":"case NonEmpty(t: T)","d":"codes/quine/labo/unref/unref$$T","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$NoCap.html","n":"NoCap","t":"case NoCap(n: N)","d":"codes/quine/labo/unref/unref$$T","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$Cap.html","n":"Cap","t":"case Cap(i: Int, w: W)","d":"codes/quine/labo/unref/unref$$T","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$Cat.html","n":"Cat","t":"case Cat(ts: T*)","d":"codes/quine/labo/unref/unref$$T$$Cat","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$PosLA.html","n":"PosLA","t":"case PosLA(t: T)","d":"codes/quine/labo/unref/unref$$T$$PosLA","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$NonEmpty.html","n":"NonEmpty","t":"case NonEmpty(t: T)","d":"codes/quine/labo/unref/unref$$T$$NonEmpty","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$NoCap.html","n":"NoCap","t":"case NoCap(n: N)","d":"codes/quine/labo/unref/unref$$T$$NoCap","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$$Cap.html","n":"Cap","t":"case Cap(i: Int, w: W)","d":"codes/quine/labo/unref/unref$$T$$Cap","k":"case"},
{"l":"api/codes/quine/labo/unref/unref$$T$.html","n":"T","t":"object T","d":"codes/quine/labo/unref/unref$$T$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$T$.html","n":"caps","t":"def caps(t: T): Set[Int]","d":"codes/quine/labo/unref/unref$$T$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$T$.html","n":"refs","t":"def refs(t: T): Set[Int]","d":"codes/quine/labo/unref/unref$$T$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$W.html","n":"W","t":"class W(cs: C*)","d":"codes/quine/labo/unref/unref$$W","k":"class"},
{"l":"api/codes/quine/labo/unref/unref$$W.html","n":"++","t":"def ++(w: W): W","d":"codes/quine/labo/unref/unref$$W","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$W$.html","n":"W","t":"object W","d":"codes/quine/labo/unref/unref$$W$","k":"object"},
{"l":"api/codes/quine/labo/unref/unref$$W$.html","n":"caps","t":"def caps(w: W): Set[Int]","d":"codes/quine/labo/unref/unref$$W$","k":"def"},
{"l":"api/codes/quine/labo/unref/unref$$W$.html","n":"refs","t":"def refs(w: W): Set[Int]","d":"codes/quine/labo/unref/unref$$W$","k":"def"},
{"l":"docs/index.html","n":"unref","t":"unref","d":"","k":"static"}];