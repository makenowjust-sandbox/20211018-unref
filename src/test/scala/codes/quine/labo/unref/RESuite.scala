package codes.quine.labo.unref

class RESuite extends munit.FunSuite:
  test("RE#toString") {
    assertEquals(RE.Lit('x').toString, "x")
    assertEquals(RE.Lit('\\').toString, "\\\\")
    assertEquals(RE.Assert(AssertKind.InputBegin).toString, "^")
    assertEquals(RE.Assert(AssertKind.InputEnd).toString, "$")
    assertEquals(RE.Assert(AssertKind.WordBoundary).toString, "\\b")
    assertEquals(RE.Assert(AssertKind.NotWordBoundary).toString, "\\B")
    assertEquals(RE.Cat().toString, "")
    assertEquals(RE.Cat(RE.Lit('x'), RE.Lit('y')).toString, "xy")
    assertEquals(RE.Cat(RE.Cat(RE.Lit('x'), RE.Lit('y')), RE.Lit('z')).toString, "(?:xy)z")
    assertEquals(RE.Cat(RE.Alt(RE.Lit('x'), RE.Lit('y')), RE.Lit('z')).toString, "(?:x|y)z")
    assertEquals(RE.Alt().toString, "(?!)")
    assertEquals(RE.Alt(RE.Lit('x'), RE.Lit('y')).toString, "x|y")
    assertEquals(RE.Alt(RE.Alt(RE.Lit('x'), RE.Lit('y')), RE.Lit('z')).toString, "(?:x|y)|z")
    assertEquals(RE.PosLA(RE.Lit('x')).toString, "(?=x)")
    assertEquals(RE.NegLA(RE.Lit('x')).toString, "(?!x)")
    assertEquals(RE.Star(RE.Lit('x')).toString, "x*")
    assertEquals(RE.Star(RE.Cat(RE.Lit('x'), RE.Lit('y'))).toString, "(?:xy)*")
    assertEquals(RE.Star(RE.Alt(RE.Lit('x'), RE.Lit('y'))).toString, "(?:x|y)*")
    assertEquals(RE.Star(RE.Star(RE.Lit('x'))).toString, "(?:x*)*")
  }
