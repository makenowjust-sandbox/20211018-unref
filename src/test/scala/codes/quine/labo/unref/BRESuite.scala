package codes.quine.labo.unref

class BRESuite extends munit.FunSuite {
  test("BRE.parse") {
    assertEquals(BRE.parse(""), Some(BRE.Cat()))
    assertEquals(BRE.parse("x"), Some(BRE.Lit('x')))
    assertEquals(BRE.parse("\\\\"), Some(BRE.Lit('\\')))
    assertEquals(BRE.parse("\\1"), Some(BRE.Ref(1)))
    assertEquals(BRE.parse("\\"), None)
    assertEquals(BRE.parse("x|y"), Some(BRE.Alt(BRE.Lit('x'), BRE.Lit('y'))))
    assertEquals(BRE.parse("xy"), Some(BRE.Cat(BRE.Lit('x'), BRE.Lit('y'))))
    assertEquals(BRE.parse("x*"), Some(BRE.Star(BRE.Lit('x'))))
    assertEquals(BRE.parse("(?:xy)"), Some(BRE.Cat(BRE.Lit('x'), BRE.Lit('y'))))
    assertEquals(BRE.parse("(?:"), None)
    assertEquals(BRE.parse("(xy)"), Some(BRE.Cap(1, BRE.Cat(BRE.Lit('x'), BRE.Lit('y')))))
    assertEquals(BRE.parse("("), None)
  }
}
