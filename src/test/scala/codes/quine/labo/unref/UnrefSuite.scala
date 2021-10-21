package codes.quine.labo.unref

class UnrefSuite extends munit.FunSuite {
  def assertUnref(input: String, expected: String)(implicit loc: munit.Location): Unit = {
    val b = BRE.parse(input).getOrElse(throw new IllegalArgumentException(s"invalid BRE: /$input/"))
    val r = unref(b)
    assertEquals(r.toString, expected)
  }

  test("unref.apply") {
    assertUnref("""x""", "x")
    assertUnref("""(x)\1""", "xx")
    assertUnref("""(x)(0)\1\2""", "x0x0")
    assertUnref("""\1(x)""", "x")
    assertUnref("""(x|y)\1""", "xx|yy")
    assertUnref("""(x|y)\1z""", "(?:xx|yy)z")
    assertUnref("""z(x|y)\1""", "z(?:xx|yy)")
    assertUnref("""z(x|y)\1z(x|y)\2z""", "z(?:xx|yy)z(?:xx|yy)z")
    assertUnref("""(x|y)(0|1)\1\2""", "x0x0|x1x1|y0y0|y1y1")
    assertUnref("""(x|y|z)(0|1)\1\2""", "x0x0|x1x1|y0y0|y1y1|z0z0|z1z1")
    assertUnref("""(x|y)(0|1|2)\1\2""", "x0x0|x1x1|x2x2|y0y0|y1y1|y2y2")
    assertUnref("""(x|y)(0|1)\2\1""", "x00x|x11x|y00y|y11y")
    assertUnref("""(x|y)\1(z)\2""", "(?:xx|yy)zz")
    assertUnref("""(?:(x|y)\1)*""", "(?:xx|yy)*")
    assertUnref("""(x|y)*\1""", "(?:x|y)*xx|(?:x|y)*yy|")
    assertUnref("""((x)|y)*\1\2""", "(?:x|y)*xxx|(?:x|y)*yy|(?:)(?:)")
  }
}
