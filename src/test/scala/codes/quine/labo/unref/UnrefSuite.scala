package codes.quine.labo.unref

class UnrefSuite extends munit.FunSuite:
  def assertUnref(input: String, expected: String)(implicit loc: munit.Location): Unit =
    val b = BRE.parse(input).getOrElse(throw new IllegalArgumentException(s"invalid BRE: /$input/"))
    val r = unref(b)
    assertEquals(r.toString, expected)

  test("unref.apply") {
    assertUnref("""x""", "x")
    assertUnref("""\b""", "\\b")
    assertUnref("""(x\b )\1""", "x\\b x ")
    assertUnref("""(?=x)""", "(?=x)")
    assertUnref("""(?=(x))\1""", "(?=x)x")
    assertUnref("""(?=(x|y))\1""", "(?=x)x|(?=y)y")
    assertUnref("""(x(?=x)|y(?=y))\1""", "x(?=x)x|y(?=y)y")
    assertUnref("""((?:x|y)(?=(0|1)))\2\1""", "x(?=0)0x|x(?=1)1x|y(?=0)0y|y(?=1)1y")
    assertUnref("""(?!x)""", "(?!x)")
    assertUnref("""(?!(x))\1""", "(?!x)")
    assertUnref("""(?!(x|y))\1""", "(?!x|y)")
    assertUnref("""(x|y)(?!(0|1)\1)\2""", "x(?!(?:0|1)x)|y(?!(?:0|1)y)")
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
    assertUnref("""((x)|y)*\1\2""", "(?:x|y)*xxx|(?:x|y)*yy|")
    assertUnref("""(x|)*\1""", "(?:x|)*xx|")
    assertUnref("""(x|(?=x))*\1""", "(?:x|(?=x))*xx|")
    assertUnref("""(x|(?=x))*\1""", "(?:x|(?=x))*xx|")
    assertUnref("""(x|)(\1)*\2""", "xx*xx|x|")
    assertUnref("""(x|y)+\1""", "(?:x|y)+xx|(?:x|y)+yy|xx|yy")
    assertUnref("""(x|y|)+\1""", "(?:x|y)+xx|(?:x|y)+yy|xx|yy|")
    assertUnref("""(x|y)?\1""", "xx|yy|")
    assertUnref("""(x|y){2}\1""", "(?:x|y)xx|(?:x|y)yy")
    assertUnref("""(x|y){1,2}\1""", "(?:x|y)xx|(?:x|y)yy|xx|yy")
    assertUnref("""(x|y){2,}\1""", "(?:x|y){2,}xx|(?:x|y){2,}yy|(?:x|y)xx|(?:x|y)yy")
    assertUnref("""(x|y|){2,}\1""", "(?:x|y|){2,}xx|(?:x|y|){2,}yy|(?:x|y|)xx|(?:x|y|)yy|(?:x|y|)")
    assertUnref(
      """((?:(?:(x)|y)\2){2})*\1""",
      "(?:(?:xx|y){2})*xxxxxxxx|(?:(?:xx|y){2})*xxyxxy|(?:(?:xx|y){2})*yxxyxx|(?:(?:xx|y){2})*yyyy|"
    )
  }
