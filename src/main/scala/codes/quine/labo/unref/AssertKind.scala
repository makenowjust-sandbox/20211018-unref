package codes.quine.labo.unref

/** AssertKind represents the kind of zero-width assertion in the regular expression. */
enum AssertKind:
  case InputBegin
  case InputEnd
  case WordBoundary
  case NotWordBoundary
