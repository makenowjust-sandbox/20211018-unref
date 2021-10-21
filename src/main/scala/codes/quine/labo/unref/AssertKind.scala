package codes.quine.labo.unref

sealed abstract class AssertKind

object AssertKind {
  case object InputBegin extends AssertKind
  case object InputEnd extends AssertKind
  case object WordBoundary extends AssertKind
  case object NotWordBoundary extends AssertKind
}
