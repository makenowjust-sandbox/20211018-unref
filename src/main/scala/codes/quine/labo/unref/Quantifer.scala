package codes.quine.labo.unref

/** Quantifier is a quantity specifier for repeats. */
enum Quantifier:
  case Star
  case Plus
  case Question
  case Exact(n: Int)
  case Unbounded(n: Int)
  case Bounded(n1: Int, n2: Int)

  /** Returns the minimum number of repeats. */
  def min: Int = this match
    case Star          => 0
    case Plus          => 1
    case Question      => 0
    case Exact(n)      => n
    case Unbounded(n)  => n
    case Bounded(n, _) => n

  /** Returns the maximum number of repeats. If there is no upper bound, the return value will be `None`. */
  def max: Option[Int] = this match
    case Star          => None
    case Plus          => None
    case Question      => Some(1)
    case Exact(n)      => Some(n)
    case Unbounded(n)  => None
    case Bounded(_, n) => Some(n)
