package lazylist

import lazylist.LazyList.*

enum LazyList[+A] {
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = foldRight(Option.empty)((a, b) => Option(a))

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): LazyList[A] = (this, n) match {
    case (Empty, _)         => Empty
    case (_, nn) if nn <= 0 => Empty
    case (Cons(h, t), _) =>
      Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): LazyList[A] = (this, n) match {
    case (Empty, _)         => Empty
    case (_, nn) if nn <= 0 => Empty
    case (Cons(h, t), _)    => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): LazyList[A] = foldRight(empty) { (a, b) =>
    if (p(a)) {
      cons(a, b)
    } else {
      b
    }
  }

  def exist(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def forAllStacksafe(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAllStacksafe(p)
    case _                    => false
  }

  def map[B](f: A => B): LazyList[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): LazyList[A] = foldRight(empty) { (a, b) =>
    if (p(a)) {
      cons(a, b)
    } else {
      b
    }
  }

  def append[A2 >: A](other: => LazyList[A2]): LazyList[A2] = foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(empty)((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this) {
    case Empty      => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeViaUnfold(n: Int): LazyList[A] = unfold((n, this)) {
    case (1, Cons(h, t))            => Some((h(), (0, empty)))
    case (nn, Cons(h, t)) if nn > 1 => Some((h(), (nn - 1, t())))
    case _                          => None
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _                    => None
  }

  def zipWith[B](other: LazyList[B]): LazyList[(A, B)] = unfold((this, other)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some((ah(), bh()), (at(), bt()))
    case _                            => None
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
    case (Cons(ah, at), Empty)        => Some((Some(ah()), None), (at(), empty))
    case (Empty, Cons(bh, bt))        => Some((None, Some(bh())), (empty, bt()))
    case _                            => None
  }

  def startsWith[A](prefix: LazyList[A]): Boolean = {
    zipAll(prefix).forAll((a, b) => b.isEmpty || a == b)
  }

  def tails: LazyList[LazyList[A]] = unfold(this) {
    case Empty      => None
    case Cons(h, t) => Some(cons(h(), t()), t())
  }

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] = foldRight((init, cons(init, empty))) { (a, b) =>
    lazy val b1   = b
    lazy val next = f(a, b1._1)
    (next, cons(next, b1._2))
  }._2

}

object LazyList {

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] = {
    if (as.isEmpty) {
      Empty
    } else {
      cons(as.head, apply(as.tail*))
    }
  }
  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] = unfold(())(_ => Some((a, ())))

  def from(n: Int): LazyList[Int] = unfold(n)(nn => Some((nn, nn)))

  def fibs: LazyList[Int] = {
    unfold((0, 1))((a, b) => Some(a, (b, a + b)))
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state) match {
      case None          => empty
      case Some((h, ss)) => cons(h, unfold(ss)(f))
    }
  }

}
