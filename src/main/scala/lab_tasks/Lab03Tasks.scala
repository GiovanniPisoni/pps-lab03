package lab_tasks

import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher}
import u03.Optionals.Optional
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, filter, map}

import scala.annotation.tailrec

object Tasks_Lab03:

  //------------------------------TASK1------------------------------

  /*
   * Skip the first n elements of the sequence
   * E.g., [10, 20, 30], 2 => [30]
   * E.g., [10, 20, 30], 3 => []
   * E.g., [10, 20, 30], 0 => [10, 20, 30]
   * E.g., [], 2 => []
   */
  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
    case Cons(h1, t1) => second match
      case Cons(h2, t2) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()
    case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case _ => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
    case _ => Nil()

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */
  @tailrec
  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) => t match
      case Cons(h2, t2) => min(if h < h2
      then Cons(h, t2)
      else Cons(h2, t2))
      case _ => Optional.Just(h)
    case _ => Empty()

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => t match
      case Cons(h2, t2) => Cons(h, evenIndices(t2))
      case _ => Cons(h, Nil())
    case _ => Nil()


  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) => if h == elem
                       then true
                       else contains(t)(elem)
    case _ => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] =
    def removeDuplicates(seen: Sequence[A], remaining: Sequence[A]): Sequence[A] = remaining match
      case Cons(h, t) => if contains(seen)(h)
                         then removeDuplicates(seen, t)
                         else Cons(h, removeDuplicates(Cons(h, seen), t))
      case _ => Nil()

    removeDuplicates(Nil(), s)

  /*
   * Group contiguous elements in the sequence
   * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
   * E.g., [10, 20, 30] => [[10], [20], [30]]
   * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
   */
  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    def makeGroups(group: Sequence[A], remaining: Sequence[A]): Sequence[Sequence[A]] = remaining match
      case Cons(h, t) => group match
        case Cons(h2, t2) => if h == h2
                             then makeGroups(Cons(h, group), t)
                             else Cons(group, makeGroups(Cons(h, Nil()), t))
        case _ => makeGroups(Cons(h, Nil()), t)
      case _ => Cons(group, Nil())

    s match
      case Cons(h, t) => makeGroups(Cons(h, Nil()), t)
      case _ => Nil()

  /*
   * Partition the sequence into two sequences based on the predicate
   * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
   * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
   */
  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
    case Cons(h, t) => val (leftSeq, rightSeq) = partition(t)(pred)
      if pred(h)
      then (Cons(h, leftSeq), rightSeq)
      else (leftSeq, Cons(h, rightSeq))
    case _ => (Nil(), Nil())

  //------------------------------TASK2------------------------------

  def getCourses(s: Sequence[Person]): Sequence[String] = s match
    case Cons(person, course) => person match
      case Teacher(_, c) => Cons(c, getCourses(course))
      case _ => getCourses(course)
    case _ => Nil()

  def foldLeft[A, B](s: Sequence[A])(acc: B)(f: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
    case _ => acc

  def getTotalNumOfCourses(s: Sequence[Person]): Int =
    val teacher = filter(s) {
      case Teacher(_, _) => true
      case _ => false
    }
    foldLeft(map(teacher) {
      case Teacher(_, c) => c
    })(0)((acc, elem) => acc + 1)

