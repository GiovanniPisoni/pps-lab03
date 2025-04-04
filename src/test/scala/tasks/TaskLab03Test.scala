import org.junit.Assert.assertEquals
import org.junit.Test
import lab_tasks.Tasks_Lab03.*
import u02.Modules.Person
import u02.Modules.Person.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Streams.Stream
import u03.Streams.Stream.{Empty, empty}

class Lab03Task2Lab3Test:

  val sequence : Sequence[Person] = Cons(Teacher("Viroli", "PPS"),
                Cons(Teacher("Ricci", "PCD"),
                Cons(Student("Pisoni", 2024), Nil())))
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def getCoursesTest() =
    assertEquals(Cons("PPS", Cons("PCD", Nil())), getCourses(sequence))
    assertEquals(Nil(), getCourses(Nil()))

  @Test def foldLeftTest() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def getTotalNumOfCoursesTest() =
    assertEquals(2, getTotalNumOfCourses(sequence))
    assertEquals(0, getTotalNumOfCourses(Nil()))

class Lab03Task3Test:

  @Test def testFill() =
    assertEquals(Cons("1", Cons("1", Nil())), Stream.toList(Stream.fill(2)("1")))
    assertEquals(empty(), fill(0)("1"))

  @Test def testFibonacci() =
    val fib = Stream.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil())))))), Stream.toList(Stream.take(fib)(6)))