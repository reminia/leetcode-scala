import scala.reflect.ClassTag

object LazyArray {

  type assign[T] = Int => Int => T

  def apply[T: ClassTag](bound: (Int, Int))(g: assign[T]): LazyArray[T] =
    new LazyArray[T] {
      override val f: assign[T] = g
      override val calculated: Array[Array[Boolean]] =
        Array.ofDim(bound._1, bound._2)
      override val array: Array[Array[T]] = Array.ofDim[T](bound._1, bound._2)
    }
}

// a lazy initialized array
trait LazyArray[T] {
  import LazyArray._

  val f: assign[T]

  val calculated: Array[Array[Boolean]]

  val array: Array[Array[T]]

  def apply(i: Int)(j: Int): T =
    if (calculated(i)(j)) array(i)(j)
    else {
      calculated(i)(j) = true
      array(i)(j) = f(i)(j)
      array(i)(j)
    }
}
