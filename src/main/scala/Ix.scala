import scala.language.postfixOps
import scala.reflect.ClassTag

//array index
trait Ix[T] {
  def range(range: (T, T)): List[T]

  def index(range: (T, T))(a: T): Int
}

object Ix {

  implicit object Tuple2Ix extends Ix[(Int, Int)] {
    override def range(range: ((Int, Int), (Int, Int))): List[(Int, Int)] = {
      val (l, u) = range
      val seq = for {
        x <- l._1 to l._2
        y <- u._1 to u._2
      } yield {
        x -> y
      }
      seq.toList
    }

    override def index(range: ((Int, Int), (Int, Int)))(a: (Int, Int)): Int = {
      val (l, u) = range
      val row = l._1 to l._2 indexOf a._1
      val column = u._1 to u._2 indexOf a._2
      row * (l._2 - l._1 + 1) + column
    }
  }

  implicit object IntIx extends Ix[Int] {
    //unsafe
    override def range(range: (Int, Int)): List[Int] =
      range._1 to range._2 toList

    //unsafe
    override def index(range: (Int, Int))(a: Int): Int = a - range._1
  }

  def range[T: Ix](range: (T, T)): List[T] = implicitly[Ix[T]].range(range)

  def index[T: Ix](range: (T, T))(a: T): Int = implicitly[Ix[T]].index(range)(a)

  def main(args: Array[String]): Unit = {
    println(index((3, 4))(3))
    println(index((1, 2), (3, 4))((1, 4)))
  }

  def listArray[I<: Ix[I], T: ClassTag](bounds: (I, I))(elems: List[T]): IArray[I, T] = {
    new IArray[I, T] {
      val array: Array[T] = elems.toArray

      override val bound: (I, I) = bounds

      override def apply(i: I): T = array(index(bounds)(i)(implicitly[Ix[I]]))
    }
  }

  // class IArrayImpl[I <: Ix[I], T] extends IArray[I, T] {
  //   override val bound: (I, I) = _
  //   override val f: I => T = _
  //
  //   override def apply(i: I): T = ???
  // }

}

trait IArray[I <: Ix[I], T] {
  val bound: (I, I)
  def apply(i: I): T
}
