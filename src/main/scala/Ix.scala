import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

//array index
trait Ix[T] {
  //unsafe
  def range(range: (T, T)): List[T]

  //unsafe
  def index(range: (T, T))(a: T): Int
}

trait IArray[I, T] {
  val ix: Ix[I]
  val bound: (I, I)

  def apply(i: I): T
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


  def listArray[I: Ix, T: ClassTag](bounds: (I, I))(f: I => T): IArray[I, T] = {
    new IArray[I, T] {
      val map: mutable.HashMap[I, T] = mutable.HashMap.empty

      override val bound: (I, I) = bounds

      override def apply(i: I): T = {
        map.getOrElseUpdate(i, f(i))
      }

      override val ix: Ix[I] = implicitly[Ix[I]]
    }
  }

  def main(args: Array[String]): Unit = {
    println(index((3, 4))(3))
    println(index((1, 2), (3, 4))((1, 4)))
  }
}
