object AddTwoNumber extends Test {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      if (next == null)
        x.toString
      else {
        s"$x->${next.toString}"
      }
    }
  }

  object ListNode {
    def unapply(node: ListNode): Option[(Int, ListNode)] =
      if (node == null) None
      else
        Some(node.x, node.next)
  }

  sealed trait Node

  case object Empty extends Node

  case class LinkedList(x: Int, next: Node) extends Node

  //or a LinkedList like
  case class Linked(x: Int, next: Linked)

  def len(list: ListNode): Int = {
    if (list == null) 0
    else 1 + len(list.next)
  }

  def reverse(list: ListNode): ListNode = {
    if (list == null) null
    else if (list.next == null) list
    else {
      val res = reverse(list.next)
      res.next = new ListNode(list.x)
      res
    }
  }

  // v1
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def node(x: Int) = new ListNode(x)

    var x = 0
    var result: ListNode = null
    var prev = result
    var l3: ListNode = null
    var l4: ListNode = null
    if (len(l1) > len(l2)) {
      l3 = l1
      l4 = l2
    } else {
      l3 = l2
      l4 = l1
    }
    while (l3 != null) {
      var sum: Int = 0
      if (l4 == null) {
        sum = l3.x + x
      } else {
        sum = l3.x + l4.x + x
      }
      x = sum / 10
      val y = sum % 10
      val next = node(y)
      if (prev == null) {
        result = next
        prev = next
      } else {
        prev.next = next
        prev = next
      }
      l3 = l3.next
      if (l4 != null) {
        l4 = l4.next
      }
    }
    if (x != 0) {
      prev.next = node(1)
    }
    result
  }

  def node2list(node: ListNode): List[Int] = {
    node match {
      case ListNode(x, null) => List(x)
      case ListNode(x, t) => x :: node2list(t)
    }
  }

  def list2node(list: List[Int]): ListNode = {
    list match {
      case h :: t =>
        val node = new ListNode(h)
        node.next = list2node(t)
        node
      case _ => null
    }
  }

  // v2: zip then foldLeft
  def add(l1: ListNode, l2: ListNode): ListNode = {
    list2node(add(node2list(l1), node2list(l2)))
  }

  def zip(l1: List[Int], l2: List[Int]): List[(Int, Int)] = {
    (l1, l2) match {
      case (x :: t1, y :: t2) => (x, y) :: zip(t1, t2)
      case (x :: t1, Nil) => (x, 0) :: zip(t1, Nil)
      case (Nil, y :: t1) => (0, y) :: zip(Nil, t1)
      case (Nil, Nil) => Nil
    }
  }

  def add(l1: List[Int], l2: List[Int]): List[Int] = {
    val result = zip(l1, l2).foldLeft[(Int, List[Int])]((0, Nil)) {
      case (x, y) =>
        val sum: Int = x._1 + y._1 + y._2
        (sum / 10, (sum % 10) :: x._2)
    }
    if (result._1 == 1) {
      (1 :: result._2).reverse
    } else {
      result._2.reverse
    }
  }

  def node(x: Int) = new ListNode(x)

  // v3: traverse list
  def f(l1: ListNode, l2: ListNode, init: Int): ListNode = {
    (l1, l2) match {
      case (ListNode(x, n1), ListNode(y, n2)) =>
        val sum = x + y + init
        val a = sum / 10
        val b = sum % 10
        val n = node(b)
        n.next = f(n1, n2, a)
        n
      case (null, ListNode(y, n2)) =>
        val sum = 0 + y + init
        val a = sum / 10
        val b = sum % 10
        val n = node(b)
        n.next = f(null, n2, a)
        n
      case (ListNode(x, n1), null) =>
        val sum = 0 + x + init
        val a = sum / 10
        val b = sum % 10
        val n = node(b)
        n.next = f(n1, null, a)
        n
      case (null, null) =>
        if(init == 1) node(1)
      else null
    }
  }

  def main(args: Array[String]): Unit = {
    val node1 = new ListNode(6)
    node1.next = new ListNode(6)
    val node2 = new ListNode(4)
    node2.next = new ListNode(5)
    addTwoNumbers(node1, node2).toString shouldEqual  "0->2->1"
    add(node1, node2).toString shouldEqual  "0->2->1"
    f(node1, node2, 0).toString shouldEqual  "0->2->1"
  }
}
