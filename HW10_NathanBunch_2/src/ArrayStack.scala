package scala
package collection
package mutable

import generic._
import scala.reflect.ClassTag

object ArrayStack extends SeqFactory[ArrayStack] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ArrayStack[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, ArrayStack[A]] = new ArrayStack[A]
  def empty: ArrayStack[Nothing] = new ArrayStack()
  def apply[A: ClassTag](elems: A*): ArrayStack[A] = {
    val els: Array[AnyRef] = elems.reverseMap(_.asInstanceOf[AnyRef])(breakOut)
    if (els.length == 0) new ArrayStack()
    else new ArrayStack[A](els, els.length)
  }

  private[mutable] def growArray(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](math.max(x.length * 2, 1))
    Array.copy(x, 0, y, 0, x.length)
    y
  }

  private[mutable] def clone(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](x.length)
    Array.copy(x, 0, y, 0, x.length)
    y
  }
}

@SerialVersionUID(8565219180626620510L)
class ArrayStack[T] private(private var table : Array[AnyRef],
                            private var index : Int)
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqLike[T, ArrayStack[T]]
    with GenericTraversableTemplate[T, ArrayStack]
    with IndexedSeqOptimized[T, ArrayStack[T]]
    with Cloneable[ArrayStack[T]]
    with Builder[T, ArrayStack[T]]
    with Serializable
{
  def this() = this(new Array[AnyRef](1), 0)

  def apply(n: Int): T =
    table(index - 1 - n).asInstanceOf[T]

  def length = index

  override def companion = ArrayStack

  def update(n: Int, newelem: T) =
    table(index - 1 - n) = newelem.asInstanceOf[AnyRef]

  def push(x: T) {
    if (index == table.length) table = ArrayStack.growArray(table)
    table(index) = x.asInstanceOf[AnyRef]
    index += 1
  }

  def pop(): T = {
    if (index == 0) sys.error("Stack empty")
    index -= 1
    val x = table(index).asInstanceOf[T]
    table(index) = null
    x
  }

  def top: T = table(index - 1).asInstanceOf[T]

  def dup() = push(top)

  def clear() {
    index = 0
    table = new Array(1)
  }

  def drain(f: T => Unit) = while (!isEmpty) f(pop())

  override def ++=(xs: TraversableOnce[T]): this.type = { xs foreach += ; this }

  def +=(x: T): this.type = { push(x); this }

  def result = {
    reverseTable()
    this
  }

  private def reverseTable() {
    var i = 0
    val until = index / 2
    while (i < until) {
      val revi = index - i - 1
      val tmp = table(i)
      table(i) = table(revi)
      table(revi) = tmp
      i += 1
    }
  }

  def combine(f: (T, T) => T): Unit = push(f(pop(), pop()))

  def reduceWith(f: (T, T) => T): Unit = while(size > 1) combine(f)

  override def size = index

  def preserving[T](action: => T) = {
    val oldIndex = index
    val oldTable = ArrayStack.clone(table)

    try {
      action
    } finally {
      index = oldIndex
      table = oldTable
    }
  }

  override def isEmpty: Boolean = index == 0

  override def iterator: Iterator[T] = new AbstractIterator[T] {
    var currentIndex = index
    def hasNext = currentIndex > 0
    def next() = {
      currentIndex -= 1
      table(currentIndex).asInstanceOf[T]
    }
  }

  override def foreach[U](f: T => U) {
    var currentIndex = index
    while (currentIndex > 0) {
      currentIndex -= 1
      f(table(currentIndex).asInstanceOf[T])
    }
  }

  override def clone() = new ArrayStack[T](ArrayStack.clone(table), index)
}