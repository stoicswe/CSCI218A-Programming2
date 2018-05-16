import scala.collection.{AbstractIterator, AbstractSeq, breakOut, mutable}
import scala.reflect.ClassTag

object MyArrayStack {
  def newBuilder[A]: MyArrayStack[A] = new MyArrayStack[A]
  def empty: MyArrayStack[Nothing] = new MyArrayStack()
  def apply[A: ClassTag](elems: A*): MyArrayStack[A] = {
    val els: Array[AnyRef] = elems.reverseMap(_.asInstanceOf[AnyRef])(breakOut)
    if (els.length == 0) new MyArrayStack()
    else new MyArrayStack[A](els, els.length)
  }

  def growArray(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](math.max(x.length * 2, 1))
    Array.copy(x, 0, y, 0, x.length)
    y
  }

  def clone(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](x.length)
    Array.copy(x, 0, y, 0, x.length)
    y
  }
}

class MyArrayStack[T] private(private var table : Array[AnyRef], private var index : Int)
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with mutable.Builder[T, MyArrayStack[T]]
{
  def this() = this(new Array[AnyRef](1), 0)
  def apply(n: Int): T = table(index - 1 - n).asInstanceOf[T]
  def length = index
  def clear() {index = 0; table = new Array(1)}
  def top: T = table(index - 1).asInstanceOf[T]
  def result = {reverseTable(); this}
  def +=(x: T): this.type = { push(x); this }
  override def isEmpty: Boolean = index == 0

  def push(x: T) {
    if (index == table.length) table = MyArrayStack.growArray(table)
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
}