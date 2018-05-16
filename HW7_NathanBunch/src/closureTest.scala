package myscope {
  class MyClass {
    def myfunction(f:(String) => Unit, name: String) {
      f(name)
    }
  }
}

package myscope2 {
  class MyClass2 {
    def myfunction(f: Int => Boolean, x: Int) {
      println(f(x))
    }
  }
}

//must use the import to use the classes/functions that exist in the other package
import myscope.MyClass
import myscope2.MyClass2

object closureTest extends App {
  var myclass = new MyClass()
  var myclass2 = new MyClass2()
  var myfunc = (x: String) => println(x.toLowerCase)
  var myfunc2 = (x: Int) => {if (x > 0) true else false}
  myclass.myfunction(myfunc, "HELLO WORLD")
  myclass2.myfunction(myfunc2, 25)
}
