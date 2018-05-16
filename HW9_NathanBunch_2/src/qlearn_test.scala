object qlearn_test extends App {
  var myBot = new qbot(6,6)
  //currently does not support just any size, has to be 6,6 size.
  val learningData: Array[Array[Int]] = Array[Array[Int]](Array(-(1), -(1), -(1), -(1), 0, -(1)), Array(-(1), -(1), -(1), 0, -(1), 100), Array(-(1), -(1), -(1), 0, -(1), -(1)), Array(-(1), 0, 0, -(1), 0, -(1)), Array(0, -(1), -(1), 0, -(1), 100), Array(-(1), 0, -(1), -(1), 0, 100))
  myBot.train(learningData)
  myBot.test()
}
