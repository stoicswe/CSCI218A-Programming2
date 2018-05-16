import scala.util.Random

class qbot(val rx: Int, val ry: Int){
  private var weights = Array.ofDim[Int](rx, ry)
  private val initialStates = Array(1, 3, 5, 2, 4, 0)
  private var currentState = 0
  private var learnRate = 0.0

  def train(room: Array[Array[Int]], learnRate: Double = 0.8, iterations: Int = 10): Unit ={
    this.learnRate = learnRate
    if (room(0).length != rx || room(1).length != ry){println("Error. Data not right size for this robot.")} else {
      for(j <- 0 to iterations; i <- 0 to weights(0).length-1){episode(initialStates(i), room)}
      println("Q Matrix Values: ")
      for(i <- 0 to weights(0).length-1){
        for (j <- 0 to weights(1).length-1){
          print(weights(i)(j) + ",\t")
        }
        println()
      }
      println()
    }
  }

  def test(): Unit ={
    println("Shortest routes from initial states: ")
    for (i <- 0 to weights(0).length-1){
      currentState = initialStates(i)
      var newState = 0
      do{
        newState = maximum(currentState, true)
        print(currentState + ", ")
        currentState = newState
      } while(currentState < 5)
      println("5")
    }
  }

  def episode(init: Int, room: Array[Array[Int]]): Unit ={
    currentState = init
    do{
      chooseAction(room)
    } while (currentState == 5)

    for(i <- 0 to weights(0).length-1){
      chooseAction(room)
    }
  }

  def chooseAction(room: Array[Array[Int]]): Unit ={
    var possibleAction = 0
    possibleAction = getRandomAction(weights(0).length, room)
    if (room(currentState)(possibleAction) >= 0){
      weights(currentState)(possibleAction) = reward(possibleAction, room)
      currentState = possibleAction
    }
  }

  def getRandomAction(upper: Int, room: Array[Array[Int]]): Int ={
    var action = 0
    var choice = false
    while(!choice){
      action = new Random().nextInt(upper)
      if(room(currentState)(action) > -1){ choice = true}
    }
    action
  }

  def maximum(state: Int, returnIndex: Boolean): Int ={
    var win = 0
    var foundWin = false
    var done = false
    while(!done){
      foundWin = false
      for(i <- 0 to weights(0).length-1){
        if (i != win){
          if (weights(state)(i) > weights(state)(win)){
            win = i
            foundWin = true
          }
        }
      }

      if (!foundWin){
        done = true
      }
    }

    if(returnIndex) win else weights(state)(win)
  }

  def reward(action: Int, room: Array[Array[Int]]): Int ={
    (room(currentState)(action) + (learnRate * maximum(action, false))).toInt
  }

}
