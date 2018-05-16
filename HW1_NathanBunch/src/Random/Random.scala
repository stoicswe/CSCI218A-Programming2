package Random {

  class Random(private var seed: Int) {
    var a = 1664525
    var b = 1013904223
    var n = 32

    def nextInt(siz: Int = 1) = (siz * (seed * a + b) % Math.pow(2, n)).toInt

    def nextDouble(siz: Double = 1.0) = siz * (seed * a + b) % Math.pow(2, n)

    def setSeed(newSeed: Int) {
      this.seed = newSeed
    }
  }

}
