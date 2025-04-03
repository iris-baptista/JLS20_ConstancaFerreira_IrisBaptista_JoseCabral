object MyRandom {
  def nextInt(): (Int, Random) = MyRandom.nextInt()
}

trait Random {
  def nextInt: (Int, Random)
}

case class MyRandom(seed: Long) extends Random { //seed e o valor max q pode gerar?
  def nextInt(): (Int, Random) = {
    val newSeed = ((seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL) //L para long vou assumir
    val valorGerado = (newSeed >>> 16).toInt //>>> faz shift to utmost left
    val newRandom = MyRandom(newSeed)

    (valorGerado, newRandom)
  }
}
