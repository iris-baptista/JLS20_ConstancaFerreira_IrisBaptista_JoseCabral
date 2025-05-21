import scala.math.pow

object MyRandom {
  def nextInt(): (Int, Random) = MyRandom.nextInt()
}

trait Random {
  def nextInt(sizeList: Int): (Int, Random)
}

case class MyRandom(seed: Long) extends Random { //seed equivalente a x na equacao y=f(x)
  def nextInt(sizeList: Int): (Int, Random) = {

    //deviamos por aquela cena da seed baseada no tempo
    val newSeed = ((seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL) //L para long

    val bitsWanted= getBits(sizeList, 9) //9 e o maior possivel para qualquer board de go (da 512 valores, board maior tem 361)
    val shifts= 48-bitsWanted //seed tem 48 bits normalmente, shifts e os bits q vamos remover
    val valorGerado = (newSeed >>> shifts).toInt //>>> faz shift to utmost left para limitar o valor

    val newRandom = MyRandom(newSeed)
    (valorGerado, newRandom)
  }

  def getBits(max: Int, bitsPossible: Int): Int = {
    if(pow(2, bitsPossible-1).intValue >= max){ //se o exponencial de 2 menor/igual ainda for maior q o limite max
      getBits(max, bitsPossible-1) //repete com o exponencial menor
    }
    else{ //se o exponencial de 2 menor for menor q o limite max (nao inclui todos os indexes da lista)
      bitsPossible //devolve os bits possiveis
    }
  }
}
