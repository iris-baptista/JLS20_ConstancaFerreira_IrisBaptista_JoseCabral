import scala.math.pow

object MyRandom {
  def nextInt(): (Int, MyRandom) = MyRandom.nextInt()
}

case class MyRandom(seed: Long) { //seed equivalente a x na equacao y=f(x)
  val limit= 20
  var counter= 0

  def nextInt(sizeList: Int): (Int, MyRandom) = {
    val newSeed = ((seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL) //L para long

    val bitsWanted= getBits(sizeList, 9) //9 e o maior possivel para qualquer board de go (da 512 valores, board maior tem 361)
    val shifts= 48-bitsWanted //seed tem 48 bits normalmente, shifts e os bits q vamos remover
    val valorGerado = (newSeed >>> shifts).toInt //>>> faz shift right para limitar o valor

    val newRandom = MyRandom(newSeed)
    if(valorGerado >= sizeList || valorGerado < 0){ //se gerou um dos valores out of bounds
      if(limit == counter){ //to prevent overflowing the buffer/stack
        (0, newRandom)
      }
      else{
        newRandom.counter= counter+1
        newRandom.nextInt(sizeList)
      }
    }
    else {
      (valorGerado, newRandom)
    }
  }

  def getBits(max: Int, bitsPossible: Int): Int = {
    if(pow(2, bitsPossible-1).intValue >= max){ //se o exponencial de 2 menor/igual ainda for maior q o limite max
      if(bitsPossible == 1){ //para impedir stack overflow (e de entrar em numero negativos)
        bitsPossible
      }
      else{
        getBits(max, bitsPossible-1) //repete com o exponencial menor
      }
    }
    else{ //se o exponencial de 2 menor for menor q o limite max (nao inclui todos os indexes da lista)
      bitsPossible //devolve os bits possiveis
    }
  }
}
