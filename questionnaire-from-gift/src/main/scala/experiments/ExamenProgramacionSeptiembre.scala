package experiments

/**
 * Created by alvaro on 20/08/15.
 */
object ExamenProgramacionSeptiembre extends App {

  def esCapicua(n: Int) = n.toString == n.toString.reverse

  def esPrimo(n: Int) = !(2 to n/2).exists(n % _ == 0)

  def esImpar(n: Int) = n % 2 == 1

  def tieneMasDigitosDe(n: Int, digitos: Int) = n.toString.size > digitos

  def primero(capicua: Boolean, impar: Boolean, primo: Boolean, digitosMinimos: Int) = {

    def aceptado(n: Int) = {

      (!capicua || esCapicua(n)) &&
        (!impar || esImpar(n)) &&
        (!primo || esPrimo(n)) && tieneMasDigitosDe(n, digitosMinimos - 1)
    }

    Iterator.from(1).find(aceptado).get

  }

  println( primero(true,true,false,1) )
  println( primero(true,true,false,3) )
  println( primero(false,true,true,4) )
  println( primero(true,false,true,5) )
}
