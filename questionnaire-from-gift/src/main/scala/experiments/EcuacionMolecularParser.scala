package experiments

import java.io.{PrintStream, Serializable, Writer}

import scala.Option
import scala.collection.mutable.IndexedSeq
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by alvaro on 12/11/17.
  */

object EcuacionMolecular{

  case class AtomoEnMolecula(elemento: String, cantidad: Int = 1)

  case class Molecula(atomos: List[AtomoEnMolecula], cantidad: Int = 1)

  case class LadoEcuacion(moleculas: List[Molecula])

  case class Ecuacion(ladoIzquierdo: LadoEcuacion, ladoDerecho: LadoEcuacion)

  def atomosDeMolecula( m: Molecula, multiplier: Int = 1 ) : Map[String,Int] = {
    m.atomos.map( am => am.elemento -> am.cantidad * m.cantidad * multiplier ).toMap
  }

  def atomosDeLadoDeEcuacion( l : LadoEcuacion, multipliers: Option[List[Int]] = None ) : Map[String,Int] = {
    val multipliers_ = multipliers.getOrElse(Iterator.continually(1).take(l.moleculas.size).toList)
    val maps = l.moleculas.zip(multipliers_).map{ case (mol, mul) => atomosDeMolecula(mol,mul) }
    maps.foldLeft( Map[String,Int]() ){ (m,accum) =>
      val keys = m.keySet ++ accum.keySet
      keys.map( k => k -> (m.getOrElse(k,0) + accum.getOrElse(k,0) ) ).toMap
    }
  }

  def esAjustada( e: Ecuacion, multipliers: List[Int] ): Boolean ={
    val ni = e.ladoIzquierdo.moleculas.size
    val nd = e.ladoDerecho.moleculas.size
    assert( ni+nd == multipliers.size )
    val (mi,md) = multipliers.splitAt(ni)
    val ai = atomosDeLadoDeEcuacion(e.ladoIzquierdo, Some(mi))
    val ad = atomosDeLadoDeEcuacion(e.ladoDerecho, Some(md))

    ai == ad
  }

  def ajusta( e: Ecuacion, maxSum: Int = 10 ) : Option[Ecuacion]= {
    val it_ = Secuencias.iterator(e.ladoDerecho.moleculas.size + e.ladoIzquierdo.moleculas.size, maxSum )
    val it = it_.map{ l => l.map(_+1) }
    val multipliers = it.find( esAjustada(e,_) )


    val ret = multipliers.map{ m =>
      val (mi,md) = m.splitAt(e.ladoIzquierdo.moleculas.size)
      val li = e.ladoIzquierdo.moleculas.zip(mi).map{ case (mol,mul) =>
        Molecula(mol.atomos, mol.cantidad*mul)
      }
      val ld = e.ladoDerecho.moleculas.zip(md).map{ case (mol,mul) =>
        Molecula(mol.atomos, mol.cantidad*mul)
      }
      Ecuacion( LadoEcuacion(li), LadoEcuacion(ld) )
    }

    ret

  }

  def toString( e: Ecuacion ) = {

    def toStringC(i: Int) = if( i > 1 ) i.toString else ""

    def toStringA(a: AtomoEnMolecula ) = a.elemento + toStringC( a.cantidad )

    def toStringM(m: Molecula ) =  toStringC(m.cantidad) + m.atomos.map(toStringA(_)).mkString


    def toStringL(l: LadoEcuacion ) = {
      l.moleculas.map( toStringM(_) ).mkString(" + ")
    }
    toStringL(e.ladoIzquierdo) + " <--> " + toStringL(e.ladoDerecho)
  }
}

object Secuencias{


  private def primero( a: IndexedSeq[Int], sum: Int ){
    a(0) = sum
    for( i <- 1 until a.size ) a(i) = 0
  }

  private def siguiente( a: Array[Int], sum: Int, pos: Int = 0 ) : Boolean = {
    assert( pos < a.length )
    if( pos < a.length-1) {
      // INTENTO CAMBIAR LA COLA
      if (siguiente(a, sum - a(pos), pos + 1)) {
        return true
      }

      // SI NO CAMBIA, DEBO BAJAR UNA UNIDAD LA POSICION
      if( a(pos) > 0 ) {
        a(pos) -= 1
        val slice = a.view.slice(pos+1,a.length)
        primero( slice , sum - a(pos) )
        return true
      }
    }
    return false

  }

  private def supersiguiente( a: Array[Int] ) : Int = {
    val sum = a.sum
    if( siguiente( a, sum ) ){
      return sum
    }

    primero(a,sum+1)
    return sum+1
  }

  private def pruebaSimple() {
    val sum = 3
    val s = new Array[Int](3)
    primero(s, sum)
    println(s.mkString(","))
    while (siguiente(s, sum)) {
      println(s.mkString(","))
    }
  }

  def iterator( size: Int, maxSum: Int ) = {
    val s = new Array[Int](size)
    primero(s,1)
    val ret = Iterator.iterate(s.toList){ v =>
      val a = v.toArray
      supersiguiente(a)
      a.toList
    }
    ret.takeWhile( _.sum <= maxSum )
  }
}

object ProbarSecuencia extends App{
  import Secuencias._

  for( s <- Secuencias.iterator(3,3) ) println( s.mkString(","))


}



class EcuacionMolecularParser extends RegexParsers {

  import EcuacionMolecular._

  def blanco = "\\s*".r

  def atomo: Parser[String] = "[A-Z][a-z]?".r

  def numero: Parser[Int] = "[0-9]+".r ^^ {
    case n => n.toInt
  }

  def atomoEnMolecula: Parser[AtomoEnMolecula] = atomo ~ numero.? ^^ {
    case a ~ n => AtomoEnMolecula(a, n.getOrElse(1))
  }

  def molecula: Parser[Molecula] = blanco ~> (numero.? ~ rep(atomoEnMolecula)) <~ blanco ^^ {
    case n ~ as => Molecula(as, n.getOrElse(1))
  }

  def ladoDeEcuacion = molecula ~ rep((blanco ~ "\\+".r ~ blanco) ~> molecula) ^^ {
    case m ~ ms => LadoEcuacion(m :: ms)
  }

  def ladoIzquierdo = ladoDeEcuacion <~ (blanco ~ ("=".r | "<-*>".r) )

  def ecuacion = (ladoIzquierdo <~ blanco) ~ ladoDeEcuacion <~ blanco ^^ {
    case li ~ ld => Ecuacion(li, ld)
  }

}

object ProbarEcuacion extends App {

  import EcuacionMolecular._

  val parser = new EcuacionMolecularParser
  import parser._


  val result = parser.parse(parser.ecuacion, " HCl + MnO2 <-->  MnCl2 + H2O + Cl2")



  result match {
    case Success(ecuacion, _) =>
      println( EcuacionMolecular.toString(ecuacion) )
      //println( atomosDeLadoDeEcuacion(ecuacion.ladoIzquierdo) )
      //println( atomosDeLadoDeEcuacion(ecuacion.ladoDerecho) )

      val e = ajusta(ecuacion,20)

      e.foreach( ec => println( EcuacionMolecular.toString(ec)) )
    case _: NoSuccess => println("Error")
  }
}