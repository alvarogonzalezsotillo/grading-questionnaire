package dsl

/**
  * Created by alvaro on 10/08/16.
  */
object CosasPorHacer extends App with DSL{


  haz la cama y compra el pan

  y después enciende la luz

  pon la mesa y enciende la cama y haz la mesa

}

trait DSL{

  trait Cosa

  case object cama extends Cosa
  case object luz extends Cosa
  case object basura extends Cosa
  case object mesa extends Cosa
  case object pan extends Cosa


  object y{
    def después( ascca: AccionSobreCosaConArticulo ) = {
      print( s"y al rato después " )
      ascca
    }

  }


  class AccionSobreCosaConArticulo{
    val verbo = toString
    def la( c: Cosa ) : AccionSobreCosaConArticulo = accion("la",c)
    def el( c: Cosa ) : AccionSobreCosaConArticulo = accion("le",c)
    def accion( articulo: String, c: Cosa ) = {
      println( s"Me han dicho que $verbo en la $c")
      new AccionSobreCosaConArticulo
    }
    def y( a: AccionSobreCosaConArticulo ) = a
  }

  case object haz extends AccionSobreCosaConArticulo
  case object enciende extends AccionSobreCosaConArticulo
  case object tira extends AccionSobreCosaConArticulo
  case object pon extends AccionSobreCosaConArticulo
  case object compra extends AccionSobreCosaConArticulo
}
