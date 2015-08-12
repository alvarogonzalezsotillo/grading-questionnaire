package giftToLatex

import java.io.File

/**
 * Created by alvaro on 12/08/15.
 */
object Main extends App{

  if( args.size != 1 ){
    throw new IllegalArgumentException( "gift file needed")
  }

  val giftFile = new File(args(0))

  GiftToLatex( giftFile )
}
