package common

// import sun.misc.BASE64Decoder
import java.util.Base64

object BinaryConverter {

  private def log2(x: Double) = Math.log(x) / Math.log(2)

  /**
   * (version, bitsPerIndex, numberOfIndexes, byte0, byte1, ...)
   */
  def toBinarySolutions(solutionIndexes: Seq[Int], version: Byte ): Array[Byte] = {
    val bitsPerIndex = log2(solutionIndexes.max).ceil.toInt
    assert(bitsPerIndex <= 8)
    val valuesPerByte = 8 / bitsPerIndex
    val solutions = new Array[Byte]((solutionIndexes.size.toDouble / valuesPerByte).ceil.toInt)
    for (i <- 0 until solutionIndexes.size) {
      val indexInByte = valuesPerByte - 1 - (i % valuesPerByte)
      solutions(i / valuesPerByte) = (solutions(i / valuesPerByte) | (solutionIndexes(i) << (bitsPerIndex * indexInByte))).toByte
    }
    (Seq(version, bitsPerIndex.toByte, solutionIndexes.size.toByte) ++ solutions).toArray
  }

  def fromBinarySolutions(solutions: Array[Byte] ) : (Seq[Int], Byte) = {

    val bitsPerIndex = solutions(1)
    val valuesPerByte = 8 / bitsPerIndex
    val n = solutions(2)
    val solutionIndexes = new Array[Int](n)
    val version = solutions(0)
    val data = solutions.drop(3)
    for (i <- 0 until n) {
      val indexInByte = valuesPerByte - 1 - (i % valuesPerByte)
      val originalMask = (1 << bitsPerIndex) - 1
      val mask = originalMask << (bitsPerIndex * indexInByte)
      val sol = (data(i / valuesPerByte) & mask)
      solutionIndexes(i) = sol >> (bitsPerIndex * indexInByte)
      //println(s"i:$i indexInByte:$indexInByte mask:$mask data():${data(i / valuesPerByte)} sol:$sol solutionIndexes:${solutionIndexes(i)}")
    }
    (solutionIndexes,version)
  }

  def toBase64(buffer: Array[Byte]): String = {
    val encoder = Base64.getEncoder()
    encoder.encodeToString(buffer)
  }

  def fromBase64( s: String ): Array[Byte] = {
    val decoder = Base64.getDecoder()
    decoder.decode(s)
  }


}
