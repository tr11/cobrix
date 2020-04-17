package za.co.absa.cobrix.cobol.reader.stream

class StreamFromBytes(private val bytes: Array[Byte]) extends  SimpleStream {
  override def size: Long = 0

  override def offset: Long = 0

  override def inputFileName: String = null

  override def next(numberOfBytes: Int): Array[Byte] = bytes

  override def close(): Unit = {}
}
