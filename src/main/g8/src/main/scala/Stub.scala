import java.util.Scanner
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import java.lang.Long.bitCount
import java.nio.CharBuffer
import java.util.Arrays.binarySearch
import scala.annotation.tailrec
import scala.annotation.switch
import scala.annotation.unchecked
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import scala.util.control.Breaks._

object Stub extends App {
  // val sc = new Scanner(System.in)
  val out = new PrintWriter(System.out)

  object Input {
    import java.lang.Character._

    lazy val in = new BufferedReader(new InputStreamReader(java.lang.System.in))
    var buf: Array[Char] = Array()
    var pos: Int = 0

    def next(): String = {
      while (pos == buf.length) {
        buf = in.readLine.toArray
        pos = 0
      }
      while (isWhitespace(buf(pos))) {
        pos += 1
      }
      while (pos == buf.length) {
        buf = in.readLine.toArray
        pos = 0
      }
      val s = pos
      while (pos < buf.length && !isWhitespace(buf(pos))) {
        pos += 1
      }
      new java.lang.String(buf, s, pos - s)
    }

    def nextInt(): Int = next.toInt

    def nextLong(): Long = next.toLong

    def nextFloat(): Float = next.toFloat

    def nextDouble(): Double = next.toDouble

    val nextString: () => String = next _
  }

  import Input._

  // do something

  out.flush
}
