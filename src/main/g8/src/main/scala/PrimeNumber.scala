import scala.annotation._
import scala.collection.mutable.ArrayBuffer

object PrimeNumber {
  def apply(n: Int = 1000000): Array[Int] = sieve(n)

  def sieve(n: Int): Array[Int] = {
    val primes = new ArrayBuffer[Int]
    val isPrime = Array.fill(n + 1)(true)

    @tailrec
    def loop(i: Int): Unit = {

      @tailrec
      def innerLoop(j: Int): Unit = {
        if (j > n) ()
        else {
          isPrime(j) = false
          innerLoop(j + i)
        }
      }

      if (i > n) ()
      else {
        if (isPrime(i)) {
          primes += i
          innerLoop(i * 2)
        }
        loop(i + 1)
      }
    }

    loop(2)
    primes.toArray
  }
}
