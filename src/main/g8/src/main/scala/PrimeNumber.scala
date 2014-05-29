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

class PrimeNumber(val p: Long) {

  def modPow(x: Long, n: Long): Long = {

    @tailrec
    def loop(acc: Long, x: Long, n: Long): Long = {
      if (n == 0) acc
      else if ((n & 1) == 0) loop(acc, x * x % p, n >> 1)
      else loop(acc * x % p, x * x % p, n >> 1)
    }

    loop(1, x, n)
  }

  def modInv(x: Long): Long = modPow(x, p - 2)

  // Partial permutation aPb
  def modPermu(a: Long, b: Long): Long = {
    
    @tailrec
    def loop(acc: Long, i: Long, j: Long): Long = {
      if (j == 0) acc
      else loop(acc * i % p, i - 1, j - 1)
    }
    
    loop(1, a, b)
  }

  def modFact(x: Long): Long = modPermu(x, x)

  def modInvFact(x: Long): Long = {

    @tailrec
    def loop(acc: Long, x: Long): Long = {
      if (x < 2) acc
      else loop(acc * modInv(x) % p, x - 1)
    }

    loop(1, x)
  }

  // Compbination aCb
  def modCombi(a: Long, b: Long): Long = modPermu(a, b) * modInvFact(b) % p
}
