import scala.annotation.tailrec

class RMQ(n: Int) {
  private val IntMax = Int.MaxValue << 2
  private val N = {
    var m = 1
    while (m < n) { m *= 2 }
    m
  }
  private val dat = Array.fill(2 * N - 1)(IntMax)

  // Update ith value with x
  def update(i: Int, x: Int): Unit = {
    var j = i + N - 1
    dat(j) = x
    while (j > 0) {
      j = (j - 1) / 2
      dat(j) = dat(j * 2 + 1) min dat(j * 2 + 2)
    }
  }

  // Returns the minimum value in [a, b)
  def query(a: Int, b: Int): Int = {

    def subq(i: Int, l: Int, r: Int): Int = {
      if (r <= a || b <= l) IntMax
      else if (a <= l && r <= b) dat(i)
      else subq(i * 2 + 1, l, (l + r) / 2) min subq(i * 2 + 2, (l + r) / 2, r)
    }

    subq(0, 0, N)
  }
}
