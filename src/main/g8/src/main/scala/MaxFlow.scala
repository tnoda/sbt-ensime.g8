import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

class MaxFlow(s: Int, t: Int, MaxV: Int) {
  type Edge = (Int, Double, Int) // (to, capacity, rev)
  type Graph = Array[Array[Edge]]
  type GraphBuf = Array[ArrayBuffer[Edge]]

  val gb: GraphBuf = Array.fill(MaxV)(new ArrayBuffer[Edge])
  val g: Graph = Array.ofDim[Array[Edge]](MaxV)
  
  def addEdge(from: Int, to: Int, capacity: Double): Unit = {
    gb(from) += ((to, capacity, gb(to).length))
    gb(to)   += ((from, 0, gb(from).length - 1))
  }

  def build(): Unit =
    List.range(0, gb.length).foreach { i => g(i) = gb(i).toArray }
  
  def relevel(): Array[Int] = {
    val level = Array.fill(MaxV)(-1)
    level(s) = 0
    
    @tailrec
    def bfs(q: Queue[Int]): Array[Int] = {
      if (q.isEmpty) level
      else {
        val (v, vs) = q.dequeue
        val validAdjacents: PartialFunction[Edge, Int] = {
          case (to, cap, rev) if cap > 0 && level(to) < 0 => to
        }
        val vas = g(v).collect(validAdjacents)
        vas.foreach { i => level(i) = level(v) + 1 }
        bfs(vs ++ vas)
      }
    }

    bfs(Queue(s))
  }

  def updateCapacity(i: Int, j: Int, d: Double): Unit = {
    val (to, cap, rev) = g(i)(j)
    g(i)(j) = (to, cap + d, rev)
  }

  def buildBlockingPaths(level: Array[Int]): Double = {
    val history = Array.ofDim[Int](MaxV)

    def dfs(v: Int, t: Int, f: Double): Double = {

      def augment(i: Int): Double = {

        @tailrec
        def loop(i: Int): Double = {
          history(v) = i
          if (i ==  g(v).length) 0
          else {
            val (to, cap, rev) = g(v)(i)
            val d = if (cap > 0 && level(v) < level(to)) dfs(to, t, f min cap)
                    else 0
            if (d > 0) {
              updateCapacity(v, i, -d)
              updateCapacity(to, rev, d)
              d
            }
            else {
              loop(i + 1)
            }
          }
        }

        loop(i)
      }

      if (v == t) f
      else augment(history(v))
    }

    @tailrec
    def loop(acc: Double): Double = {
      val f = dfs(s, t, Double.MaxValue / 2)
      if (f > 0) loop(acc + f)
      else acc
    }

    loop(0.0D)
  }

  def maxFlow(): Double = {

    @tailrec
    def loop(acc: Double): Double = {
      val level = relevel
      if (level(t) < 0) acc
      else loop(acc + buildBlockingPaths(level))
    }

    loop(0.0D)
  }
}
