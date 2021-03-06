class UnionFind(n: Int) {
  private[this] val id = Array.tabulate(n)(identity)
  private[this] val sz = Array.fill(n)(1)

  private[this] def root(i: Int): Int = {
    var j = i
    while (j != id(j)) {
      id(j) = id(id(j))
      j = id(j)
    }
    j
  }

  final def find(p: Int, q: Int): Boolean = root(p) == root(q)
  val f = find _

  final def union(p: Int, q: Int): Unit = {
    val i = root(p)
    val j = root(q)
    if (sz(i) < sz(j)) {
      id(i) = j
      sz(j) += sz(i)
    }
    else {
      id(j) = i
      sz(i) += sz(j)
    }

  }
  val u = union _

  override def toString(): String = id.mkString(",")
}
