package akka.actor

import java.util.concurrent.TimeUnit

import akka.util.{ WildcardTree, WildcardIndex }
import org.openjdk.jmh.annotations.{ OutputTimeUnit, _ }

import scala.annotation.tailrec

/*
[info] Benchmark                               Mode  Cnt   Score   Error  Units
[info] WildcardTreeBenchmark.find_deepTree     avgt   30  24.760 ± 0.989  us/op
[info] WildcardTreeBenchmark.find_double       avgt   30   1.097 ± 0.033  us/op
[info] WildcardTreeBenchmark.find_single       avgt   30   1.124 ± 0.035  us/op
[info] WildcardTreeBenchmark.find_wideTree     avgt   30   0.116 ± 0.003  us/op
[info] WildcardTreeBenchmark.oldFind_deepTree  avgt   30  24.216 ± 0.722  us/op
[info] WildcardTreeBenchmark.oldFind_double    avgt   30   1.039 ± 0.030  us/op
[info] WildcardTreeBenchmark.oldFind_single    avgt   30   1.071 ± 0.029  us/op
[info] WildcardTreeBenchmark.oldFind_wideTree  avgt   30   0.119 ± 0.003  us/op
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Warmup(iterations = 4)
@Measurement(iterations = 10)
class WildcardIndexBenchmark {

  private implicit def listToIterator[T](l: List[T]): Iterator[T] = l.iterator
  private val emptyTree = WildcardIndex[Int]()

  val veryDeepTree = emptyTree
    .insert(List.fill(2000)("a").toArray, 1)
    .insert(List.fill(2000)("b").toArray, 2)
    .insert(List.fill(2000)("c").toArray, 3)

  private def randomString(length: Int): String = Stream.continually(util.Random.nextPrintableChar) take length mkString
  private val randomStrings = (1 to 2000).map(_ => randomString(6)).toList
  @tailrec
  private def insertRandomStrings(tree: WildcardIndex[Int], qty: Int): WildcardIndex[Int] = qty match {
    case 0 => tree
    case q => insertRandomStrings(tree.insert(List(randomStrings(q - 1)).toArray, q), q - 1)
  }

  val veryWideTree = insertRandomStrings(emptyTree, 2000)

  val doubleStarTree = emptyTree
    .insert(Array("a"), 1)
    .insert(Array("a", "b"), 2)
    .insert(Array("a", "c"), 3)
    .insert(Array("a", "**"), 4)
    .insert(Array("e", "*", "c"), 5)
    .insert(Array("x", "y", "*"), 6)
    .insert(Array("x", "**", "z"), 7)

  val singleStarTree = emptyTree
    .insert(Array("a"), 1)
    .insert(Array("a", "b"), 2)
    .insert(Array("a", "c"), 3)
    .insert(Array("a", "*"), 4)
    .insert(Array("e", "*", "c"), 5)
    .insert(Array("x", "y", "*"), 6)
    .insert(Array("x", "*", "z"), 7)

  private val emptyTreeOld = WildcardTree[Int]()

  val veryDeepTreeOld = emptyTreeOld
    .insert(List.fill(2000)("a"), 1)
    .insert(List.fill(2000)("b"), 2)
    .insert(List.fill(2000)("c"), 3)

  @tailrec
  private def insertRandomStrings(tree: WildcardTree[Int], qty: Int): WildcardTree[Int] = qty match {
    case 0 => tree
    case q => insertRandomStrings(tree.insert(List(randomStrings(q - 1)).toIterator, q), q - 1)
  }

  val veryWideTreeOld = insertRandomStrings(emptyTreeOld, 2000)

  val doubleStarTreeOld = emptyTreeOld
    .insert(List("a"), 1)
    .insert(List("a", "b"), 2)
    .insert(List("a", "c"), 3)
    .insert(List("a", "**"), 4)
    .insert(List("e", "*", "c"), 5)
    .insert(List("x", "y", "*"), 6)
    .insert(List("x", "**", "z"), 7)

  val singleStarTreeOld = emptyTreeOld
    .insert(List("a"), 1)
    .insert(List("a", "b"), 2)
    .insert(List("a", "c"), 3)
    .insert(List("a", "*"), 4)
    .insert(List("e", "*", "c"), 5)
    .insert(List("x", "y", "*"), 6)
    .insert(List("x", "*", "z"), 7)

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def find_single = {
    singleStarTree.find(List("x", "y", "z"))
    singleStarTree.find(List("r", "z", "z"))
    singleStarTree.find(List("a", "b", "c"))
    singleStarTree.find(List("x", "b", "c", "z"))
    singleStarTree.find(List("e", "b", "c"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def oldFind_single = {
    singleStarTreeOld.find(List("x", "y", "z"))
    singleStarTreeOld.find(List("r", "z", "z"))
    singleStarTreeOld.find(List("a", "b", "c"))
    singleStarTreeOld.find(List("x", "b", "c", "z"))
    singleStarTreeOld.find(List("e", "b", "c"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def find_double = {
    doubleStarTree.find(List("x", "y", "z"))
    doubleStarTree.find(List("r", "z", "z"))
    doubleStarTree.find(List("a", "b", "c"))
    doubleStarTree.find(List("x", "b", "c", "z"))
    doubleStarTree.find(List("e", "b", "c"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def oldFind_double = {
    doubleStarTreeOld.find(List("x", "y", "z"))
    doubleStarTreeOld.find(List("r", "z", "z"))
    doubleStarTreeOld.find(List("a", "b", "c"))
    doubleStarTreeOld.find(List("x", "b", "c", "z"))
    doubleStarTreeOld.find(List("e", "b", "c"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def find_deepTree = {
    veryDeepTree.find(List.fill(2000)("a"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def oldFind_deepTree = {
    veryDeepTreeOld.find(List.fill(2000)("a"))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def find_wideTree = {
    veryWideTree.find(List(randomStrings(3)))
  }

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def oldFind_wideTree = {
    veryWideTreeOld.find(List(randomStrings(3)))
  }

}

