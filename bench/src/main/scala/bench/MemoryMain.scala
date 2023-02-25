package bench
import java.text.NumberFormat
import java.util.Locale
import scala.collection.immutable.{ArraySeq, Queue}
import scala.collection.{SortedSet, mutable}
import collection.JavaConverters.*
object MemoryMain{
  def main(args: Array[String]): Unit = {
    def obj = new Object()
    def nums[T](n: Int, f: Int => T) = (0 until n).iterator.map(f)
    val collections = Seq[(String, Int => AnyRef)](
      ("Vector",          nums(_, _ => obj).toVector),
      ("Array",           nums(_, _ => obj).toArray),
      ("ArraySeq",           nums(_, _ => obj).to[ArraySeq[Object]](ArraySeq)),
      ("List",            nums(_, _ => obj).toList),
      ("UnforcedLazyList",  nums(_, _ => obj).to[LazyList[Object]](LazyList)),
      ("ForcedLazyList",    {n => val x = nums(n, _ => obj).to[LazyList[Object]](LazyList); x.foreach(x => ()); x}),
      ("Set",             nums(_, _ => obj).toSet),
      ("Map",             nums(_, _ => (obj, obj)).toMap),

      ("SortedSet", nums(_, x=>x).to[SortedSet[Int]](SortedSet)),
      ("Queue",     nums(_, _ => obj).to[Queue[Object]](Queue)),

      ("m.Buffer",    nums(_, _ => obj).to[mutable.Buffer[Object]](mutable.Buffer)),
      ("m.Map",       n => mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*)),
      ("m.Set",       nums(_, _ => obj).to[mutable.Set[Object]](mutable.Set)),
      ("m.Queue",     nums(_, _ => obj).to[mutable.Queue[Object]](mutable.Queue)),
      ("m.PriQueue",  nums(_, x=>x).to[mutable.PriorityQueue[Int]](mutable.PriorityQueue)),
      ("m.Stack",     nums(_, _ => obj).to[mutable.Stack[Object]](mutable.Stack)),
      ("m.SortedSet", nums(_, x=>x).to[mutable.SortedSet[Int]](mutable.SortedSet)),

      ("String",  "1" * _),

      ("ArrayBoolean",  nums(_, _ % 2 == 0).toArray),
      ("ArrayByte",     nums(_, _.toByte).toArray),
      ("ArrayShort",    nums(_, _.toShort).toArray),
      ("ArrayInt",      nums(_, _.toInt).toArray),
      ("ArrayLong",     nums(_, _.toLong).toArray),

      ("BoxArrayBoolean", nums(_, x => (x % 2 == 0).asInstanceOf[AnyRef]).toArray),
      ("BoxArrayByte",    nums(_, _.toByte.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayShort",   nums(_, _.toShort.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayInt",     nums(_, _.toInt.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayLong",    nums(_, _.toLong.asInstanceOf[AnyRef]).toArray),

      ("j.List",    nums(_, _.toLong.asInstanceOf[AnyRef]).toBuffer.asJava: java.util.List[AnyRef]),
      ("j.Map",       n => mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*).asJava: java.util.Map[AnyRef, AnyRef]),
      ("j.Set",     nums(_, _ => obj).to[mutable.Set[Object]](mutable.Set).asJava: java.util.Set[AnyRef])
    )
    val sizes = Seq(0, 1, 4, 16, 64, 256, 1024, 4069, 16192, 65536, 262144, 1048576)
    val results = for((name, factory) <- collections) yield {
      val numbers = for(n <- sizes) yield DeepSize(factory(n))
      (name, numbers)
    }

    def printRow[I: Integral](name: String, items: Seq[I]) = {
      val width = 15
      println(
        name.padTo(width, ' ') +
        items.map(NumberFormat.getNumberInstance(Locale.US).format)
             .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    printRow("Size", sizes)
    println()
    for((name, numbers) <- results){
      printRow(name, numbers)
    }
  }
}

