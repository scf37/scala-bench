package bench

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*
case class Benchmark(name: String,
                     cases: Benchmark.Case[_]*)
object Benchmark{
  case class Case[T](name: String,
                     initializer: Int => T)
                    (callback: T => Any){
    def run(n: Int) = callback(initializer(n))
  }
  def pair[T](t: => T) = (t, t)
  val nullO: Object = null
  def obj = new Object()
  val benchmarks = Seq(
    Benchmark(
      "construct",
      Case("List", n=>n){ n =>
        var b = List.empty[Object]
        var i = 0
        while(i < n){
          b = obj :: b
          i += 1
        }
        b
      },
      Case("Vector", n=>n){ n =>
        var b = Vector.empty[Object]
        var i = 0
        while(i < n){
          b = obj +: b
          i += 1
        }
        b
      },
      Case("Set", n=>n){ n =>
        var b = Set.empty[Object]
        var i = 0
        while(i < n){
          b = b + obj
          i += 1
        }
        b
      },
      Case("Map", n=>n){ n =>
        var b = Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b = b + (obj -> obj)
          i += 1
        }
        b
      },
      Case("Array:+", n=>n){ n =>
        var b = new Array[Object](0)
        var i = 0
        while(i < n){
          b = b :+ obj
          i += 1
        }
        b
      },
      Case("Array-prealloc", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        b
      },
      Case("Array.toSet", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        b.toSet
      },
      Case("Array.toVector", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        b.toVector
      },
      Case("Array.toMap", n=>n){ n =>
        val b = new Array[(Object, Object)](n)
        var i = 0
        while(i < n){
          b(i) = (obj, obj)
          i += 1
        }
        b.toMap
      },
      Case("m.Buffer", n=>n){ n =>
        val b = mutable.Buffer.empty[Object]
        var i = 0
        while(i < n){
          b.append(obj)
          i += 1
        }
        b
      },
      Case("m.Set", n=>n){ n =>
        val b = mutable.Set.empty[Object]
        var i = 0
        while(i < n){
          b.add(obj)
          i += 1
        }
        b
      },
      Case("m.Map", n=>n){ n =>
        val b = mutable.Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b.put(obj, obj)
          i += 1
        }
        b
      },
      Case("j.ArrayList", n => n) { n =>
        val b = java.util.ArrayList[Object]
        var i = 0
        while (i < n) {
          b.add(obj)
          i += 1
        }
        b
      },
      Case("j.HashSet", n => n) { n =>
        val b = java.util.HashSet[Object]
        var i = 0
        while (i < n) {
          b.add(obj)
          i += 1
        }
        b
      },
      Case("j.HashMap", n => n) { n =>
        val b = java.util.HashMap[Object, Object]
        var i = 0
        while (i < n) {
          b.put(obj, obj)
          i += 1
        }
        b
      }
    ),
    Benchmark(
      "deconstruct",
      Case("List.tail", List.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("Vector.tail", Vector.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("Vector.init", Vector.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.init
        x
      },
      Case("Set.-", Array.fill(_)(obj).toSet){ a =>
        var x = a
        while(x.nonEmpty) x = x - x.head
        x
      },
      Case("Map.-", Array.fill(_)(obj -> obj).toMap){ a =>
        var x = a
        while(x.nonEmpty) x = x.-(x.head._1)
        x
      },
      Case("Array.tail", Array.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("m.Buffer.remove", x => mutable.Buffer.fill(x)(obj)){ a =>
        while (a.nonEmpty) a.remove(a.length-1)
        a
      },
      Case("m.Set.remove", Array.fill(_)(obj).to(mutable.Set).pipe(c => c -> c.toArray)){ case (a, keys) =>
        var i = 0
        while (i < keys.length) {
          a.remove(keys(i))
          i += 1
        }
        a
      },
      Case("m.Map.remove", x => mutable.Map(Array.fill(x)(obj -> obj):_*).pipe(c => c -> c.keysIterator.toArray)){ case (a, keys) =>
        var i = 0
        while (i < keys.length) {
          a.remove(keys(i))
          i += 1
        }
        a
      },
      Case("j.ArrayList.remove", x => mutable.Buffer.fill(x)(obj).asJava) { a =>
        while (!a.isEmpty) a.remove(a.size() - 1)
        a
      },
      Case("j.HashSet.remove", Array.fill(_)(obj).to(mutable.Set).asJava.pipe(c => c -> c.toArray)) { case (a, keys) =>
        var i = 0
        while (i < keys.length) {
          a.remove(keys(i))
          i += 1
        }
        a
      },
      Case("j.HashMap.remove", x => mutable.Map(Array.fill(x)(obj -> obj): _*).asJava.pipe(c => c -> c.keySet().toArray)) { case (a, keys) =>
        var i = 0
        while (i < keys.length) {
          a.remove(keys(i))
          i += 1
        }
        a
      }

    ),
    Benchmark(
      "concat",
      Case("List", x => pair(List.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("Vector", x => pair(Vector.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("Set", x => pair(Array.fill(x)(obj).toSet)){ case (a, b) =>
        a ++ b
      },
      Case("Map", x => pair(Array.fill(x)(obj -> obj).toMap)){ case (a, b) =>
        a ++ b
      },
      Case("Array++", x => pair(Array.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("Array-arraycopy", x => pair(Array.fill(x)(obj))){ case (a, b) =>
        val x = new Array[Object](a.length + b.length)
        System.arraycopy(a, 0, x, 0, a.length)
        System.arraycopy(b, 0, x, a.length, b.length)
        x
      },
      Case("m.Buffer", x => pair(mutable.Buffer.fill(x)(obj))){ case (a, b) =>
        a.appendAll(b)
      },
      Case("m.Set", x => pair(Array.fill(x)(obj).to(mutable.Set))){ case (a, b) =>
        a ++= b
      },
      Case("m.Map", x => pair(mutable.Map(Array.fill(x)(obj -> obj):_*))){ case (a, b) =>
        a ++= b
      },
      Case("j.ArrayList", x => pair(mutable.Buffer.fill(x)(obj).asJava)) { case (a, b) =>
        a.addAll(b)
      },
      Case("j.HashSet", x => pair(Array.fill(x)(obj).to(mutable.Set).asJava)) { case (a, b) =>
        a.addAll(b)
      },
      Case("j.HashMap", x => pair(mutable.Map(Array.fill(x)(obj -> obj): _*).asJava)) { case (a, b) =>
        a.putAll(b)
      }
    ),
    Benchmark(
      "foreach",
      Case("List", List.fill(_)(obj)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("List-while", List.fill(_)(obj)){ case a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          var j = a
          while(j.nonEmpty){
            last = j.head
            j = j.tail
          }
          i += 1
        }
        last
      },
      Case("Vector", Vector.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Vector-while", Vector.fill(_)(obj)) { a =>
        var i = 0
        var last = nullO
        while (i < 10) {
          var j = 0
          while (j < a.size) {
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("Set", Array.fill(_)(obj).toSet){ a =>
        var i = 0
        var last = nullO
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Map", Array.fill(_)(obj -> obj).toMap){ a =>
        var i = 0
        var last = nullO
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Array", Array.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Array-while", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var i = 0
        while(i < 10) {
          var j = 0
          while(j < n){
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Buffer", x => mutable.Buffer.fill(x)(obj)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("m.Buffer-while", x => mutable.Buffer.fill(x)(obj)) { a =>
        var last = nullO
        var i = 0
        while (i < 10) {
          var j = 0
          while (j < a.size) {
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Set", Array.fill(_)(obj).to(mutable.Set)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("m.Map", x => mutable.Map(Array.fill(x)(obj -> obj):_*)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("j.ArrayList", x => mutable.Buffer.fill(x)(obj).asJava) { a =>
        var last = nullO
        var i = 0
        while (i < 10) {
          a.forEach(last = _)
          i += 1
        }
        last
      },
      Case("j.ArrayList-while", x => mutable.Buffer.fill(x)(obj).asJava) { a =>
        var last = nullO
        var i = 0
        while (i < 10) {
          var j = 0
          while (j < a.size) {
            last = a.get(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("j.HashSet", Array.fill(_)(obj).to(mutable.Set).asJava) { a =>
        var last = nullO
        var i = 0
        while (i < 10) {
          a.forEach(last = _)
          i += 1
        }
        last
      },
      Case("j.HashMap", x => mutable.Map(Array.fill(x)(obj -> obj): _*).asJava) { a =>
        var last = nullO
        var i = 0
        while (i < 10) {
          a.forEach((k, v) => last = k)
          i += 1
        }
        last
      }
    ),
    Benchmark(
      "lookup",
      Case("List", x => x -> List.fill(x)(obj)){ case (n, a) =>
        var i = 0
        var last = nullO
        while(i < 100) {
          var j = 0
          while (j < n) {
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("Vector", x => x -> Vector.fill(x)(obj)){ case (n, a) =>

        var last = nullO
        var i = 0
        while(i < 100) {
          var j = 0
          while (j < n) {
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.toSet
      }){ case (keys, a) =>

        var last = false
        var i = 0
        while(i < 100) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            j += 1
          }
          i += 1
        }
        last
      },
      Case("Map", x => {
        val r = Array.fill(x)(obj -> obj).toMap
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var last = nullO
        var i = 0
        while(i < 100) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            j += 1
          }
          i += 1
        }
        last
      },
      Case("Array", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var i = 0
        while(i < 100) {
          var j = 0
          while(j < n){
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Buffer", x => x -> mutable.Buffer.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var i = 0
        while(i < 100) {
          var j = 0
          while (j < n) {
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.to(mutable.Set)
      }){ case (keys, a) =>
        var last = false
        var i = 0
        while(i < 100) {
          val n = keys.length
          var j = 0
          while (j < n) {
            last = a(keys(j))
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Map", x => {
        val r = mutable.Map(Array.fill(x)(obj -> obj):_*)
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var last = nullO
        var i = 0
        while(i < 100) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            j += 1
          }
          i += 1
        }
        last
      },
      Case("j.ArrayList", x => x -> mutable.Buffer.fill(x)(obj).asJava) { case (n, a) =>
        var last = nullO
        var i = 0
        while (i < 100) {
          var j = 0
          while (j < n) {
            last = a.get(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("j.HashSet", x => {
        val r = Array.fill(x)(obj)
        r -> r.to(mutable.Set).asJava
      }) { case (keys, a) =>
        var last = false
        var i = 0
        while (i < 100) {
          val n = keys.length
          var j = 0
          while (j < n) {
            last = a.contains(keys(j))
            j += 1
          }
          i += 1
        }
        last
      },
      Case("j.HashMap", x => {
        val r = mutable.Map(Array.fill(x)(obj -> obj): _*).asJava
        r.keySet().toArray -> r
      }) { case (keys, a) =>
        var last = nullO
        var i = 0
        while (i < 100) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a.get(keys(j))
            j += 1
          }
          i += 1
        }
        last
      }
    )
  )

}
