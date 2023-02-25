package bench

import java.nio.file.{Files, Paths}
import java.text.NumberFormat
import java.time.Duration
import java.util.Locale
import scala.jdk.CollectionConverters.*
import scala.collection.immutable.Queue
import scala.collection.{SortedSet, mutable}


object PerfMain{

  def main(args: Array[String]): Unit = {

    def printRow[I: Integral](name: String, items: Seq[I]) = {
      val width = 15
      println(
        name.padTo(width, ' ') +
        items.map(NumberFormat.getNumberInstance(Locale.US).format)
             .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    // How large the collections will be in each benchmark
    val sizes = Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)
    // How many times to repeat each benchmark
    val repeats = 7
    // How long each benchmark runs, in millis
    val durationNs = Duration.ofMillis(2000).toNanos
    // How long a benchmark can run before we stop incrementing it
    val cutoffNs = Duration.ofMillis(2000).toNanos

    printRow("Size", sizes)
    val output = mutable.Map.empty[(String, String, Long), mutable.Buffer[Long]]
    val cutoffSizes = mutable.Map.empty[(String, String), Int]
    for(i <- 1 to repeats){
      println("Run " + i)
      for(benchmark <- Benchmark.benchmarks){
        println()
        println(benchmark.name)
        println()
        for (bench <- benchmark.cases){
          val key = benchmark.name -> bench.name


          val times =
            for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield{
              val buf = output.getOrElseUpdate((benchmark.name, bench.name, size), mutable.Buffer())
              def handle(run: Boolean) = {
                System.gc()

                val start = System.nanoTime()
                var count = 0
                while(System.nanoTime() - start < durationNs){
                  if (run) bench.run(size)
                  else bench.initializer(size)
                  count += 1
                }
                val end = System.nanoTime()
                (count, end - start)
              }
              val (initCounts, initTime) = handle(run = false)
              val (runCounts, runTime) = handle(run = true)
              val res = runTime / runCounts - initTime / initCounts
              buf.append(res)
              if (res > cutoffNs) {
                cutoffSizes(key) = math.min(
                  cutoffSizes.getOrElse(key, Int.MaxValue),
                  size
                )
              }
              res
            }
          printRow(bench.name, times)
        }
      }
    }
    Files.writeString(Paths.get("target/results.json"), upickle.default.write(output.mapValues(_.toList).toMap))
  }
}

