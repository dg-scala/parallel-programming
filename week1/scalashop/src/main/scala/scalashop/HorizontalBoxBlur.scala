package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      row <- from until end
      col <- 0 until src.width
    } yield dst(col, row) = boxBlurKernel(src, col, row, radius)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def parallelParameters(dimension: Int): (Int, Int) =
      if (numTasks <= 0 || dimension <= numTasks)
        (dimension, 1)
      else
        (numTasks, dimension / numTasks)

    def forkTask(start: Int, end: Int): ForkJoinTask[Unit] =
      task {
        blur(src, dst, start, end, radius)
      }

    val (size, taskSize) = parallelParameters(src.height)
    val tasks: Array[ForkJoinTask[Unit]] = new Array(size)

    var row = 0
    for ( i <- 0 until size) yield
      if (i < size - 1) {
        tasks.update(i, forkTask(row, row + taskSize))
        row += taskSize
      }
      else { // in the last iteration we grab all that's left
        tasks.update(i, forkTask(row, src.height))
      }

    // wait for tasks to complete
    for ( i <- 0 until size) yield tasks(i).join()
  }

}
