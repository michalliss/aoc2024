package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day5

object Day5Tests extends ZIOSpecDefault {

  val example  = (ZStream.fromResource("day5/example.txt")).orDie
  val data     = (ZStream.fromResource("day5/data.txt")).orDie

  def spec = suite("Day 5")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day5.part1.solve(example)
        } yield (assert(res)(equalTo(143)))
      },
      test("Data") {
        for {
          res <- Day5.part1.solve(data)
        } yield (assert(res)(equalTo(5639)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day5.part2.solve(example)
        } yield (assert(res)(equalTo(123)))
      },
      test("Data") {
        for {
          res <- Day5.part2.solve(data)
        } yield (assert(res)(equalTo(5273)))
      }
    )
  )
}
