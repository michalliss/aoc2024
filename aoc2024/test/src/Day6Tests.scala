package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day6

object Day6Tests extends ZIOSpecDefault {

  val example  = (ZStream.fromResource("day6/example.txt")).orDie
  val data     = (ZStream.fromResource("day6/data.txt")).orDie

  def spec = suite("Day 6")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day6.part1.solve(example)
        } yield (assert(res)(equalTo(41)))
      },
      test("Data") {
        for {
          res <- Day6.part1.solve(data)
        } yield (assert(res)(equalTo(4711)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day6.part2.solve(example)
        } yield (assert(res)(equalTo(6)))
      },
      test("Data") {
        for {
          res <- Day6.part2.solve(data)
        } yield (assert(res)(equalTo(1562)))
      }
    )
  )
}
