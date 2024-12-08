package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day7

object Day7Tests extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day7/example.txt")).orDie
  val data    = (ZStream.fromResource("day7/data.txt")).orDie

  def spec = suite("Day 7")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day7.part1.solve(example)
        } yield (assert(res)(equalTo(3749)))
      },
      test("Data") {
        for {
          res <- Day7.part1.solve(data)
        } yield (assert(res)(equalTo(3245122495150L)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day7.part2.solve(example)
        } yield (assert(res)(equalTo(11387)))
      },
      test("Data") {
        for {
          res <- Day7.part2.solve(data)
        } yield (assert(res)(equalTo(105517128211543L)))
      }
    )
  )
}
