package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day4

object Day4Tests extends ZIOSpecDefault {

  val example  = (ZStream.fromResource("day4/example.txt")).orDie
  val data     = (ZStream.fromResource("day4/data.txt")).orDie
  val example2 = (ZStream.fromResource("day4/example2.txt")).orDie

  def spec = suite("Day 4")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day4.part1.solve(example)
        } yield (assert(res)(equalTo(18)))
      },
      test("Data") {
        for {
          res <- Day4.part1.solve(data)
        } yield (assert(res)(equalTo(2633)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day4.part2.solve(example2)
        } yield (assert(res)(equalTo(9)))
      },
      test("Data") {
        for {
          res <- Day4.part2.solve(data)
        } yield (assert(res)(equalTo(1936)))
      }
    )
  )
}
