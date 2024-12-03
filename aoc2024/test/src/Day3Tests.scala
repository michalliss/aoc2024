package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day3

object Day3Tests extends ZIOSpecDefault {

  val example  = (ZStream.fromResource("day3/example.txt")).orDie
  val data     = (ZStream.fromResource("day3/data.txt")).orDie
  val example2 = (ZStream.fromResource("day3/example2.txt")).orDie

  def spec = suite("Day 3")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day3.part1.solve(example)
        } yield (assert(res)(equalTo(161)))
      },
      test("Data") {
        for {
          res <- Day3.part1.solve(data)
        } yield (assert(res)(equalTo(161289189)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day3.part2.solve(example2)
        } yield (assert(res)(equalTo(48)))
      },
      test("Data") {
        for {
          res <- Day3.part2.solve(data)
        } yield (assert(res)(equalTo(83595109)))
      }
    )
  )
}
