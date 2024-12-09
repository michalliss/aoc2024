package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day8

object Day8Tests extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day8/example.txt")).orDie
  val data    = (ZStream.fromResource("day8/data.txt")).orDie

  def spec = suite("Day 8")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day8.part1.solve(example)
        } yield (assert(res)(equalTo(14)))
      },
      test("Data") {
        for {
          res <- Day8.part1.solve(data)
        } yield (assert(res)(equalTo(409)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day8.part2.solve(example)
        } yield (assert(res)(equalTo(34)))
      },
      test("Data") {
        for {
          res <- Day8.part2.solve(data)
        } yield (assert(res)(equalTo(1308)))
      }
    )
  )
}
