package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day9

object Day9Tests extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day9/example.txt")).orDie
  val data    = (ZStream.fromResource("day9/data.txt")).orDie

  def spec = suite("Day 9")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day9.part1.solve(example)
        } yield (assert(res)(equalTo(1928)))
      },
      test("Data") {
        for {
          res <- Day9.part1.solve(data)
        } yield (assert(res)(equalTo(6337921897505L)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day9.part2.solve(example)
        } yield (assert(res)(equalTo(2858)))
      },
      test("Data") {
        for {
          res <- Day9.part2.solve(data)
        } yield (assert(res)(equalTo(6362722604045L)))
      }
    )
  )
}
