package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day2

object Day2Tests extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day2/example.txt") >>> ZPipeline.utfDecode >>> ZPipeline.splitLines).orDie
  val data    = (ZStream.fromResource("day2/data.txt") >>> ZPipeline.utfDecode >>> ZPipeline.splitLines).orDie

  def spec = suite("Day 2")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day2.part1.solve(example)
        } yield (assert(res)(equalTo(2)))
      },
      test("Data") {
        for {
          res <- Day2.part1.solve(data)
        } yield (assert(res)(equalTo(224)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day2.part2.solve(example)
        } yield (assert(res)(equalTo(4)))
      },
      test("Data") {
        for {
          res <- Day2.part2.solve(data)
        } yield (assert(res)(equalTo(293)))
      }
    )
  )
}
