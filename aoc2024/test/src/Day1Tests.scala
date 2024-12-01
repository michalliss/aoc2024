package aoc2024.test
import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

import aoc2024.Day1

object Day1Tests extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day1/example.txt") >>> ZPipeline.utfDecode >>> ZPipeline.splitLines).orDie
  val data    = (ZStream.fromResource("day1/data.txt") >>> ZPipeline.utfDecode >>> ZPipeline.splitLines).orDie

  def spec = suite("Day 1")(
    suite("Part 1")(
      test("Example") {
        for {
          res <- Day1.part1.solve(example)
        } yield (assert(res)(equalTo(11)))
      },
      test("Data") {
        for {
          res <- Day1.part1.solve(data)
        } yield (assert(res)(equalTo(1660292)))
      }
    ),
    suite("Part 2")(
      test("Example") {
        for {
          res <- Day1.part2.solve(example)
        } yield (assert(res)(equalTo(31)))
      },
      test("Data") {
        for {
          res <- Day1.part2.solve(data)
        } yield (assert(res)(equalTo(22776016)))
      }
    )
  )
}
