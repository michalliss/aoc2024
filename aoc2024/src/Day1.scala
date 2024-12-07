package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import NoWhitespace.*

object Day1 {

  object parsing {
    def number[$: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def parser[$: P] = P(number ~ CharPred(_.isWhitespace).rep ~ number)

    def parseOpt(input: String) = parse(input, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  def extractLists(lines: ZStream[Any, Nothing, String]) = lines
    .map(parsing.parseOpt)
    .some
    .runCollect
    .map(x => (x.map(_._1), x.map(_._2)))

  object part1 {
    def sumOfDistances(l1: Chunk[Int], l2: Chunk[Int]) = {
      l1.sorted.zip(l2.sorted).map { case (e1, e2) => Math.abs(e1 - e2) }.sum
    }

    def solve(lines: ZStream[Any, Nothing, String]) = for {
      (l1, l2) <- extractLists(lines)
    } yield sumOfDistances(l1, l2)
  }

  object part2 {
    def similarityScore(l1: Chunk[Int], l2: Chunk[Int]) = {
      l1.map(e1 => e1 * l2.count(_ == e1)).sum
    }

    def solve(lines: ZStream[Any, Nothing, String]) = for {
      (l1, l2) <- extractLists(lines)
    } yield similarityScore(l1, l2)
  }
}
