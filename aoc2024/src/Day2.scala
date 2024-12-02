package aoc2024

import fastparse.*
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import zio.*
import zio.Config.Bool
import zio.stream.*

import NoWhitespace.*

object Day2 {

  object parsing {
    def number[$: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def parser[$: P] = P(number ~ CharPred(_.isWhitespace).rep).rep.map(_.toList)

    def parseOpt(input: String) = parse(input, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  def extractLevels(lines: ZStream[Any, Nothing, String]) = lines.map(parsing.parseOpt).some

  extension (v: Int) infix def between(range: (Int, Int)) = { v >= range._1 && v <= range._2 }
  extension (v: Int) def sign                             = { Math.signum(v).toInt }

  object part1 {
    def isSafe(level: List[Int]) = {
      def helper(l: List[Int], sign: Int): Boolean = {
        l match
          case Nil              => true
          case _ :: Nil         => true
          case e1 :: e2 :: tail => ((e2 - e1).sign == sign) && (Math.abs(e2 - e1) between (1, 3)) && helper(e2 :: tail, sign)
      }

      helper(level, -1) || helper(level, 1)
    }

    def solve(lines: ZStream[Any, Nothing, String]) = extractLevels(lines).filter(isSafe(_)).runCount
  }

  object part2 {
    def isSafe(level: List[Int]) = {
      def helper(lastTaken: Int, next: List[Int], sign: Int, canSkip: Boolean): Boolean = {
        def checkAdjacent(a: Int, b: Int)                   = ((b - a).sign == sign) && (Math.abs(b - a) between (1, 3))
        def considerNoSkip(a: Int, b: Int, rest: List[Int]) = (checkAdjacent(a, b) && helper(b, rest, sign, canSkip))
        def considerSkip(a: Int, b: Int, rest: List[Int])   = (canSkip && checkAdjacent(a, b) && helper(b, rest, sign, false))

        next match
          case Nil            => true                                     // Always safe
          case a :: Nil       => canSkip || (checkAdjacent(lastTaken, a)) // Safe if can skip or elements fit the  rules
          case a :: b :: tail => considerNoSkip(lastTaken, a, b :: tail) || considerSkip(lastTaken, b, tail)
      }

      level match
        case e1 :: e2 :: tail =>
          helper(e1, e2 :: tail, -1, true) ||
          helper(e1, e2 :: tail, 1, true) ||
          helper(e2, tail, -1, false) ||
          helper(e2, tail, 1, false)
        case _                => true
    }

    def solve(lines: ZStream[Any, Nothing, String]) = extractLevels(lines).filter(isSafe(_)).runCount

  }
}
