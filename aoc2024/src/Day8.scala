package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import NoWhitespace.*

object Day8 {
  case class Position(row: Int, column: Int) {
    def +(p2: Position) = Position(row + p2.row, column + p2.column)
    def -(p2: Position) = Position(row - p2.row, column - p2.column)
    def /(d: Int)       = Position(row / d, column / d)
    def *(d: Int)       = Position(row * d, column * d)
  }

  case class Antenna(frequency: Char, position: Position)

  enum NodeType:
    case Antenna(frequency: Char)
    case Ground

  case class Map(width: Int, height: Int, map: Array[Array[NodeType]]) {
    def antennas: Set[Antenna] = {
      for {
        row <- 0 until height
        col <- 0 until width
      } yield map(row)(col) match
        case NodeType.Antenna(frequency) => Some(Antenna(frequency, Position(row, col)))
        case NodeType.Ground             => None
    }.collect { case Some(value) => value }.toSet

    def inBounds(pos: Position) = pos.row >= 0 && pos.row < height && pos.column >= 0 && pos.column < width
  }

  object parsing {
    def frequency[$: P] = P(CharPred(x => x != '.' && !x.isWhitespace).!.map(_.head))
    def antenna[$: P]   = P(frequency).map(x => NodeType.Antenna(x))
    def ground[$: P]    = P(CharIn(".")).map(_ => NodeType.Ground)
    def row[$: P]       = (antenna | ground).rep.map(_.toArray)
    def parser[$: P]    = (row.rep(1, "\n")).map(x => x.toArray).map(x => Map(x(0).length, x.length, x))

    def parseOpt(stream: ParserInputSource) = parse(stream, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case x: Failure            => None
  }

  object part1 {
    def antiNodes(a1: Antenna, a2: Antenna) = {
      val diff = a2.position - a1.position
      List(a1.position - diff, a2.position + diff)
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream <- file.toInputStream
      map    <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield map.antennas
      .groupBy(x => x.frequency)
      .map { case (k, v) =>
        (
          k,
          v.toList
            .combinations(2)
            .flatMap(x => antiNodes(x(0), x(1)))
            .filter(x => map.inBounds(x))
        )
      }
      .values
      .flatten
      .toSet
      .size
  }

  object part2 {
    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a else gcd(b, a % b)
    }

    def antiNodes2(a1: Antenna, a2: Antenna, inBounds: Position => Boolean) = {
      val diff       = a2.position - a1.position
      val step       = diff / gcd(Math.abs(diff.row), Math.abs(diff.column));
      val direction1 = LazyList.from(1).map(step * _).map(x => a1.position - x).takeWhile(inBounds)
      val direction2 = LazyList.from(1).map(step * _).map(x => a1.position + x).takeWhile(inBounds)
      LazyList(a1.position, a2.position) ++ direction1 ++ direction2
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream <- file.toInputStream
      map    <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield map.antennas
      .groupBy(x => x.frequency)
      .map { case (k, v) =>
        (
          k,
          v.toList
            .combinations(2)
            .flatMap(x => antiNodes2(x(0), x(1), map.inBounds))
            .filter(x => map.inBounds(x))
        )
      }
      .values
      .flatten
      .toSet
      .size
  }
}
