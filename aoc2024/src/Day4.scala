package aoc2024

import fastparse.*
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import zio.*
import zio.Config.Bool
import zio.stream.*

import NoWhitespace.*
import java.io.InputStream

object Day4 {

  object part1 {
    case class Position(x: Int, y: Int)

    case class SearchWindow(position: Position, size: Int) {
      def left      = (0 until size).map(x => Position(position.x, position.y - x))
      def right     = (0 until size).map(x => Position(position.x, position.y + x))
      def up        = (0 until size).map(x => Position(position.x - x, position.y))
      def down      = (0 until size).map(x => Position(position.x + x, position.y))
      def leftUp    = (0 until size).map(x => Position(position.x - x, position.y - x))
      def leftDown  = (0 until size).map(x => Position(position.x + x, position.y - x))
      def rightUp   = (0 until size).map(x => Position(position.x - x, position.y + x))
      def rightDown = (0 until size).map(x => Position(position.x + x, position.y + x))

      def allToCheck = List(left, right, up, down, leftUp, leftDown, rightUp, rightDown)
    }

    case class WordSearch(words: Array[Array[Char]], n: Int, m: Int) {
      def windowed(size: Int) = for {
        i <- 0 until n
        j <- 0 until m
      } yield SearchWindow(Position(i, j), size)

      def atOpt(pos: Position) = for {
        row  <- words.lift(pos.x)
        elem <- row.lift(pos.y)
      } yield elem

      def findOccurences(word: String) = windowed(word.length)
        .map(x => x.allToCheck.map(y => y.map(atOpt).collect { case Some(value) => value }.mkString).count(_ == word))
        .sum
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      wordSearch <- (file >>> ZPipeline.utfDecode >>> ZPipeline.splitLines)
                      .map(x => x.toCharArray().toArray)
                      .runCollect
                      .map(_.toArray)
                      .map(x => WordSearch.apply(x, x.length, x(0).length))
    } yield wordSearch.findOccurences("XMAS")
  }

  object part2 {
    case class Position(x: Int, y: Int)

    case class SearchWindow(position: Position, size: Int) {
      def leftUp    = (1 to size).map(x => Position(position.x - x, position.y - x))
      def leftDown  = (1 to size).map(x => Position(position.x + x, position.y - x))
      def rightUp   = (1 to size).map(x => Position(position.x - x, position.y + x))
      def rightDown = (1 to size).map(x => Position(position.x + x, position.y + x))

      def diagonal1 = leftUp ++ List(position) ++ rightDown
      def diagonal2 = leftDown ++ List(position) ++ rightUp
    }

    case class WordSearch(words: Array[Array[Char]], n: Int, m: Int) {
      def windowed(size: Int) = for {
        i <- 0 until n
        j <- 0 until m
      } yield SearchWindow(Position(i, j), size)

      def atOpt(pos: Position) = for {
        row  <- words.lift(pos.x)
        elem <- row.lift(pos.y)
      } yield elem

      def findMAS = windowed(1)
        .map(x => {
          val diagonalString  = x.diagonal1.map(atOpt).collect { case Some(value) => value }.mkString
          val diagonalString2 = x.diagonal2.map(atOpt).collect { case Some(value) => value }.mkString
          if (diagonalString == "MAS" || diagonalString == "SAM") && (diagonalString2 == "MAS" || diagonalString2 == "SAM") then 1 else 0
        })
        .sum
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      wordSearch <- (file >>> ZPipeline.utfDecode >>> ZPipeline.splitLines)
                      .map(x => x.toCharArray().toArray)
                      .runCollect
                      .map(_.toArray)
                      .map(x => WordSearch.apply(x, x.length, x(0).length))
    } yield wordSearch.findMAS
  }
}
