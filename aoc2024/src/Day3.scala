package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import java.io.InputStream

import NoWhitespace.*

object Day3 {

  object part1 {
    object parsing {
      case class Mul(left: Int, right: Int) { def result = left * right }

      def number[$: P]  = P(CharIn("0-9").rep(1).!.map(_.toInt))
      def mul[$: P]     = P("mul(" ~ number ~ "," ~ number ~ ")").map { case (a, b) => Mul(a, b) }
      def garbage[$: P] = P((!mul ~ AnyChar).rep)
      def parser[$: P]  = P(garbage ~ mul ~ garbage).rep

      def parseOpt(stream: InputStream) = parse(stream, c => parser(using c)) match
        case Success(value, index) => Some(value)
        case _: Failure            => None
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream <- file.toInputStream
      parsed <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield parsed.map(_.result).sum
  }

  object part2 {
    sealed trait Instruction
    case class Mul(left: Int, right: Int) extends Instruction { def result = left * right }
    case object Do                        extends Instruction
    case object Dont                      extends Instruction

    object parsing {
      def number[$: P]      = P(CharIn("0-9").rep(1).!.map(_.toInt))
      def mul[$: P]         = P("mul(" ~ number ~ "," ~ number ~ ")").map { case (a, b) => Mul(a, b) }
      def `do`[$: P]        = P("do()").map(_ => Do)
      def dont[$: P]        = P("don't()").map(_ => Dont)
      def instruction[$: P] = P(mul | `do` | dont)
      def garbage[$: P]     = P((!instruction ~ AnyChar).rep)
      def parser[$: P]      = P(garbage ~ instruction ~ garbage).rep

      def parseOpt(stream: InputStream) = parse(stream, c => parser(using c)) match
        case Success(value, index) => Some(value)
        case _: Failure            => None
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream <- file.toInputStream
      parsed <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield {
      case class Acc(sum: Int, enabled: Boolean) {
        def enable             = this.copy(enabled = true)
        def disable            = this.copy(enabled = false)
        def addSum(value: Int) = this.copy(sum = sum + value)
      }

      parsed
        .foldLeft(Acc(0, true))((acc, elem) =>
          elem match
            case mul: Mul => if acc.enabled then acc.addSum(mul.result) else acc
            case Do       => acc.enable
            case Dont     => acc.disable
        )
        .sum
    }
  }
}
