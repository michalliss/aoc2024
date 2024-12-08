package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import java.io.InputStream

import NoWhitespace.*

object Day7 {

  object part1 {
    case class Result(value: Long)
    case class ProblemLine(result: Result, expr: OpTree)
    case class Problem(lines: List[ProblemLine])

    enum OpTree {
      case Value(value: Long)
      case Add(left: OpTree, right: OpTree)
      case Multiply(left: OpTree, right: OpTree)
      case Placeholder(left: OpTree, right: OpTree)

      def eval: Option[Long] = this match
        case Value(value)             => Some(value)
        case Add(left, right)         => for { l <- left.eval; r <- right.eval } yield l + r
        case Multiply(left, right)    => for { l <- left.eval; r <- right.eval } yield l * r
        case Placeholder(left, right) => None

      def replacedPlaceholders: List[OpTree] = this match
        case Value(value)             => List(Value(value))
        case Add(left, right)         =>
          for {
            lr <- left.replacedPlaceholders
            rr <- right.replacedPlaceholders
          } yield Add(lr, rr)
        case Multiply(left, right)    =>
          for {
            lr <- left.replacedPlaceholders
            rr <- right.replacedPlaceholders
          } yield Multiply(lr, rr)
        case Placeholder(left, right) =>
          for {
            lr  <- left.replacedPlaceholders
            rr  <- right.replacedPlaceholders
            res <- List(Add(lr, rr), Multiply(lr, rr))
          } yield res
    }

    object parsing {
      def number[$: P]      = P(CharIn("0-9").rep(1).!.map(_.toLong))
      def result[$: P]      = P(number).map(Result.apply)
      def const[$: P]       = P(number).map(OpTree.Value.apply)
      def expr[$: P]        = P(const ~ (" " ~ const).rep).map(x => x._2.foldLeft(x._1)((acc, e) => OpTree.Placeholder(acc, e)))
      def problemLine[$: P] = P(result ~ ": " ~ expr).map(ProblemLine.apply.tupled)
      def problem[$: P]     = P(problemLine.rep(sep = "\n")).map(x => Problem(x.toList))

      def parseOpt(stream: InputStream) = parse(stream, c => problem(using c)) match
        case Success(value, index) => Some(value)
        case _: Failure            => None
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream  <- file.toInputStream
      problem <- ZIO.fromOption(parsing.parseOpt(stream))
      result  <- ZIO
                   .foreachPar(problem.lines)(p => ZIO.attempt(p.expr.replacedPlaceholders.flatMap(_.eval).find(_ == p.result.value)))
                   .map(_.flatten)
                   .map(_.sum)
    } yield result
  }

  object part2 {
    case class Result(value: Long)
    case class ProblemLine(result: Result, expr: OpTree)
    case class Problem(lines: List[ProblemLine])

    enum OpTree {
      case Value(value: Long)
      case Add(left: OpTree, right: OpTree)
      case Multiply(left: OpTree, right: OpTree)
      case Concat(left: OpTree, right: OpTree)
      case Placeholder(left: OpTree, right: OpTree)

      def eval: Option[Long] = this match
        case Value(value)             => Some(value)
        case Add(left, right)         => for { l <- left.eval; r <- right.eval } yield l + r
        case Multiply(left, right)    => for { l <- left.eval; r <- right.eval } yield l * r
        case Concat(left, right)      => for { l <- left.eval; r <- right.eval } yield (l.toString() + r.toString()).toLong
        case Placeholder(left, right) => None

      def replacedPlaceholders: LazyList[OpTree] = this match
        case Value(value)             => LazyList(Value(value))
        case Add(left, right)         =>
          for {
            lr <- left.replacedPlaceholders
            rr <- right.replacedPlaceholders
          } yield Add(lr, rr)
        case Multiply(left, right)    =>
          for {
            lr <- left.replacedPlaceholders
            rr <- right.replacedPlaceholders
          } yield Multiply(lr, rr)
        case Concat(left, right)      =>
          for {
            lr <- left.replacedPlaceholders
            rr <- right.replacedPlaceholders
          } yield Concat(lr, rr)
        case Placeholder(left, right) =>
          for {
            lr  <- left.replacedPlaceholders
            rr  <- right.replacedPlaceholders
            res <- LazyList(Add(lr, rr), Multiply(lr, rr), Concat(lr, rr))
          } yield res
    }

    object parsing {
      def number[$: P]      = P(CharIn("0-9").rep(1).!.map(_.toLong))
      def result[$: P]      = P(number).map(Result.apply)
      def const[$: P]       = P(number).map(OpTree.Value.apply)
      def expr[$: P]        = P(const ~ (" " ~ const).rep).map(x => x._2.foldLeft(x._1)((acc, e) => OpTree.Placeholder(acc, e)))
      def problemLine[$: P] = P(result ~ ": " ~ expr).map(ProblemLine.apply.tupled)
      def problem[$: P]     = P(problemLine.rep(sep = "\n")).map(x => Problem(x.toList))

      def parseOpt(stream: InputStream) = parse(stream, c => problem(using c)) match
        case Success(value, index) => Some(value)
        case _: Failure            => None
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream  <- file.toInputStream
      problem <- ZIO.fromOption(parsing.parseOpt(stream))
      result  <- ZIO
                   .foreachPar(problem.lines)(p => ZIO.attempt(p.expr.replacedPlaceholders.flatMap(_.eval).find(_ == p.result.value)))
                   .map(_.flatten)
                   .map(_.sum)
    } yield result
  }
}
