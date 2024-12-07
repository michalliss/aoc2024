package aoc2024

import fastparse.*
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import zio.*
import zio.stream.*

import NoWhitespace.*
import java.io.InputStream

import scalax.collection.edges.~>
import scalax.collection.edges.labeled._
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.immutable.Graph

object Day5 {
  case class Rule(number: Int, successor: Int)

  case class Update(numbers: List[Int]):
    def middleElement = numbers(numbers.size / 2)

  case class Problem(rules: List[Rule], updates: List[Update]) {
    def successorsOf(x: Int) = rules.filter(_.number == x).foldLeft(Set.empty[Int])(_ + _.successor)
    def predecesorsGraph     = Graph.from(rules.map(r => r.successor ~> r.number))
    def validUpdates         = updates.filter(isUpdateValid)
    def invalidUpdates       = updates.filter(!isUpdateValid(_))

    private def isUpdateValid(update: Update) = {
      def helper(list: List[Int], mustContain: Set[Int], visited: Set[Int]): Boolean = {
        list match
          case Nil       => visited.intersect(mustContain).isEmpty
          case e :: tail =>
            if (mustContain.contains(e))
            then helper(tail, (mustContain - e) union successorsOf(e), visited + e)
            else helper(tail, mustContain union successorsOf(e), visited + e)
      }
      helper(update.numbers, Set(), Set())
    }

  }

  object parsing {
    def number[$: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def rule[$: P]   = P(number ~ "|" ~ number).map(x => Rule(x._1, x._2))
    def update[$: P] = number.rep(1, ",").map(_.toList).map(Update.apply)
    def parser[$: P] = (rule.rep(sep = "\n") ~ "\n\n" ~ update.rep(sep = "\n")).map(x => Problem(x._1.toList, x._2.toList))

    def parseOpt(input: InputStream) = parse(input, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  object part1 {
    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream <- file.toInputStream
      parsed <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield parsed.validUpdates.map(_.middleElement).sum
  }

  object part2 {
    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream         <- file.toInputStream
      problem        <- ZIO.fromOption(parsing.parseOpt(stream))
      correctedUdates = problem.invalidUpdates
                          .map(update => {
                            val graph         = problem.predecesorsGraph.filter(x => update.numbers.contains(x.outer))
                            val last          = graph.nodes.find(_.inDegree == 0).get
                            val correctUpdate = Update(last.topologicalSort().right.get.toList.reverse.map(_.outer).toList)
                            correctUpdate.middleElement
                          })
    } yield correctedUdates.sum
  }
}
