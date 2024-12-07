package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import java.io.InputStream
import scala.annotation.tailrec

import NoWhitespace.*

object Day6 {
  object Ground
  object Wall
  case class Guard(direction: Direction)

  enum Direction:
    case Up, Down, Left, Right
    def rotateRight = this match
      case Up    => Right
      case Down  => Left
      case Left  => Up
      case Right => Down

  type Node             = Ground.type | Wall.type | Guard
  type NodeWithoutGuard = Ground.type | Wall.type

  case class Pos(x: Int, y: Int)

  case class WorldMap(map: Array[Array[Node]]) {
    val width  = map(0).length
    val height = map.length

    def isValid(pos: Pos) = pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height
    def at(pos: Pos)      = map(pos.y)(pos.x)

    lazy val withoutGuard: Array[Array[NodeWithoutGuard]] = map.map(
      _.map(x =>
        x match
          case Ground           => Ground
          case Wall             => Wall
          case Guard(direction) => Ground
      )
    )

    lazy val positions = for { y <- map.indices; x <- map(y).indices } yield Pos(x, y)
    lazy val walls     = positions.collect { case pos if at(pos) == Wall => pos }
    lazy val grounds   = positions.collect { case pos if at(pos) == Ground => pos }

    lazy val (guardPosition, guardDirection) = positions
      .map(x => (x, at(x)))
      .collectFirst { case (pos, Guard(direction)) => (pos, direction) }
      .get

    def replace(pos: Pos, node: Node) = {
      val newMap = map.clone()
      newMap(pos.y) = newMap(pos.y).clone()
      newMap(pos.y)(pos.x) = node
      WorldMap(newMap)
    }

    def nextPos(pos: Pos, direction: Direction) = direction match
      case Direction.Up    => Pos(pos.x, pos.y - 1)
      case Direction.Down  => Pos(pos.x, pos.y + 1)
      case Direction.Left  => Pos(pos.x - 1, pos.y)
      case Direction.Right => Pos(pos.x + 1, pos.y)

    def willHitWall(pos: Pos, nextPos: Pos) = {
      isValid(nextPos) && withoutGuard(nextPos.y)(nextPos.x) == Wall
    }

    def willExitMap(pos: Pos, nextPos: Pos) = {
      !isValid(nextPos)
    }

    enum NextStepResult:
      case OutOfMap
      case HitWall
      case Moved(next: Pos)

    def nextStep(pos: Pos, direction: Direction) = {
      val nexPos = nextPos(pos, direction)
      if !isValid(nexPos) then NextStepResult.OutOfMap
      else if willHitWall(pos, nexPos) then NextStepResult.HitWall
      else NextStepResult.Moved(nexPos)
    }

    def visitedByGuard = {
      @tailrec
      def loop(pos: Pos, direction: Direction, visited: Set[Pos]): Set[Pos] = {
        nextStep(pos, direction) match
          case NextStepResult.OutOfMap    => visited + pos
          case NextStepResult.HitWall     => loop(pos, direction.rotateRight, visited + pos)
          case NextStepResult.Moved(next) => loop(next, direction, visited + pos)
      }

      loop(guardPosition, guardDirection, Set())
    }

    def isLooping = {
      @tailrec
      def loop(pos: Pos, direction: Direction, visited: Set[(Pos, Direction)]): Boolean = {
        if visited.contains((pos, direction)) then true
        else
          nextStep(pos, direction) match
            case NextStepResult.OutOfMap    => false
            case NextStepResult.HitWall     => loop(pos, direction.rotateRight, visited + ((pos, direction)))
            case NextStepResult.Moved(next) => loop(next, direction, visited + ((pos, direction)))
      }

      loop(guardPosition, guardDirection, Set())
    }
  }

  object parsing {
    def number[$: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def ground[$: P] = P(".").map(_ => Ground)
    def wall[$: P]   = P("#").map(_ => Wall)
    def guard[$: P]  = P("^".! | "v".! | "<".! | ">".!).map(x =>
      Guard(x match
        case "^" => Direction.Up
        case "v" => Direction.Down
        case "<" => Direction.Left
        case ">" => Direction.Right
      )
    )
    def node[$: P]   = P(ground | wall | guard)
    def row[$: P]    = P(node.rep(1)).map(_.toArray)
    def parser[$: P] = (row.rep(1, "\n")).map(x => WorldMap(x.toArray))

    def parseOpt(input: InputStream) = parse(input, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  object part1 {
    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream  <- file.toInputStream
      problem <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield problem.visitedByGuard.size
  }

  object part2 {
    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream        <- file.toInputStream
      problem       <- ZIO.fromOption(parsing.parseOpt(stream))
      nodesToReplace = problem.grounds.toSet intersect problem.visitedByGuard
      looping       <- ZIO.foreachPar(nodesToReplace.toList)(x => ZIO.succeed(problem.replace(x, Wall).isLooping))
    } yield looping.count(_ == true)
  }
}
