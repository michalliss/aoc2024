package aoc2024

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.stream.*

import NoWhitespace.*
import zio.ZIO
import scala.collection.immutable.Queue

object Day9 {

  enum BlockType:
    case Empty
    case Data(id: Int)

  case class Block(t: BlockType, size: Int) {
    def idOption = t match {
      case BlockType.Data(id) => Some(id)
      case _                  => None
    }

    def replace(b: Block): List[Block] =
      if (b.size < size) then List(b, Block(BlockType.Empty, size - b.size))
      else List(b)
  }

  case class Problem(fileLayout: List[Block]) {
    def flatten = fileLayout.flatMap(x => List.fill(x.size)(x.t))
  }

  object parsing {
    def number[$: P] = P(CharIn("0-9").!.map(_.toInt))
    def block[$: P]  = P(Index ~ number).map { case (i, s) => { Block(if i % 2 == 0 then BlockType.Data(i / 2) else BlockType.Empty, s) } }
    def parser[$: P] = block.rep.map(_.toList).map(Problem(_))

    def parseOpt(stream: ParserInputSource) = parse(stream, c => parser(using c)) match
      case Success(value, index) => Some(value)
      case x: Failure            => { println(x); None }
  }

  object part1 {
    def compact(disk: List[BlockType]) = {
      def loop(disk: List[BlockType], index: Int, toTake: List[(BlockType, Int)], acc: Queue[BlockType]): List[BlockType] = {
        (disk, toTake) match
          case (h1 :: t1, (h2, i2) :: t2) =>
            if (index > i2) then loop(t1, index + 1, toTake, acc :+ BlockType.Empty)
            else
              h1 match
                case BlockType.Empty    => loop(t1, index + 1, t2, acc :+ h2)
                case BlockType.Data(id) => loop(t1, index + 1, toTake, acc :+ h1)
          case _                          => acc.toList
      }

      loop(disk, 0, disk.zipWithIndex.collect { case (x: BlockType.Data, i) => (x, i) }.reverse, Queue.empty)
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream  <- file.toInputStream
      problem <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield compact(problem.flatten).zipWithIndex.foldLeft(0L)((acc, x) => {
      x._1 match
        case BlockType.Empty    => acc
        case BlockType.Data(id) => acc + x._2 * id
    })
  }

  object part2 {
    // based on https://github.com/nikiforo/aoc24/blob/main/src/main/scala/io/github/nikiforo/aoc24/D9T2.scala
    // spend too much time to debug my own solution

    def compact(disk: List[Block]) = {
      def findFirstFreeSpaceOf(disk: Vector[Block], size: Int) = {
        disk.zipWithIndex.find(x => x._1.t == BlockType.Empty && x._1.size >= size)
      }

      def loop(disk: Vector[Block], acc: List[Block]): List[Block] = {
        disk.lastOption match
          case None       => acc
          case Some(last) => {
            last match
              case Block(BlockType.Empty, size)    => loop(disk.init, last :: acc)
              case Block(BlockType.Data(id), size) => {
                findFirstFreeSpaceOf(disk, size) match
                  case None           => loop(disk.init, last :: acc)
                  case Some((b, ind)) => {
                    val replaced = disk.take(ind) ++ disk(ind).replace(last) ++ disk.drop(ind + 1).init
                    loop(replaced, Block(BlockType.Empty, last.size) :: acc)
                  }
              }
          }
      }

      loop(disk.toVector, Nil)
    }

    def solve(file: ZStream[Any, Nothing, Byte]) = for {
      stream  <- file.toInputStream
      problem <- ZIO.fromOption(parsing.parseOpt(stream))
    } yield Problem(compact(problem.fileLayout)).flatten.zipWithIndex.foldLeft(0L)((acc, x) => {
      x._1 match
        case BlockType.Empty    => acc
        case BlockType.Data(id) => acc + x._2 * id
    })
  }
}
