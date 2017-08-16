package com.tobiatesan.twist

import com.tobiatesan.twist.ai.minimax.{
  BasicAlphaBeta,
  KillerAlphaBeta,
  SimpleKillerAlphaBeta,
  QuiescenceAlphaBeta
}
import com.tobiatesan.twist.ai.mtcs.{UCTAgent}
import com.tobiatesan.twist.ai.{RandomAgent}
import com.tobiatesan.twist.draughts.{Draughts, LiveDraughtsPosition}
import com.tobiatesan.twist.matches.{Match, MatchLog, PlyEntry}
import com.tobiatesan.twist.game.{
  Move,
  Game,
  LivePosition,
  TerminalPosition,
  Min,
  Max
}
import com.tobiatesan.twist.player.{AI}
import java.io.File
import com.github.tototoshi.csv.CSVWriter
import scala.collection.parallel.immutable.ParSeq

package benchmark {

  abstract class Metric {
    def getHeader: Seq[String]
    def getRow: Seq[String]
  }

  case class BasicMetric(draw: Int,
    max: Int,
    nodesAvg: (Double, Double),
    totNodes: (Int, Int),
    rounds: Int)
      extends Metric {
    def getHeader =
      Seq("draw",
        "max",
        "maxonnondraw",
        "drawontot",
        "nodesAvg_min",
        "nodesAvg_max",
        "totNodes_min",
        "totNodes_max",
        "rounds")
    def getRow =
      Seq(
        draw.toString,
        max.toString,
        (max.toDouble / (rounds - draw).toDouble).toString,
        (draw.toDouble / rounds.toDouble).toString,
        nodesAvg._1.toString,
        nodesAvg._2.toString,
        totNodes._1.toString,
        totNodes._2.toString,
        rounds.toString
      )
  }

  abstract class Extractor[G <: Game[G]](t: Tournament[G]) {
    def extract(): Metric
  }

  class BasicExtractor[G <: Game[G]](t: Tournament[G]) extends Extractor[G](t) {
    def extract: BasicMetric = {
      val run = t.run().toList
      val (draw, max, nodesAvg, totNodes) = run
        .map((run) => {
          val split = Seq(run.plys.sliding(1, 2).flatten.toList,
            run.plys.drop(1).sliding(1, 2).flatten.toList)
          val totNodes =
            split.map(_.map(_.s.get.getNodes).foldLeft((0))((x, y) => (x + y)))
          val avgNodes = totNodes.map(_ / run.plys.size * 2)
          val util = run.t.utility
          if (util == 0)
            (1, 0, (avgNodes.head, avgNodes.last), (totNodes.head, totNodes.last))
          else if (util > 0)
            (0, 1, (avgNodes.head, avgNodes.last), (totNodes.head, totNodes.last))
          else
            (0, 0, (avgNodes.head, avgNodes.last), (totNodes.head, totNodes.last))
        })
        .foldLeft((0, 0, (0, 0), (0, 0)))(
        (x, y) =>
        (x._1 + y._1,
          x._2 + y._2,
          (x._3._1 + y._3._1, x._3._2 + y._3._2),
          (x._4._1 + y._4._1, x._4._2 + y._4._2)))

      BasicMetric(draw,
        max,
        (nodesAvg._1.toDouble / t.rounds, nodesAvg._2.toDouble / t.rounds),
        totNodes,
        t.rounds)
    }
  }

  abstract class Printer[M <: Metric](chain: Option[Printer[M]]) {
    def printRow(p: M): Unit
    def printHead(p: M): Unit
    def prin(s: Stream[M]): Unit = {
      printHead(s.head)
      chain.map(_.printHead(s.head))
      for (p <- s.iterator) {
        printRow(p)
        chain.map(_.printRow(p))
      }
      close()
      chain.map(_.close())
    }
    def close(): Unit
  }

  class ScreenPrinter[M <: Metric](chain: Option[Printer[M]] = None)
      extends Printer[M](chain) {
    def printHead(p: M) = {
      print(
        (p.getHeader).mkString("\t") + "\n"
      )
    }
    def printRow(p: M) = {
      print(
        (p.getRow).mkString("\t") + "\n"
      )
    }
    def close() = {}
  }

  class CSVPrinter[M <: Metric](f: File, chain: Option[Printer[M]] = None)
      extends Printer[M](chain) {
    val writer = CSVWriter.open(f)
    def printHead(p: M) = {
      writer.writeRow(p.getHeader)
    }
    def printRow(p: M) = {
      writer.writeRow(p.getRow)
    }
    def close() = writer.close()
  }

  class CSVPlusScreenPrinter[M <: Metric](f: File,
    chain: Option[Printer[M]] = None)
      extends CSVPrinter[M](f, Some(new ScreenPrinter[M]))

  object Rounds {
    val Tiny = 20
    val Small = 30
    val Medium = 30
    val Large = 40
  }


  class Tournament[G <: Game[G]](val rounds: Int,
    val g: Stream[scala.util.Random],
    val m: scala.util.Random => Match[G]) {
    def run(): ParSeq[MatchLog[G]] = {
      (1 to rounds).par.map(
        (i: Int) => {
          m(g(i - 1)).run()
        }
      )
    }
  }

}
