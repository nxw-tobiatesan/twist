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
import com.tobiatesan.twist.matches.{Match, MatchLog, PlyEntry }
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

object KillerBenchmark extends App {
  import com.tobiatesan.twist.benchmark._

  case class ExtendedMetric(
    d: Int,
    k: Int,
    b: Metric)
      extends Metric {
    def getHeader = Seq("depth", "k") ++ b.getHeader
    def getRow = Seq[String](d.toString, k.toString) ++ b.getRow
  }

  print ("Benchmarking basic alpha-beta vs random")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/random_vs_alphabeta.csv")
  ).prin(
    for {
      d <- Stream(3,4,5)
    } yield
      ExtendedMetric(
        d,
        -1,
        new BasicExtractor(
          new Tournament(
            Rounds.Large,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new RandomAgent(r),
              new BasicAlphaBeta(
                draughts.NaiveDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                d, true),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )

  print ("Benchmarking minimax vs. random")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/random_vs_killer.csv")
  ).prin(
    for {
      d <- Stream(3,4,5)
      k <- Stream(2,4,8,16)
    } yield
      ExtendedMetric(
        d,
        k,
        new BasicExtractor(
          new Tournament(
            Rounds.Large,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new RandomAgent(r),
              new KillerAlphaBeta(
                draughts.NaiveDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                d,
                k,
                true),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )

  print ("SIMPLE KILLER")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/random_vs_simple_killer.csv")
  ).prin(
    for {
      d <- Stream(3,4,5)
      k <- Stream(2,4,8,16)
    } yield
      ExtendedMetric(
        d,
        k,
        new BasicExtractor(
          new Tournament(
            Rounds.Large,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new RandomAgent(r),
              new SimpleKillerAlphaBeta(
                draughts.NaiveDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                d,
                k,
                true),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )
}
