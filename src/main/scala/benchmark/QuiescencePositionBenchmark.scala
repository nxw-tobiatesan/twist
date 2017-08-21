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

object QuiescencePositionBenchmark extends App {
  import com.tobiatesan.twist.benchmark._
  case class ExtendedMetric(d: Int, extra: Int, b: Metric)
      extends Metric {
    def getHeader = Seq("depth", "extra") ++ b.getHeader
    def getRow = Seq[String](d.toString, extra.toString) ++ b.getRow
  }

  print ("Benchmarking minimax vs quiescence with positionmaterialaware")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/minimax_vs_quiescence_position.csv")
  ).prin(
    for {
      d <- Stream(2,3,4)
      extra <- Stream(0,1,2,3)
      if (d + extra <= 5)
    } yield
      ExtendedMetric(
        d,
        extra,
        new BasicExtractor(
          new Tournament(
            Rounds.Medium,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new UCTAgent[Draughts](
                1200,
                1.5,
                false,
                r),
              new QuiescenceAlphaBeta(
                draughts.PositionAwareDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                d,
                draughts.BasicDraughtsQuiescenceCheck,
                extra,
                true),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )
  print ("Benchmarking quiescence with positionmaterialaware vs minimax")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/minimax_vs_quiescence_position_REVERSE.csv")
  ).prin(
    for {
      d <- Stream(2,3,4)
      extra <- Stream(0,1,2,3,4)
      if (d + extra <= 5)
    } yield
      ExtendedMetric(
        d,
        extra,
        new BasicExtractor(
          new Tournament(
            Rounds.Large,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new QuiescenceAlphaBeta(
                draughts.PositionAwareDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                d,
                draughts.BasicDraughtsQuiescenceCheck,
                extra,
                false),
              new UCTAgent[Draughts](
                1200,
                1.5,
                true,
                r),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )
}
