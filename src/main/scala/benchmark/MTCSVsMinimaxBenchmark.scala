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

object MTCSvsMinimaxBenchmark extends App {
  import com.tobiatesan.twist.benchmark._
  case class ExtendedMetric(budget: Int, d: Int, c: Double, b: Metric) extends Metric {
    def getHeader = Seq("budget", "depth", "c") ++ b.getHeader
    def getRow = Seq[String](budget.toString, d.toString, c.toString) ++ b.getRow
  }

  print ("Benchmarking mtcs vs minimax")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/mtcs_vs_minimax.csv")
  ).prin(
    for {
      depth <- Stream(3)
      budget <- Stream(600, 1200, 2400, 4800)
      c <- Stream(0.5, 1.0, 1.5, 2.0, 4.0)
    } yield
      ExtendedMetric(
        budget,
        depth,
        c,
        new BasicExtractor(
          new Tournament(
            Rounds.Small,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new UCTAgent[Draughts](
                budget,
                c,
                false,
                r),
              new BasicAlphaBeta(
                draughts.NaiveDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                depth,
                true),
              false,
              basicDraughts.startingPosition
            )
          )
        ).extract()
      )
  )

  print ("Benchmarking minimax vs mtcs")
  new CSVPlusScreenPrinter[ExtendedMetric](
    new File("benchmark/mtcs_vs_minimax_REVERSE.csv")
  ).prin(
    for {
      depth <- Stream(3)
      budget <- Stream(600, 1200, 2400, 4800)
      c <- Stream(0.5, 1.0, 1.5, 2.0, 4.0)
    } yield
      ExtendedMetric(
        budget,
        depth,
        c,
        new BasicExtractor(
          new Tournament(
            Rounds.Small,
            Stream.from(0).map(new scala.util.Random(_)),
            (r: scala.util.Random) =>
            new Match(
              new BasicAlphaBeta(
                draughts.NaiveDraughtsEvaluation,
                draughts.BasicDraughtsMoveOrdering,
                depth,
                false),
              new UCTAgent[Draughts](
                budget,
                c,
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
