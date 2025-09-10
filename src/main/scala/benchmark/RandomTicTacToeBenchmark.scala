package com.tobiatesan.twist
import com.tobiatesan.twist.ai.{RandomAgent}
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
import games.tictactoe.{TicTacToe, LiveTicTacToePosition}

object RandomTicTacToeBenchmark extends App {
  import com.tobiatesan.twist.benchmark._
  val rounds = 40

  new CSVPlusScreenPrinter[BasicMetric](
    new File("benchmark/random_tictactoe.csv")
  ).prin(
    Stream(
      new BasicExtractor(
        new Tournament(
          Rounds.Large,
          Stream.from(0).map(new scala.util.Random(_)),
          (r: scala.util.Random) =>
          new Match(
            new RandomAgent(r),
            new RandomAgent(r),
            false,
            new TicTacToe().startingPosition
          )
        )
      ).extract()
    )
  )
}
