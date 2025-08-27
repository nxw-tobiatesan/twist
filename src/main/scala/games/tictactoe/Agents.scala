package com.tobiatesan.twist.tictactoe



import scala.util.Random

trait Agent {
  def selectMove(game: Game): Move
}
