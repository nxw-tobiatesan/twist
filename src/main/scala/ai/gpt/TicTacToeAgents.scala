package com.tobiatesan.twist.ai.gpt



import com.tobiatesan.twist.tictactoe._
import scala.util.Random

trait GPTAgent {
  def selectMove(game: Game): Move
}
