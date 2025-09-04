package com.tobiatesan.twist.games

import scala.io.StdIn

class AgentMenu(label: String, options: Seq[(String, String)], default: String) {
  def select(): String = {
    println(s"Choose $label agent:")
    options.zipWithIndex.foreach { case ((name, _), idx) => println(s"  ${idx + 1}) $name") }
    val input = scala.io.StdIn.readLine().trim
    val idx = try { input.toInt - 1 } catch { case _: NumberFormatException => -1 }
    if (idx >= 0 && idx < options.length) options(idx)._2 else default
  }
}

class GamePlayer[G, M, B, P, AgentType](
  gameFactory: () => G,
  printBoard: B => Unit,
  humanMove: (G, B) => M,
  agentMove: (AgentType, G) => M,
  getBoard: G => B,
  getCurrent: G => P,
  play: (G, M) => G,
  winner: G => Option[P],
  isDraw: G => Boolean,
  agentOptions: Seq[(String, String)],
  agentSelector: String => AgentType
) {
  def run(): Unit = {
    var game = gameFactory()
    val xAgent = new AgentMenu("X", agentOptions, "human").select()
    val oAgent = new AgentMenu("O", agentOptions, "human").select()
    def selectMove(mark: P): M = {
      val tpe = if (mark == getCurrent(game)) xAgent else oAgent
      tpe match {
        case "human" => humanMove(game, getBoard(game))
        case _ => agentMove(agentSelector(tpe), game)
      }
    }
    while (winner(game).isEmpty && !isDraw(game)) {
      printBoard(getBoard(game))
      val move = selectMove(getCurrent(game))
      game = play(game, move)
    }
    printBoard(getBoard(game))
    winner(game) match {
      case Some(mark) => println(s"$mark wins!")
      case None => println("Draw!")
    }
  }
}
