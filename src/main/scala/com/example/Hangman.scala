package com.example
import java.io.IOException

import zio._
import zio.console._
import zio.random._

object Hangman extends App {

  lazy val getGuess: ZIO[Console, IOException, Guess] = ???

  lazy val getName: ZIO[Console, IOException, Name] = ???

  lazy val chooseWord: URIO[Random, Word] = ???

  def gameLoop(oldState: State): ZIO[Console, IOException, Unit] = ???

  def analyzeNewGuess(oldState: State, newState: State, guess: Guess): GuessResult =
    if (oldState.guesses.contains(guess)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(guess.char)) GuessResult.Correct
    else GuessResult.Incorrect

  def renderState(state: State): URIO[Console, Unit] = {

    /*
        --------
        |      |
        |      0
        |     \|/
        |      |
        |     / \
        -

        f     n  c  t  o
        -  -  -  -  -  -  -
        Guesses: a, z, y, x
     */
    val stage = ZIO.fromOption(stages.lift(state.failuresCount)).orDieWith(_ => new Error("Boom!"))
    val word =
      state.word.toList
        .map(c => if (state.guesses.map(_.char).contains(c)) s" $c " else "   ")
        .mkString

    val line    = List.fill(state.word.length)(" - ").mkString
    val guesses = s" Guesses: ${state.guesses.map(_.char).mkString(", ")}"

    stage.flatMap { stg =>
      putStrLn {
        s"""
         #$stg
         #
         #$word
         #$line
         #
         #$guesses
         #
         #""".stripMargin('#')
      }
    }
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = ???
}
