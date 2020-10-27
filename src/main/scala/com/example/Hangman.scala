package com.example
import java.io.IOException

import zio._
import zio.console._
import zio.random._

object Hangman extends App {

  def getUserInput(message: String): ZIO[Console, IOException, String] = putStrLn(message) *> getStrLn

  lazy val getGuess: ZIO[Console, IOException, Guess] =
    for {
      input <- getUserInput("What's your next guess?")
      guess <- ZIO.fromOption(Guess.make(input)) <> (putStrLn("Invalid input. Please try again...") *> getGuess)
    } yield guess

  lazy val getName: ZIO[Console, IOException, Name] =
    for {
      input <- getUserInput("What's your name?")
      name  <- ZIO.fromOption(Name.make(input)) <> (putStrLn("Invalid input. Please try again...") *> getName)
    } yield name

  lazy val chooseWord: URIO[Random, Word] =
    for {
      index <- nextIntBounded(words.length)
      word  <- ZIO.fromOption(words.lift(index).flatMap(Word.make)).orDieWith(_ => new Error("Boom!"))
    } yield word

  def gameLoop(oldState: State): ZIO[Console, IOException, Unit] =
    for {
      guess       <- renderState(oldState) *> getGuess
      newState    = oldState.addGuess(guess)
      guessResult = analyzeNewGuess(oldState, newState, guess)
      _ <- guessResult match {
            case GuessResult.Won =>
              putStrLn(s"Congratulations ${newState.name.name}! You won!") *> renderState(newState)
            case GuessResult.Lost =>
              putStrLn(s"Sorry ${newState.name.name}! You Lost! Word was: ${newState.word.word}") *>
                renderState(newState)
            case GuessResult.Correct =>
              putStrLn(s"Good guess, ${newState.name.name}!") *> gameLoop(newState)
            case GuessResult.Incorrect =>
              putStrLn(s"Bad guess, ${newState.name.name}!") *> gameLoop(newState)
            case GuessResult.Unchanged =>
              putStrLn(s"${newState.name.name}, You've already tried that letter!") *> gameLoop(newState)
          }
    } yield ()

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
    val hangman = ZIO(hangmanStages(state.failuresCount)).orDie
    val word =
      state.word.toList
        .map(c => if (state.guesses.map(_.char).contains(c)) s" $c " else "   ")
        .mkString

    val line    = List.fill(state.word.length)(" - ").mkString
    val guesses = s" Guesses: ${state.guesses.map(_.char).mkString(", ")}"

    hangman.flatMap { hangman =>
      putStrLn {
        s"""
         #$hangman
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

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      name <- putStrLn("Welcome to ZIO Hangman!") *> getName
      word <- chooseWord
      _    <- gameLoop(State.initial(name, word))
    } yield ()).exitCode
}
