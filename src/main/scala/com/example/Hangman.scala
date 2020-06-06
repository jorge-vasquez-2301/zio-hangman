package com.example
import java.io.IOException

import zio._
import zio.console._
import zio.random._

object Hangman extends App {

  lazy val getGuess: ZIO[Console, IOException, Guess] =
    for {
      input <- putStrLn("What's your next guess?") *> getStrLn
      guess <- Guess.make(input) match {
                case Some(guess) => ZIO.succeed(guess)
                case None        => putStrLn("Invalid input. Please try again...") *> getGuess
              }
    } yield guess

  lazy val getName: ZIO[Console, IOException, Name] =
    for {
      input <- putStrLn("What's your name?") *> getStrLn
      name <- Name.make(input) match {
               case Some(name) => ZIO.succeed(name)
               case None       => putStrLn("Invalid input. Please try again...") *> getName
             }
    } yield name

  lazy val chooseWord: URIO[Random, Word] =
    for {
      index <- nextIntBounded(words.length)
      word  <- ZIO.fromOption(words.lift(index).flatMap(Word.make)).orDieWith(_ => new Error("Boom!"))
    } yield word

  def gameLoop(oldState: State): ZIO[Console, IOException, Unit] =
    for {
      _           <- renderState(oldState)
      guess       <- getGuess
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

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      _            <- putStrLn("Welcome to ZIO Hangman!")
      name         <- getName
      word         <- chooseWord
      initialState = State.initial(name, word)
      _            <- gameLoop(initialState)
    } yield ExitCode.success).orElse(ZIO.succeed(ExitCode.failure))
}
