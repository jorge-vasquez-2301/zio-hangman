package com.example
import java.io.IOException

import zio._

object Hangman extends ZIOAppDefault {

  def getUserInput(message: String): IO[IOException, String] = Console.readLine(message)

  lazy val getGuess: IO[IOException, Guess] =
    for {
      input <- getUserInput("What's your next guess? ")
      guess <- ZIO.from(Guess.make(input)) <> (Console.printLine("Invalid input. Please try again...") <*> getGuess)
    } yield guess

  lazy val getName: IO[IOException, Name] =
    for {
      input <- getUserInput("What's your name? ")
      name  <- ZIO.from(Name.make(input)) <> (Console.printLine("Invalid input. Please try again...") <*> getName)
    } yield name

  lazy val chooseWord: UIO[Word] =
    for {
      index <- Random.nextIntBounded(words.length)
      word  <- ZIO.from(words.lift(index).flatMap(Word.make)).orDieWith(_ => new Error("Boom!"))
    } yield word

  def gameLoop(oldState: State): IO[IOException, Unit] =
    for {
      guess       <- renderState(oldState) <*> getGuess
      newState    = oldState.addGuess(guess)
      guessResult = analyzeNewGuess(oldState, newState, guess)
      _ <- guessResult match {
            case GuessResult.Won =>
              Console.printLine(s"Congratulations ${newState.name.name}! You won!") <*> renderState(newState)
            case GuessResult.Lost =>
              Console.printLine(s"Sorry ${newState.name.name}! You Lost! Word was: ${newState.word.word}") <*>
                renderState(newState)
            case GuessResult.Correct =>
              Console.printLine(s"Good guess, ${newState.name.name}!") <*> gameLoop(newState)
            case GuessResult.Incorrect =>
              Console.printLine(s"Bad guess, ${newState.name.name}!") <*> gameLoop(newState)
            case GuessResult.Unchanged =>
              Console.printLine(s"${newState.name.name}, You've already tried that letter!") <*> gameLoop(newState)
          }
    } yield ()

  def analyzeNewGuess(oldState: State, newState: State, guess: Guess): GuessResult =
    if (oldState.guesses.contains(guess)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(guess.char)) GuessResult.Correct
    else GuessResult.Incorrect

  def renderState(state: State): IO[IOException, Unit] = {

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
    val hangman = ZIO.attempt(hangmanStages(state.failuresCount)).orDie
    val word =
      state.word.toList
        .map(c => if (state.guesses.map(_.char).contains(c)) s" $c " else "   ")
        .mkString

    val line    = List.fill(state.word.length)(" - ").mkString
    val guesses = s" Guesses: ${state.guesses.map(_.char).mkString(", ")}"

    hangman.flatMap { hangman =>
      Console.printLine {
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

  val run: IO[IOException, Unit] =
    for {
      name <- Console.printLine("Welcome to ZIO Hangman!") <*> getName
      word <- chooseWord
      _    <- gameLoop(State.initial(name, word))
    } yield ()
}
