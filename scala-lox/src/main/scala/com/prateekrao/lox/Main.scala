package com.prateekrao.lox;

import scala.io.StdIn;
import scala.io.Source;

object Lox {

  private var hasError: Boolean = false;

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + " ] Error" + where + ": " + message);
    hasError = true;
  }

  def main(args: Array[String]): Unit = {

    if (args.length > 1) {
      println("Usage: scala-lox [script]")
      System.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path)

    try {
      val content = source.mkString

      run(content)

      if (hasError) System.exit(65)

    } finally {
      source.close()
    }

  }

  def runPrompt(): Unit = {
    while (true) {
      print("> ")
      val line = StdIn.readLine()

      if (line == null) return
      run(line)
      hasError = false;

    }
  }

  def run(source: String): Unit = {
    None
  }

}
