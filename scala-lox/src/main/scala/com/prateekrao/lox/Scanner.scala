package com.prateekrao.lox

import scala.collection.mutable.ListBuffer

import com.prateekrao.lox.TokenType._

class Scanner(source: String) {

  private val tokens: ListBuffer[Token] = ListBuffer.empty[Token]
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): ListBuffer[Token] = {
    while (!isAtEnd()) {
      start = current
      scanToken()
    }

    tokens.append(Token(EOF, "", null, line))
    tokens
  }

  private def isAtEnd(): Boolean = {
    current >= source.length()
  }

  private def scanToken(): Unit = {
    val c = getNext()
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case _   => Lox.error(line, "Unexpected character.")
    }
  }

  private def getNext(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def addToken(`type`: TokenType, literal: Any = null): Unit = {
    val text = source.substring(start, current)
    tokens.append(Token(`type`, text, literal, line))
  }

}
