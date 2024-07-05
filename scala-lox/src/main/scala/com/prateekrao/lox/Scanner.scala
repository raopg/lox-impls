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
    val c = advance()
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
      case '!' => addToken(if (matchChar('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (matchChar('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (matchChar('=')) LESSER_EQUAL else LESSER)
      case '>' => addToken(if (matchChar('=')) GREATER_EQUAL else GREATER)
      case '/' => {

        if (matchChar('/')) {
          while (peek() != '\n' && !isAtEnd()) advance()
        } else {
          addToken(SLASH)
        }

      }

      case _ => Lox.error(line, "Unexpected character.")
    }
  }

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def matchChar(expected: Char): Boolean = {
    if (isAtEnd() || source.charAt(current) != expected) return false
    current += 1
    true
  }

  private def peek(): Char = {
    if (isAtEnd()) '\u0000' else source.charAt(current)
  }

  private def addToken(`type`: TokenType, literal: Any = null): Unit = {
    val text = source.substring(start, current)
    tokens.append(Token(`type`, text, literal, line))
  }

}
