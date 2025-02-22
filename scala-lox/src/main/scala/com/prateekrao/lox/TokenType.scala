package com.prateekrao.lox;

enum TokenType {
  // Single character tokens
  case LEFT_PAREN,
       RIGHT_PAREN,
       LEFT_BRACE,
       RIGHT_BRACE,
       COMMA,
       DOT,
       MINUS,
       PLUS,
       SEMICOLON,
       SLASH,
       STAR

  // One or two character tokens
  case BANG,
       BANG_EQUAL,
       EQUAL,
       EQUAL_EQUAL,
       GREATER,
       GREATER_EQUAL,
       LESSER,
       LESSER_EQUAL

  // Literals
  case IDENTIFIER,
       STRING,
       NUMBER

  // Keywords
  case AND,
       CLASS,
       ELSE,
       FALSE,
       FUN,
       FOR,
       IF,
       NIL,
       OR,
       PRINT,
       RETURN,
       SUPER,
       THIS,
       TRUE,
       VAR,
       WHILE

  case EOF
}

