package parser

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

type TokenType int

const (
	TokenEOF TokenType = iota
	TokenIdentifier
	TokenKeyword
	TokenNumber
	TokenString
	TokenOperator
	TokenPunctuation
	TokenBooleanLiteral
	TokenNullLiteral
)

type Token struct {
	Type  TokenType
	Value string
}

type Lexer struct {
	input string
	pos   int
	start int
}

func NewLexer(input string) *Lexer {
	return &Lexer{input: input}
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if l.pos >= len(l.input) {
		return Token{Type: TokenEOF}
	}

	l.start = l.pos
	r := l.next()

	switch {
	case isLetter(r):
		return l.lexIdentifierOrKeyword()
	case isDigit(r):
		return l.lexNumber()
	case r == '\'' || r == '"':
		return l.lexString()
	case isOperator(r):
		return l.lexOperator()
	case isPunctuation(r):
		return Token{Type: TokenPunctuation, Value: string(r)}
	case r == '.':
		if l.peekIs('.') && l.peekNextIs('.') {
			l.next()
			l.next()
			return Token{Type: TokenPunctuation, Value: "..."}
		}
		return Token{Type: TokenPunctuation, Value: string(r)}
	default:
		return Token{Type: TokenEOF}
	}
}

func (l *Lexer) next() rune {
	if l.pos >= len(l.input) {
		return 0
	}
	r, size := utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += size
	return r
}

func (l *Lexer) peek() rune {
	r := l.next()
	l.pos -= utf8.RuneLen(r)
	return r
}

func (l *Lexer) skipWhitespace() {
	for unicode.IsSpace(l.peek()) {
		l.next()
	}
}

func (l *Lexer) lexIdentifierOrKeyword() Token {
	for isLetter(l.peek()) || isDigit(l.peek()) {
		l.next()
	}
	value := l.input[l.start:l.pos]
	if isKeyword(value) {
		return Token{Type: TokenKeyword, Value: value}
	}
	if isBooleanLiteral(value) {
		return Token{Type: TokenBooleanLiteral, Value: value}
	}
	if isNullLiteral(value) {
		return Token{Type: TokenNullLiteral, Value: value}
	}
	return Token{Type: TokenIdentifier, Value: value}
}

func (l *Lexer) lexNumber() Token {
	for isDigit(l.peek()) || l.peek() == '.' {
		l.next()
	}
	return Token{Type: TokenNumber, Value: l.input[l.start:l.pos]}
}

func (l *Lexer) lexString() Token {
	quote := l.input[l.start]
	l.next() // Skip opening quote
	for l.peek() != rune(quote) && l.pos < len(l.input) {
		if l.peek() == '\\' {
			l.next() // Skip escape character
		}
		l.next()
	}
	if l.pos < len(l.input) {
		l.next() // Skip closing quote
	}
	return Token{Type: TokenString, Value: l.input[l.start:l.pos]}
}

func (l *Lexer) lexOperator() Token {
	for isOperator(l.peek()) {
		l.next()
	}
	return Token{Type: TokenOperator, Value: l.input[l.start:l.pos]}
}

func isLetter(r rune) bool {
	return unicode.IsLetter(r) || r == '_' || r == '$'
}

func isDigit(r rune) bool {
	return unicode.IsDigit(r)
}

func isOperator(r rune) bool {
	return strings.ContainsRune("+-*/=<>!&|^%", r)
}

func isPunctuation(r rune) bool {
	return strings.ContainsRune("(){}[];,.", r)
}

func isKeyword(s string) bool {
	keywords := []string{
		"var", "let", "const", "function", "return", "if", "else", "for", "while", "do",
		"switch", "case", "default", "break", "continue", "try", "catch", "finally", "throw",
		"class", "new", "this", "super", "import", "export", "async", "await",
	}
	for _, kw := range keywords {
		if s == kw {
			return true
		}
	}
	return false
}

func isBooleanLiteral(s string) bool {
	return s == "true" || s == "false"
}

func isNullLiteral(s string) bool {
	return s == "null"
}

func (l *Lexer) peekIs(expected rune) bool {
	if l.pos >= len(l.input) {
		return false
	}
	r, _ := utf8.DecodeRuneInString(l.input[l.pos:])
	return r == expected
}

func (l *Lexer) peekNextIs(expected rune) bool {
	if l.pos+1 >= len(l.input) {
		return false
	}
	r, _ := utf8.DecodeRuneInString(l.input[l.pos+1:])
	return r == expected
}
