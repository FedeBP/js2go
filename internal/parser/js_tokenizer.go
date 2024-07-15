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
	TokenString
	TokenNumber
	TokenOperator
	TokenPunctuation
	TokenKeyword
)

type Token struct {
	Type  TokenType
	Value string
}

type Tokenizer struct {
	input string
	pos   int
}

func NewTokenizer(input string) *Tokenizer {
	return &Tokenizer{input: input}
}

func (t *Tokenizer) next() rune {
	if t.pos >= len(t.input) {
		return 0
	}
	r, size := utf8.DecodeRuneInString(t.input[t.pos:])
	t.pos += size
	return r
}

func (t *Tokenizer) peek() rune {
	if t.pos >= len(t.input) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(t.input[t.pos:])
	return r
}

func (t *Tokenizer) NextToken() Token {
	for unicode.IsSpace(t.peek()) {
		t.next()
	}

	switch ch := t.peek(); {
	case ch == 0:
		return Token{Type: TokenEOF}
	case isLetter(ch):
		return t.readIdentifier()
	case ch == '\'' || ch == '"':
		return t.readString()
	case unicode.IsDigit(ch):
		return t.readNumber()
	case isOperator(ch):
		return t.readOperator()
	case isPunctuation(ch):
		return t.readPunctuation()
	default:
		t.next() // consume the unrecognized character
		return Token{Type: TokenEOF}
	}
}

func isPunctuation(ch rune) bool {
	return strings.ContainsRune("(){}[];,.", ch)
}

func (t *Tokenizer) readIdentifier() Token {
	var value string
	for isLetter(t.peek()) || unicode.IsDigit(t.peek()) {
		value += string(t.next())
	}
	if isKeyword(value) {
		return Token{Type: TokenKeyword, Value: value}
	}
	return Token{Type: TokenIdentifier, Value: value}
}

func (t *Tokenizer) readString() Token {
	quote := t.next()
	var value string
	for t.peek() != quote {
		if t.peek() == 0 {
			panic("Unterminated string literal")
		}
		value += string(t.next())
	}
	t.next() // Consume closing quote
	return Token{Type: TokenString, Value: value}
}

func (t *Tokenizer) readNumber() Token {
	var value string
	for unicode.IsDigit(t.peek()) || t.peek() == '.' {
		value += string(t.next())
	}
	return Token{Type: TokenNumber, Value: value}
}

func (t *Tokenizer) readOperator() Token {
	var value string
	for isOperator(t.peek()) {
		value += string(t.next())
	}
	return Token{Type: TokenOperator, Value: value}
}

func (t *Tokenizer) readPunctuation() Token {
	return Token{Type: TokenPunctuation, Value: string(t.next())}
}

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch) || ch == '_' || ch == '$'
}

func isOperator(ch rune) bool {
	return strings.ContainsRune("+-*/=<>!&|^%", ch)
}

func isKeyword(s string) bool {
	keywords := []string{"function", "return", "var", "let", "const", "if", "else", "for", "while"}
	for _, kw := range keywords {
		if s == kw {
			return true
		}
	}
	return false
}
