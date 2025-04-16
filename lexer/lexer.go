package lexer

import "mono/token"

type Lexer struct {
	input        string // the program itself
	position     int    // current pos of input
	readPosition int    // current reading position in input
	ch           byte   // current char under examination
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		// EOF
		l.ch = 0
	} else {
		// The char itself
		l.ch = l.input[l.readPosition]
	}

	// Update position
	l.position = l.readPosition
	// Move on with the read position
	l.readPosition += 1
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	// Always get rid of white spaces
	l.skipWhiteSpace()

	switch l.ch {
	case '=':
		// Handle cmp
		if l.peekChar() == '=' {
			ch := l.ch   // This is gonna be the prev char soon
			l.readChar() // Move the cursor.
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			// Simple assign
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '!':
		// Handle !=
		if l.peekChar() == '=' {
			ch := l.ch   // This is gonna be the prev char soon
			l.readChar() // Move the cursor.
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			// Handle bang!
			tok = newToken(token.BANG, l.ch)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) { // Handle strings
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) { // Handle numbers
			tok.Literal = l.readNumber()
			tok.Type = token.INT
			return tok
		} else {
			// We don't know how to handle this
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

func (l *Lexer) skipWhiteSpace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	// Move the cursor til' we find a space or something
	for isLetter(l.ch) {
		l.readChar()
	}
	// Slice the whole identifier
	return l.input[position:l.position]
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func (l *Lexer) readNumber() string {
	position := l.position
	// Move the cursor til' we find a space or something
	for isDigit(l.ch) {
		l.readChar()
	}
	// Slice the whole identifier
	return l.input[position:l.position]
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}
