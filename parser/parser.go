package parser

import (
	"fmt"
	"mono/ast"
	"mono/lexer"
	"mono/token"
)

type Parser struct {
	l *lexer.Lexer // The lexer will help us with the tokens and stuff

	errors    []string    // For storing errors during parsing
	curToken  token.Token // Current token
	peekToken token.Token // peek token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	// Read two tokens, to set cur and peekToken
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s", t, p.peekToken.Type)
	// Append the error to the errors array
	p.errors = append(p.errors, msg)
}

// Helper for compare the current token type
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// Helper for compare the peek token type
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// "assertion" helper for the expected peek token
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		// Move on
		p.nextToken()
		return true
	} else {
		// Append the error to the list
		p.peekError(t)
		return false
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	// Create the ast object
	stmt := &ast.LetStatement{Token: p.curToken}

	// If not a identifier...
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	// This is gonna be literally the variable name
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Assert that after the variable name, "=" is the next token
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	// TODO
	// Skipping expression until find the semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	// The statement is done
	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	// Create the ast object
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	// TODO
	// Skipping expression until find the semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	// The statement is done
	return stmt
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return nil
	}
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	// Move the lexer, then the parser will move as well
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}
