package parser

import (
	"fmt"
	"mono/ast"
	"mono/lexer"
	"mono/token"
)

// This is our hierarchy in our parser
const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > OR <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X OR !X
	CALL        // myFunc(x)
)

// This is the Parser main class
type Parser struct {
	l *lexer.Lexer // The lexer will help us with the tokens and stuff

	errors    []string    // For storing errors during parsing
	curToken  token.Token // Current token
	peekToken token.Token // Peek token

	prefixParseFns map[token.TokenType]prefixParseFn // prefix parse helpers
	infixParseFns  map[token.TokenType]infixParseFn  // infix parse helpers
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	// Read two tokens, to set cur and peekToken
	// This is kind of the same thing we do in the lexer with cur and peek position
	p.nextToken()
	p.nextToken()

	// Register infix and prefix parser functions
	// FOR PRATT PARSER, DELETE THIS COMMENT LATER
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)

	return p
}

// Getter of the parser errors
func (p *Parser) Errors() []string {
	return p.errors
}

// Setter of the last error encountered in the parsing process
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
	// Create the ast object for the LetStatement
	stmt := &ast.LetStatement{Token: p.curToken}

	// If not a identifier...
	// Not being an identifier means, the peekToken is not a keyword
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	// This is gonna be literally the variable name
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Assert that after the variable name, "=" is the next token
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	// TODO, TBD
	// Skipping expression until find the semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	// The statement is done, the ast Node is ready to be returned!
	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	// Create the ast object for Return
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	// TODO, TBD
	// Skipping expression until find the semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	// The return statement Node is done
	return stmt
}

// As simple as the naming suggests, simply parse an Identifier
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

// This is WHERE HELL GOES ON, I STILL HAVE NO IDEA ABOUT THIS
func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		return nil
	}

	leftExp := prefix()

	return leftExp
}

// TODO: this is part of the PRATT parser
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	// Move on if the expression is fully parsed
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// This function will parse ANY STATEMENT DEFINED BY US
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	// Move the lexer, then the parser will move as well
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	// Create the program, which is an array of statements
	program := &ast.Program{}
	// The list of statements
	program.Statements = []ast.Statement{}

	// Move on until the EOF
	for p.curToken.Type != token.EOF {
		// Parse those statements
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		// Move on (maybe some day the parser will reach EOF)
		p.nextToken()
	}

	return program
}

// PRATT PARSER
type (
	prefixParseFn func() ast.Expression
	// The arg is the left side of the expression
	infixParseFn func(ast.Expression) ast.Expression
)

// This function will register the prefix function parser
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// This function will register the infix function parser
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}
