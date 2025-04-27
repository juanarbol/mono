package parser

import (
	"fmt"
	"mono/ast"
	"mono/lexer"
	"mono/token"
	"strconv"
)

// This is our hierarchy in our parser
// This works kind of an enun in C -or something-
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

// Just map tokens to precedences
var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.GT:       LESSGREATER,
	token.LT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
}

type (
	// The arg is the left side of the expression
	infixParseFn  func(ast.Expression) ast.Expression
	prefixParseFn func() ast.Expression
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

	// REGISTER PREFIX PARSER FUNCTIONS
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	// Handle numbers
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	// Handle booleans
	p.registerPrefix(token.TRUE, p.parseBooleanLiteral)
	p.registerPrefix(token.FALSE, p.parseBooleanLiteral)
	// Handle "grouped" expressions
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	// Handle "if" expressions
	p.registerPrefix(token.IF, p.parseIfExpression)

	// REGISTER INFIX PARSER FUNCTIONS
	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)

	return p
}

// This function will register the prefix function parser
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// This function will register the infix function parser
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// Getter of the parser errors
func (p *Parser) Errors() []string {
	return p.errors
}

// Getter of the peek precedence
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// Getter of the current precedence
func (p *Parser) currentPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
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

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	// Move on in parsing
	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	// Get our current precedence to make sure that hierachies are being respected
	precedence := p.currentPrecedence()
	// Move on
	p.nextToken()
	// The right side must be parsed with the current precedence
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}

	// Call the bounded function
	leftExp := prefix()

	// My dude, this is the WHOLE THING, please debug and read more carefully
	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		// Move on, continue
		p.nextToken()

		// Call the bounded function to the infix parser
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	// Move from {
	p.nextToken()

	// Iterate over the whole block and handle early EOF
	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		// Literally parse the "body", any other valid "program"
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	// Move from the '('
	p.nextToken()

	expression.Condition = p.parseExpression(LOWEST)

	// ExpectPeek will call a p.nextToken, so, no worries
	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	// THIS IS INCREDIBLE
	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken() // move on

		if !p.expectPeek(token.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	// ignore the '('
	p.nextToken()

	// Parse a expression normally
	exp := p.parseExpression(LOWEST)

	// after the expression is parsed, it should be a ')' in the peekToken
	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	// Set the value
	lit.Value = value

	return lit
}

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
