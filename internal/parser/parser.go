package parser

import (
	"fmt"
	"strconv"

	"github.com/FedeBP/js2go/pkg/ast"
)

type Parser struct {
	l *Lexer

	curToken  Token
	peekToken Token

	errors []string

	prefixParseFns map[TokenType]prefixParseFn
	infixParseFns  map[TokenType]infixParseFn
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

const (
	_ int = iota
	LOWEST
	ASSIGN      // =
	CONDITIONAL // ?:
	LogicalOr   // ||
	LogicalAnd  // &&
	EQUALITY    // == !=
	COMPARISON  // > >= < <=
	SUM         // + -
	PRODUCT     // * / %
	PREFIX      // -X or !X
	CALL        // myFunction(X)
	MEMBER      // obj.X or obj[X]
)

var precedences = map[string]int{
	"=":  ASSIGN,
	"?":  CONDITIONAL,
	"||": LogicalOr,
	"&&": LogicalAnd,
	"==": EQUALITY,
	"!=": EQUALITY,
	"<":  COMPARISON,
	"<=": COMPARISON,
	">":  COMPARISON,
	">=": COMPARISON,
	"+":  SUM,
	"-":  SUM,
	"*":  PRODUCT,
	"/":  PRODUCT,
	"%":  PRODUCT,
	".":  MEMBER,
	"[":  MEMBER,
	"(":  CALL,
}

func New(l *Lexer) *Parser {
	p := &Parser{
		l:              l,
		errors:         []string{},
		prefixParseFns: make(map[TokenType]prefixParseFn),
		infixParseFns:  make(map[TokenType]infixParseFn),
	}

	// Register prefix parse functions
	p.registerPrefix(TokenIdentifier, p.parseIdentifier)
	p.registerPrefix(TokenNumber, p.parseNumberLiteral)
	p.registerPrefix(TokenString, p.parseStringLiteral)
	p.registerPrefix(TokenBooleanLiteral, p.parseBooleanLiteral)
	p.registerPrefix(TokenNullLiteral, p.parseNullLiteral)
	p.registerPrefix(TokenOperator, p.parsePrefixExpression)
	p.registerPrefix(TokenPunctuation, p.parseGroupedExpression)
	p.registerPrefix(TokenKeyword, p.parseFunctionLiteral)
	p.registerPrefix(TokenPunctuation, p.parseArrayLiteral)
	p.registerPrefix(TokenPunctuation, p.parseObjectLiteral)
	p.registerPrefix(TokenKeyword, p.parseNewExpression)
	p.registerPrefix(TokenPunctuation, p.parseSpreadElement)

	// Register infix parse functions
	p.registerInfix(TokenOperator, p.parseInfixExpression)
	p.registerInfix(TokenPunctuation, p.parseCallExpression)
	p.registerInfix(TokenPunctuation, p.parseIndexExpression)

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) registerPrefix(tokenType TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case TokenKeyword:
		switch p.curToken.Value {
		case "var", "let", "const":
			return p.parseVariableDeclaration()
		case "function":
			return p.parseFunctionDeclaration()
		case "return":
			return p.parseReturnStatement()
		case "if":
			return p.parseIfStatement()
		case "for":
			return p.parseForStatement()
		case "while":
			return p.parseWhileStatement()
		case "do":
			return p.parseDoWhileStatement()
		case "switch":
			return p.parseSwitchStatement()
		case "try":
			return p.parseTryStatement()
		case "class":
			return p.parseClassDeclaration()
		}
	default:
		return p.parseExpressionStatement()
	}
	return p.parseExpressionStatement()
}

func (p *Parser) parseVariableDeclaration() *ast.JSVariableDeclaration {
	decl := &ast.JSVariableDeclaration{
		Kind: p.curToken.Value, // "var", "let", or "const"
	}

	p.nextToken() // move past var/let/const

	for {
		if p.curToken.Type != TokenIdentifier {
			p.errors = append(p.errors, fmt.Sprintf("Expected identifier, got %s", p.curToken.Value))
			return nil
		}

		declarator := &ast.JSVariableDeclarator{
			ID: &ast.Identifier{Name: p.curToken.Value},
		}

		p.nextToken() // move past identifier

		// Check for initializer
		if p.curToken.Type == TokenOperator && p.curToken.Value == "=" {
			p.nextToken() // move past '='

			init := p.parseExpression(LOWEST)
			if init == nil {
				return nil
			}
			declarator.Init = init
		}

		decl.Declarations = append(decl.Declarations, declarator)

		// If the next token is a comma, we have more variables to parse
		if p.curToken.Type == TokenPunctuation && p.curToken.Value == "," {
			p.nextToken() // move past comma
			continue
		}

		// If we're here, we should be at a semicolon or end of statement
		break
	}

	// Optionally consume semicolon
	if p.curToken.Type == TokenPunctuation && p.curToken.Value == ";" {
		p.nextToken()
	}

	return decl
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(TokenPunctuation, ";") && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		if p.curTokenIs(TokenPunctuation, "?") {
			leftExp = p.parseConditionalExpression(leftExp)
		} else {
			leftExp = infix(leftExp)
		}
	}

	return leftExp
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Name: p.curToken.Value}
}

func (p *Parser) parseNumberLiteral() ast.Expression {
	value, err := strconv.ParseFloat(p.curToken.Value, 64)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("could not parse %q as float", p.curToken.Value))
		return nil
	}
	return &ast.NumberLiteral{Value: value}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Value: p.curToken.Value}
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.BooleanLiteral{Value: p.curToken.Value == "true"}
}

func (p *Parser) parseNullLiteral() ast.Expression {
	return &ast.NullLiteral{}
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.JSUnaryExpression{
		Operator: p.curToken.Value,
	}

	p.nextToken()

	expression.Argument = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.JSBinaryExpression{
		Left:     left,
		Operator: p.curToken.Value,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	return exp
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.JSIfStatement{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken()
	expression.Test = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	expression.Consequent = p.parseBlockStatement()

	if p.peekTokenIs(TokenKeyword, "else") {
		p.nextToken()

		if p.peekTokenIs(TokenKeyword, "if") {
			p.nextToken()
			expression.Alternate = p.parseIfExpression()
		} else {
			if !p.expectPeek(TokenPunctuation, "{") {
				return nil
			}
			expression.Alternate = p.parseBlockStatement()
		}
	}

	return expression
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.JSFunction{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	var identifiers []*ast.Identifier

	if p.peekTokenIs(TokenPunctuation, ")") {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &ast.Identifier{Name: p.curToken.Value}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(TokenPunctuation, ",") {
		p.nextToken()
		p.nextToken()
		ident := &ast.Identifier{Name: p.curToken.Value}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	return identifiers
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{
		Statements: []ast.Statement{},
	}

	p.nextToken() // Move past the opening '{'

	for !p.curTokenIs(TokenPunctuation, "}") {
		if p.curTokenIs(TokenEOF, "") {
			p.errors = append(p.errors, "Unexpected end of file, expected '}'")
			return nil
		}

		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) curTokenIs(t TokenType, value string) bool {
	return p.curToken.Type == t && (value == "" || p.curToken.Value == value)
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.JSCallExpression{Callee: function}
	exp.Arguments = p.parseExpressionList(")")
	return exp
}

func (p *Parser) parseExpressionList(end string) []ast.Expression {
	var list []ast.Expression

	if p.peekTokenIs(TokenPunctuation, end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(TokenPunctuation, ",") {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(TokenPunctuation, end) {
		return nil
	}

	return list
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.JSArrayExpression{}
	array.Elements = p.parseExpressionList("]")
	return array
}

func (p *Parser) parseObjectLiteral() ast.Expression {
	obj := &ast.JSObjectExpression{}

	for !p.peekTokenIs(TokenPunctuation, "}") {
		p.nextToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(TokenPunctuation, ":") {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)

		obj.Properties = append(obj.Properties, &ast.JSProperty{Key: key, Value: value})

		if !p.peekTokenIs(TokenPunctuation, "}") && !p.expectPeek(TokenPunctuation, ",") {
			return nil
		}
	}

	if !p.expectPeek(TokenPunctuation, "}") {
		return nil
	}

	return obj
}

func (p *Parser) parseMemberExpression(object ast.Expression) ast.Expression {
	exp := &ast.JSMemberExpression{Object: object}

	if p.curToken.Value == "[" {
		exp.Computed = true
		p.nextToken() // Consume '['
		exp.Property = p.parseExpression(LOWEST)
		if !p.expectPeek(TokenPunctuation, "]") {
			return nil
		}
	} else if p.curToken.Value == "." {
		exp.Computed = false
		p.nextToken() // Consume '.'
		if !p.curTokenIs(TokenIdentifier, "") {
			p.errors = append(p.errors, "Expected identifier after '.'")
			return nil
		}
		exp.Property = p.parseIdentifier()
	} else {
		p.errors = append(p.errors, "Expected '[' or '.' in member expression")
		return nil
	}

	return exp
}

func (p *Parser) noPrefixParseFnError(t TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("no prefix parse function for %v found", t))
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Value]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Value]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) expectPeek(t TokenType, value string) bool {
	if p.peekTokenIs(t, value) {
		p.nextToken()
		return true
	}
	p.peekError(t, value)
	return false
}

func (p *Parser) peekTokenIs(t TokenType, value string) bool {
	return p.peekToken.Type == t && (value == "" || p.peekToken.Value == value)
}

func (p *Parser) peekError(t TokenType, value string) {
	msg := fmt.Sprintf("Expected next token to be %v", t)
	if value != "" {
		msg += fmt.Sprintf(" with value '%s'", value)
	}
	msg += fmt.Sprintf(", got %v", p.peekToken.Type)
	if p.peekToken.Value != "" {
		msg += fmt.Sprintf(" (%s)", p.peekToken.Value)
	}
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseFunctionDeclaration() *ast.JSFunction {
	fun := &ast.JSFunction{}

	p.nextToken() // Move past 'function' keyword

	if p.curToken.Type != TokenIdentifier {
		p.errors = append(p.errors, "Expected function name")
		return nil
	}
	fun.Name = &ast.Identifier{Name: p.curToken.Value}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	fun.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	fun.Body = p.parseBlockStatement()

	return fun
}

func (p *Parser) parseReturnStatement() *ast.JSReturnStatement {
	stmt := &ast.JSReturnStatement{}

	p.nextToken() // Move past 'return' keyword

	// Check if there's an expression after 'return'
	if !p.curTokenIs(TokenPunctuation, ";") && !p.curTokenIs(TokenPunctuation, "}") {
		stmt.Value = p.parseExpression(LOWEST)

		// Parse until we reach a semicolon or the end of the line
		for !p.curTokenIs(TokenPunctuation, ";") && !p.curTokenIs(TokenPunctuation, "}") && !p.curTokenIs(TokenEOF, "") {
			p.nextToken()
		}
	}

	// Consume the semicolon if it's there
	if p.curTokenIs(TokenPunctuation, ";") {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseIfStatement() *ast.JSIfStatement {
	stmt := &ast.JSIfStatement{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken()
	stmt.Test = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	stmt.Consequent = p.parseBlockStatement()

	if p.peekTokenIs(TokenKeyword, "else") {
		p.nextToken()

		if p.peekTokenIs(TokenKeyword, "if") {
			p.nextToken()
			stmt.Alternate = p.parseIfStatement()
		} else {
			if !p.expectPeek(TokenPunctuation, "{") {
				return nil
			}
			stmt.Alternate = p.parseBlockStatement()
		}
	}

	return stmt
}

func (p *Parser) parseForStatement() *ast.JSForStatement {
	stmt := &ast.JSForStatement{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken() // Move past '('

	// Parse Init
	if !p.curTokenIs(TokenPunctuation, ";") {
		if p.curTokenIs(TokenKeyword, "var") || p.curTokenIs(TokenKeyword, "let") || p.curTokenIs(TokenKeyword, "const") {
			stmt.Init = p.parseVariableDeclaration()
		} else {
			stmt.Init = p.parseExpression(LOWEST)
		}
	}

	if !p.expectPeek(TokenPunctuation, ";") {
		return nil
	}

	// Parse Test
	p.nextToken() // Move past ';'
	if !p.curTokenIs(TokenPunctuation, ";") {
		stmt.Test = p.parseExpression(LOWEST)
	}

	if !p.expectPeek(TokenPunctuation, ";") {
		return nil
	}

	// Parse Update
	p.nextToken() // Move past ';'
	if !p.curTokenIs(TokenPunctuation, ")") {
		stmt.Update = p.parseExpression(LOWEST)
	}

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseWhileStatement() *ast.JSWhileStatement {
	stmt := &ast.JSWhileStatement{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken() // Move past '('

	stmt.Test = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseDoWhileStatement() *ast.JSDoWhileStatement {
	stmt := &ast.JSDoWhileStatement{}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	if !p.expectPeek(TokenKeyword, "while") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken() // Move past '('

	stmt.Test = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	// In JavaScript, do...while statements are terminated with a semicolon
	if p.peekTokenIs(TokenPunctuation, ";") {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseSwitchStatement() *ast.JSSwitchStatement {
	stmt := &ast.JSSwitchStatement{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken() // Move past '('

	stmt.Discriminant = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	p.nextToken() // Move past '{'

	for !p.curTokenIs(TokenPunctuation, "}") {
		if p.curTokenIs(TokenEOF, "") {
			p.errors = append(p.errors, "Unexpected end of file, expected '}'")
			return nil
		}

		if p.curTokenIs(TokenKeyword, "case") || p.curTokenIs(TokenKeyword, "default") {
			switchCase := p.parseSwitchCase()
			if switchCase != nil {
				stmt.Cases = append(stmt.Cases, switchCase)
			}
		} else {
			p.errors = append(p.errors, fmt.Sprintf("Unexpected token in switch statement: %s", p.curToken.Value))
			return nil
		}
	}

	p.nextToken() // Move past '}'

	return stmt
}

func (p *Parser) parseSwitchCase() *ast.JSSwitchCase {
	switchCase := &ast.JSSwitchCase{}

	if p.curTokenIs(TokenKeyword, "case") {
		p.nextToken() // Move past 'case'
		switchCase.Test = p.parseExpression(LOWEST)
	} else if p.curTokenIs(TokenKeyword, "default") {
		p.nextToken() // Move past 'default'
		switchCase.Test = nil
	} else {
		p.errors = append(p.errors, fmt.Sprintf("Expected 'case' or 'default', got %s", p.curToken.Value))
		return nil
	}

	if !p.expectPeek(TokenPunctuation, ":") {
		return nil
	}

	p.nextToken() // Move past ':'

	switchCase.Consequent = []ast.Statement{}

	for !p.curTokenIs(TokenKeyword, "case") && !p.curTokenIs(TokenKeyword, "default") && !p.curTokenIs(TokenPunctuation, "}") {
		if p.curTokenIs(TokenEOF, "") {
			p.errors = append(p.errors, "Unexpected end of file in switch case")
			return nil
		}

		stmt := p.parseStatement()
		if stmt != nil {
			switchCase.Consequent = append(switchCase.Consequent, stmt)
		}
		p.nextToken()
	}

	return switchCase
}

func (p *Parser) parseTryStatement() *ast.JSTryStatement {
	stmt := &ast.JSTryStatement{}

	// Parse try block
	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}
	stmt.Block = p.parseBlockStatement()

	// Parse catch clause (if present)
	if p.peekTokenIs(TokenKeyword, "catch") {
		p.nextToken() // Move to 'catch'
		stmt.Handler = p.parseCatchClause()
	}

	// Parse finally clause (if present)
	if p.peekTokenIs(TokenKeyword, "finally") {
		p.nextToken() // Move to 'finally'
		if !p.expectPeek(TokenPunctuation, "{") {
			return nil
		}
		stmt.Finalizer = p.parseBlockStatement()
	}

	// A try statement must have either a catch clause or a 'finally' clause (or both)
	if stmt.Handler == nil && stmt.Finalizer == nil {
		p.errors = append(p.errors, "Try statement must have either a catch clause or a finally clause")
		return nil
	}

	return stmt
}

func (p *Parser) parseCatchClause() *ast.JSCatchClause {
	clause := &ast.JSCatchClause{}

	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	p.nextToken() // Move past '('

	// Parse the exception parameter (if present)
	if p.curToken.Type == TokenIdentifier {
		clause.Param = &ast.Identifier{Name: p.curToken.Value}
		p.nextToken()
	}

	if !p.expectPeek(TokenPunctuation, ")") {
		return nil
	}

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	clause.Body = p.parseBlockStatement()

	return clause
}

func (p *Parser) parseClassDeclaration() *ast.JSClassDeclaration {
	decl := &ast.JSClassDeclaration{}

	p.nextToken() // Move past 'class'

	// Parse class name
	if !p.curTokenIs(TokenIdentifier, "") {
		p.errors = append(p.errors, "Expected class name")
		return nil
	}
	decl.ID = &ast.Identifier{Name: p.curToken.Value}

	// Parse extends clause if present
	if p.peekTokenIs(TokenKeyword, "extends") {
		p.nextToken() // Move to 'extends'
		p.nextToken() // Move to superclass name
		if !p.curTokenIs(TokenIdentifier, "") {
			p.errors = append(p.errors, "Expected superclass name")
			return nil
		}
		decl.SuperClass = &ast.Identifier{Name: p.curToken.Value}
	}

	// Parse class body
	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	decl.Body = p.parseClassBody()

	return decl
}

func (p *Parser) parseClassBody() *ast.JSClassBody {
	body := &ast.JSClassBody{Body: []ast.JSMethodDefinition{}}

	p.nextToken() // Move past '{'

	for !p.curTokenIs(TokenPunctuation, "}") {
		if p.curTokenIs(TokenEOF, "") {
			p.errors = append(p.errors, "Unexpected end of file, expected '}'")
			return nil
		}

		method := p.parseMethodDefinition()
		if method != nil {
			body.Body = append(body.Body, *method)
		}
	}

	p.nextToken() // Move past '}'

	return body
}

func (p *Parser) parseMethodDefinition() *ast.JSMethodDefinition {
	method := &ast.JSMethodDefinition{}

	// Check for static keyword
	if p.curTokenIs(TokenKeyword, "static") {
		method.Static = true
		p.nextToken()
	}

	// Parse method name or special keywords (get, set, constructor)
	if p.curTokenIs(TokenIdentifier, "") || p.curTokenIs(TokenKeyword, "get") || p.curTokenIs(TokenKeyword, "set") {
		method.Key = &ast.Identifier{Name: p.curToken.Value}
		method.Kind = "method"
		if p.curToken.Value == "get" || p.curToken.Value == "set" {
			method.Kind = p.curToken.Value
			p.nextToken() // Move to the actual method name
			if !p.curTokenIs(TokenIdentifier, "") {
				p.errors = append(p.errors, "Expected method name after get/set")
				return nil
			}
			method.Key = &ast.Identifier{Name: p.curToken.Value}
		} else if p.curToken.Value == "constructor" {
			method.Kind = "constructor"
		}
	} else {
		p.errors = append(p.errors, "Expected method name")
		return nil
	}

	// Parse method body
	if !p.expectPeek(TokenPunctuation, "(") {
		return nil
	}

	method.Value = p.parseMethod()

	return method
}

func (p *Parser) parseMethod() *ast.JSFunction {
	function := &ast.JSFunction{}

	function.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(TokenPunctuation, "{") {
		return nil
	}

	function.Body = p.parseBlockStatement()

	return function
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{}

	stmt.Expression = p.parseExpression(LOWEST)

	// In JavaScript, semicolons are optional in many cases
	// So we'll consume it if it's there, but won't require it
	if p.peekTokenIs(TokenPunctuation, ";") {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.JSMemberExpression{
		Object:   left,
		Computed: true,
	}

	p.nextToken() // Consume the '['

	exp.Property = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, "]") {
		return nil
	}

	return exp
}

func (p *Parser) parseConditionalExpression(left ast.Expression) ast.Expression {
	expression := &ast.JSConditionalExpression{Test: left}

	p.nextToken() // Consume the '?'

	expression.Consequent = p.parseExpression(LOWEST)

	if !p.expectPeek(TokenPunctuation, ":") {
		return nil
	}

	p.nextToken() // Consume the ':'

	expression.Alternate = p.parseExpression(LOWEST)

	return expression
}

func (p *Parser) parseNewExpression() ast.Expression {
	expression := &ast.JSNewExpression{}

	p.nextToken() // Consume 'new'

	expression.Callee = p.parseExpression(CALL)

	if p.peekTokenIs(TokenPunctuation, "(") {
		p.nextToken()
		expression.Arguments = p.parseExpressionList(")")
	}

	return expression
}

func (p *Parser) parseSpreadElement() ast.Expression {
	p.nextToken() // Consume '...'

	return &ast.JSSpreadElement{
		Argument: p.parseExpression(LOWEST),
	}
}

func statementToExpression(stmt ast.Statement) ast.Expression {
	switch s := stmt.(type) {
	case ast.Expression:
		return s
	case *ast.ExpressionStatement:
		return s.Expression
	case *ast.JSVariableDeclaration:
		// For a variable declaration, we'll return the first declarator's init expression
		// This is a simplification and might not be appropriate for all use cases
		if len(s.Declarations) > 0 && s.Declarations[0].Init != nil {
			return s.Declarations[0].Init
		}
	}
	return nil // or handle this case as appropriate for your use case
}

func (p *Parser) Errors() []string {
	return p.errors
}
