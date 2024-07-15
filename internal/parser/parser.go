package parser

import (
	"fmt"
	"strings"

	esp "github.com/MichaelCombs28/goesprima"
)

type Parser struct {
	code   string
	tokens []string
	pos    int
}

func NewParser(code string) *Parser {
	return &Parser{
		code:   code,
		tokens: tokenize(code),
		pos:    0,
	}
}

func tokenize(code string) []string {
	// This is a very basic tokenizer. You might need to improve it for more complex JavaScript.
	code = strings.ReplaceAll(code, "\n", " ")
	code = strings.ReplaceAll(code, "(", " ( ")
	code = strings.ReplaceAll(code, ")", " ) ")
	code = strings.ReplaceAll(code, "{", " { ")
	code = strings.ReplaceAll(code, "}", " } ")
	code = strings.ReplaceAll(code, ";", " ; ")
	return strings.Fields(code)
}

func (p *Parser) next() string {
	if p.pos < len(p.tokens) {
		token := p.tokens[p.pos]
		p.pos++
		return token
	}
	return ""
}

func (p *Parser) peek() string {
	if p.pos < len(p.tokens) {
		return p.tokens[p.pos]
	}
	return ""
}

func (p *Parser) expect(expected string) error {
	token := p.next()
	if token != expected {
		return fmt.Errorf("expected '%s', got '%s'", expected, token)
	}
	return nil
}

func GenerateJavaScriptAST(code string) (*goesprima.Program, error) {
	parser := NewParser(code)
	statements := []goesprima.StatementListItem{}

	for parser.peek() != "" {
		stmt, err := parser.parseStatementListItem()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stmt)
	}

	return &goesprima.Program{
		Body: statements,
	}, nil
}

func (p *Parser) parseStatementListItem() (goesprima.StatementListItem, error) {
	switch p.peek() {
	case "function":
		return p.parseFunctionDeclaration()
	case "var", "let", "const":
		return p.parseVariableDeclaration()
	default:
		return p.parseStatement()
	}
}

func (p *Parser) parseStatement() (goesprima.Statement, error) {
	return p.parseExpressionStatement()
}

func (p *Parser) parseFunctionDeclaration() (*goesprima.FunctionDeclaration, error) {
	p.next() // consume "function"
	name := p.next()

	if err := p.expect("("); err != nil {
		return nil, err
	}

	params := []*goesprima.Identifier{}
	for p.peek() != ")" {
		params = append(params, &goesprima.Identifier{Name: p.next()})
		if p.peek() == "," {
			p.next()
		}
	}
	p.next() // consume ")"

	body, err := p.parseBlockStatement()
	if err != nil {
		return nil, err
	}

	return &goesprima.FunctionDeclaration{
		Name:       &goesprima.Identifier{Name: name},
		Parameters: params,
		Body:       body,
	}, nil
}

func (p *Parser) parseBlockStatement() (*goesprima.BlockStatement, error) {
	if err := p.expect("{"); err != nil {
		return nil, err
	}

	body := []goesprima.Statement{}
	for p.peek() != "}" {
		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		body = append(body, stmt)
	}
	p.next() // consume "}"

	return &goesprima.BlockStatement{Items: body}, nil
}

func (p *Parser) parseVariableDeclaration() (*goesprima.VariableDeclaration, error) {
	kind := goesprima.VariableDeclarationType(p.next()) // var, let, or const
	id := p.next()                                      // variable name

	var init goesprima.Expression
	if p.peek() == "=" {
		p.next() // consume "="
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		init = expr
	}

	if err := p.expect(";"); err != nil {
		return nil, err
	}

	return &goesprima.VariableDeclaration{
		Kind: kind,
		Declarations: []goesprima.VariableDeclarator{
			{
				ID:   &goesprima.Identifier{Name: id},
				Init: init,
			},
		},
	}, nil
}

func (p *Parser) parseExpressionStatement() (*goesprima.ExpressionStatement, error) {
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(";"); err != nil {
		return nil, err
	}

	return &goesprima.ExpressionStatement{Expression: expr}, nil
}

func (p *Parser) parseExpression() (goesprima.Expression, error) {
	left, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}

	for p.peek() == "+" || p.peek() == "-" || p.peek() == "*" || p.peek() == "/" {
		operator := p.next()
		right, err := p.parsePrimaryExpression()
		if err != nil {
			return nil, err
		}
		left = &goesprima.BinaryExpression{
			Operator: operator,
			Left:     left,
			Right:    right,
		}
	}

	return left, nil
}

func (p *Parser) parsePrimaryExpression() (goesprima.Expression, error) {
	switch {
	case p.peek() == "(":
		p.next() // consume "("
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		if err := p.expect(")"); err != nil {
			return nil, err
		}
		return expr, nil
	case isLiteral(p.peek()):
		return parseLiteral(p.next()), nil
	case isIdentifier(p.peek()):
		id := p.next()
		if p.peek() == "(" {
			return p.parseCallExpression(id)
		}
		return &goesprima.Identifier{Name: id}, nil
	default:
		return nil, fmt.Errorf("unexpected token: %s", p.peek())
	}
}

func (p *Parser) parseCallExpression(callee string) (*goesprima.CallExpression, error) {
	p.next() // consume "("

	args := []goesprima.Expression{}
	for p.peek() != ")" {
		arg, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
		if p.peek() == "," {
			p.next()
		}
	}
	p.next() // consume ")"

	return &goesprima.CallExpression{
		Callee:    &goesprima.Identifier{Name: callee},
		Arguments: args,
	}, nil
}

func parseLiteral(token string) goesprima.Literal {
	if token[0] == '"' || token[0] == '\'' {
		return goesprima.StringLiteral(strings.Trim(token, "'\""))
	}
	if isNumber(token) {
		return goesprima.NumberLiteral(token)
	}
	return nil // This should never happen if isLiteral is correct
}

func isLiteral(token string) bool {
	return token[0] == '"' || token[0] == '\'' || isNumber(token)
}

func isNumber(token string) bool {
	_, err := fmt.Sscanf(token, "%f", new(float64))
	return err == nil
}

func isIdentifier(token string) bool {
	return token != "" && (token[0] == '_' || (token[0] >= 'a' && token[0] <= 'z') || (token[0] >= 'A' && token[0] <= 'Z'))
}
