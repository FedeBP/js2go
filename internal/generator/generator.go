package generator

import (
	"bytes"
	"go/ast"
	"go/printer"
	"go/token"

	jsast "github.com/FedeBP/js2go/pkg/ast"
)

// Generator holds the state for Go code generation
type Generator struct {
	fset *token.FileSet
}

// NewGenerator creates a new Generator
func NewGenerator() *Generator {
	return &Generator{
		fset: token.NewFileSet(),
	}
}

// GenerateGoAST converts a JavaScript AST to a Go AST
func (g *Generator) GenerateGoAST(jsAst *jsast.Program) (*ast.File, error) {
	// Create the main package declaration
	file := &ast.File{
		Name: ast.NewIdent("main"),
	}

	// Transform each statement in the JavaScript AST
	for _, stmt := range jsAst.Statements {
		goStmt, err := g.transformStatement(stmt)
		if err != nil {
			return nil, err
		}
		file.Decls = append(file.Decls, goStmt)
	}

	return file, nil
}

// transformStatement converts a JavaScript statement to a Go statement
func (g *Generator) transformStatement(stmt jsast.Statement) (ast.Decl, error) {
	switch s := stmt.(type) {
	case *jsast.JSFunction:
		return g.transformFunctionDeclaration(s)
	case *jsast.JSVariableDeclaration:
		return g.transformVariableDeclaration(s)
	case *jsast.ExpressionStatement:
		return g.transformExpressionStatement(s)
	default:
		// Handle other statement types or return an error
		return nil, nil
	}
}

// transformFunctionDeclaration converts a JavaScript function to a Go function
func (g *Generator) transformFunctionDeclaration(fn *jsast.JSFunction) (*ast.FuncDecl, error) {
	// Create function parameters
	params := &ast.FieldList{}
	for _, param := range fn.Parameters {
		params.List = append(params.List, &ast.Field{
			Names: []*ast.Ident{ast.NewIdent(param.Name)},
			Type:  ast.NewIdent("interface{}"), // Use interface{} as a generic type
		})
	}

	// Transform function body
	body, err := g.transformBlockStatement(fn.Body)
	if err != nil {
		return nil, err
	}

	return &ast.FuncDecl{
		Name: ast.NewIdent(fn.Name.Name),
		Type: &ast.FuncType{
			Params: params,
			Results: &ast.FieldList{
				List: []*ast.Field{{Type: ast.NewIdent("interface{}")}},
			},
		},
		Body: body,
	}, nil
}

// transformBlockStatement converts a JavaScript block statement to a Go block statement
func (g *Generator) transformBlockStatement(block *jsast.BlockStatement) (*ast.BlockStmt, error) {
	var stmts []ast.Stmt
	for _, stmt := range block.Statements {
		goStmt, err := g.transformStatementToStmt(stmt)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, goStmt)
	}
	return &ast.BlockStmt{List: stmts}, nil
}

// transformStatementToStmt converts a JavaScript statement to a Go statement
func (g *Generator) transformStatementToStmt(stmt jsast.Statement) (ast.Stmt, error) {
	switch s := stmt.(type) {
	case *jsast.JSReturnStatement:
		return g.transformReturnStatement(s)
	case *jsast.ExpressionStatement:
		expr, err := g.transformExpression(s.Expression)
		if err != nil {
			return nil, err
		}
		return &ast.ExprStmt{X: expr}, nil
	default:
		// Handle other statement types or return an error
		return nil, nil
	}
}

// transformReturnStatement converts a JavaScript return statement to a Go return statement
func (g *Generator) transformReturnStatement(stmt *jsast.JSReturnStatement) (*ast.ReturnStmt, error) {
	if stmt.Value == nil {
		return &ast.ReturnStmt{}, nil
	}

	value, err := g.transformExpression(stmt.Value)
	if err != nil {
		return nil, err
	}

	return &ast.ReturnStmt{Results: []ast.Expr{value}}, nil
}

// transformExpression converts a JavaScript expression to a Go expression
func (g *Generator) transformExpression(expr jsast.Expression) (ast.Expr, error) {
	switch e := expr.(type) {
	case *jsast.JSBinaryExpression:
		return g.transformBinaryExpression(e)
	case *jsast.StringLiteral:
		return &ast.BasicLit{
			Kind:  token.STRING,
			Value: `"` + e.Value + `"`,
		}, nil
	case *jsast.Identifier:
		return ast.NewIdent(e.Name), nil
	case *jsast.JSCallExpression:
		return g.transformCallExpression(e)
	default:
		// Handle other expression types or return an error
		return nil, nil
	}
}

// transformBinaryExpression converts a JavaScript binary expression to a Go binary expression
func (g *Generator) transformBinaryExpression(expr *jsast.JSBinaryExpression) (*ast.BinaryExpr, error) {
	left, err := g.transformExpression(expr.Left)
	if err != nil {
		return nil, err
	}

	right, err := g.transformExpression(expr.Right)
	if err != nil {
		return nil, err
	}

	op, err := convertBinaryOperator(expr.Operator)
	if err != nil {
		return nil, err
	}

	return &ast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, nil
}

// transformCallExpression converts a JavaScript call expression to a Go call expression
func (g *Generator) transformCallExpression(expr *jsast.JSCallExpression) (*ast.CallExpr, error) {
	fun, err := g.transformExpression(expr.Callee)
	if err != nil {
		return nil, err
	}

	args := make([]ast.Expr, len(expr.Arguments))
	for i, arg := range expr.Arguments {
		args[i], err = g.transformExpression(arg)
		if err != nil {
			return nil, err
		}
	}

	return &ast.CallExpr{
		Fun:  fun,
		Args: args,
	}, nil
}

// transformVariableDeclaration converts a JavaScript variable declaration to a Go variable declaration
func (g *Generator) transformVariableDeclaration(decl *jsast.JSVariableDeclaration) (*ast.GenDecl, error) {
	spec := &ast.ValueSpec{
		Names: []*ast.Ident{ast.NewIdent(decl.Declarations[0].ID.Name)},
	}

	if decl.Declarations[0].Init != nil {
		value, err := g.transformExpression(decl.Declarations[0].Init)
		if err != nil {
			return nil, err
		}
		spec.Values = []ast.Expr{value}
	}

	return &ast.GenDecl{
		Tok:   token.VAR,
		Specs: []ast.Spec{spec},
	}, nil
}

// transformExpressionStatement converts a JavaScript expression statement to a Go statement
func (g *Generator) transformExpressionStatement(stmt *jsast.ExpressionStatement) (ast.Decl, error) {
	expr, err := g.transformExpression(stmt.Expression)
	if err != nil {
		return nil, err
	}

	// Wrap the expression in a function to make it a valid top-level declaration
	return &ast.FuncDecl{
		Name: ast.NewIdent("init"),
		Type: &ast.FuncType{},
		Body: &ast.BlockStmt{
			List: []ast.Stmt{
				&ast.ExprStmt{X: expr},
			},
		},
	}, nil
}

// convertBinaryOperator converts a JavaScript binary operator to a Go token
func convertBinaryOperator(op string) (token.Token, error) {
	switch op {
	case "+":
		return token.ADD, nil
	case "-":
		return token.SUB, nil
	case "*":
		return token.MUL, nil
	case "/":
		return token.QUO, nil
	// Add more operators as needed
	default:
		return token.ILLEGAL, nil
	}
}

// GenerateGoCode generates Go code from a Go AST
func (g *Generator) GenerateGoCode(file *ast.File) (string, error) {
	var buf bytes.Buffer

	err := printer.Fprint(&buf, g.fset, file)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}
