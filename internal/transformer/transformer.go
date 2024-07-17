package transformer

import (
	"fmt"
	goast "go/ast"
	"go/token"
	"strconv"
	"strings"

	"github.com/FedeBP/js2go/pkg/ast"
	"golang.org/x/tools/go/ast/astutil"
)

type Transformer struct {
	usedHelpers map[string]bool
}

func NewTransformer() *Transformer {
	return &Transformer{
		usedHelpers: make(map[string]bool),
	}
}

func (t *Transformer) markHelperAsUsed(helper string) {
	t.usedHelpers[helper] = true
}

func (t *Transformer) Transform(jsAST *ast.Program) (*goast.File, error) {
	file := &goast.File{
		Name: goast.NewIdent("main"),
	}

	var decls []goast.Decl
	var mainStmts []goast.Stmt

	for _, stmt := range jsAST.Statements {
		goNode, err := t.transformNode(stmt)
		if err != nil {
			return nil, err
		}
		switch node := goNode.(type) {
		case *goast.FuncDecl:
			decls = append(decls, node)
		case goast.Stmt:
			mainStmts = append(mainStmts, node)
		}
	}

	imports := &goast.GenDecl{
		Tok: token.IMPORT,
		Specs: []goast.Spec{
			&goast.ImportSpec{
				Path: &goast.BasicLit{Kind: token.STRING, Value: `"fmt"`},
			},
		},
	}

	if len(t.usedHelpers) > 0 {
		imports.Specs = append(imports.Specs, &goast.ImportSpec{
			Path: &goast.BasicLit{Kind: token.STRING, Value: `"github.com/FedeBP/js2go/runtime"`},
		})
	}

	decls = append([]goast.Decl{imports}, decls...)

	mainFunc := &goast.FuncDecl{
		Name: goast.NewIdent("main"),
		Type: &goast.FuncType{},
		Body: &goast.BlockStmt{
			List: mainStmts,
		},
	}
	decls = append(decls, mainFunc)

	file.Decls = decls

	return file, nil
}

func (t *Transformer) transformNode(node ast.Node) (goast.Node, error) {
	switch n := node.(type) {
	case *ast.JSFunction:
		return t.transformFunctionDeclaration(n)
	case *ast.JSVariableDeclaration:
		return t.transformVariableDeclaration(n)
	case *ast.ExpressionStatement:
		return t.transformExpressionStatement(n)
	case *ast.JSReturnStatement:
		return t.transformReturnStatement(n)
	case *ast.JSIfStatement:
		return t.transformIfStatement(n)
	case *ast.JSForStatement:
		return t.transformForStatement(n)
	case ast.Expression:
		return t.transformExpression(n)
	default:
		return nil, fmt.Errorf("unsupported node type: %T", n)
	}
}

func (t *Transformer) transformExpression(expr ast.Expression) (goast.Expr, error) {
	switch e := expr.(type) {
	case *ast.Identifier:
		return goast.NewIdent(e.Name), nil
	case *ast.StringLiteral:
		return t.transformStringLiteral(e), nil
	case *ast.NumberLiteral:
		return &goast.BasicLit{
			Kind:  token.FLOAT,
			Value: fmt.Sprintf("%v", e.Value),
		}, nil
	case *ast.BooleanLiteral:
		return &goast.Ident{Name: fmt.Sprintf("%v", e.Value)}, nil
	case *ast.JSCallExpression:
		return t.transformCallExpression(e)
	case *ast.JSBinaryExpression:
		return t.transformBinaryExpression(e)
	case *ast.JSUnaryExpression:
		return t.transformUnaryExpression(e)
	case *ast.JSMemberExpression:
		return t.transformMemberExpression(e)
	case *ast.JSArrowFunction:
		return t.transformArrowFunction(e)
	case *ast.JSObjectExpression:
		return t.transformObjectExpression(e)
	case *ast.JSArrayExpression:
		return t.transformArrayExpression(e)
	case *ast.JSConditionalExpression:
		return t.transformConditionalExpression(e)
	case *ast.JSNewExpression:
		return t.transformNewExpression(e)
	default:
		return nil, fmt.Errorf("unsupported expression type: %T", e)
	}
}

func (t *Transformer) transformStringLiteral(lit *ast.StringLiteral) goast.Expr {
	value := strings.Trim(lit.Value, "\"'")
	return &goast.BasicLit{
		Kind:  token.STRING,
		Value: strconv.Quote(value),
	}
}

func (t *Transformer) transformFunctionDeclaration(fn *ast.JSFunction) (*goast.FuncDecl, error) {
	params := &goast.FieldList{}
	for _, p := range fn.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(p.Name)},
			Type:  goast.NewIdent("interface{}"),
		})
	}

	body, err := t.transformBlockStatement(fn.Body)
	if err != nil {
		return nil, err
	}

	var bodyStmts []goast.Stmt
	for _, param := range fn.Parameters {
		bodyStmts = append(bodyStmts, &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(param.Name + "Str")},
			Tok: token.DEFINE,
			Rhs: []goast.Expr{
				&goast.CallExpr{
					Fun: &goast.SelectorExpr{
						X:   goast.NewIdent("fmt"),
						Sel: goast.NewIdent("Sprint"),
					},
					Args: []goast.Expr{goast.NewIdent(param.Name)},
				},
			},
		})
	}

	// Replace parameter uses in the body
	astutil.Apply(body, nil, func(c *astutil.Cursor) bool {
		n := c.Node()
		if id, ok := n.(*goast.Ident); ok {
			for _, param := range fn.Parameters {
				if id.Name == param.Name {
					c.Replace(goast.NewIdent(param.Name + "Str"))
					break
				}
			}
		}
		return true
	})

	bodyStmts = append(bodyStmts, body.List...)

	return &goast.FuncDecl{
		Name: goast.NewIdent(fn.Name.Name),
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
		},
		Body: &goast.BlockStmt{List: bodyStmts},
	}, nil
}

func (t *Transformer) transformVariableDeclaration(decl *ast.JSVariableDeclaration) (goast.Stmt, error) {
	if len(decl.Declarations) != 1 {
		return nil, fmt.Errorf("expected single declaration, got %d", len(decl.Declarations))
	}

	d := decl.Declarations[0]
	init, err := t.transformExpression(d.Init)
	if err != nil {
		return nil, err
	}

	return &goast.AssignStmt{
		Lhs: []goast.Expr{goast.NewIdent(d.ID.Name)},
		Tok: token.DEFINE,
		Rhs: []goast.Expr{init},
	}, nil
}

func (t *Transformer) transformExpressionStatement(stmt *ast.ExpressionStatement) (goast.Stmt, error) {
	expr, err := t.transformExpression(stmt.Expression)
	if err != nil {
		return nil, err
	}

	return &goast.ExprStmt{X: expr}, nil
}

func (t *Transformer) transformCallExpression(expr *ast.JSCallExpression) (goast.Expr, error) {
	if memberExpr, ok := expr.Callee.(*ast.JSMemberExpression); ok {
		if ident, ok := memberExpr.Object.(*ast.Identifier); ok && ident.Name == "console" {
			if propIdent, ok := memberExpr.Property.(*ast.Identifier); ok && propIdent.Name == "log" {
				args := make([]goast.Expr, 0, len(expr.Arguments))
				for _, arg := range expr.Arguments {
					transformedArg, err := t.transformExpression(arg)
					if err != nil {
						return nil, err
					}
					args = append(args, transformedArg)
				}
				return &goast.CallExpr{
					Fun: &goast.SelectorExpr{
						X:   goast.NewIdent("fmt"),
						Sel: goast.NewIdent("Println"),
					},
					Args: args,
				}, nil
			}
		}
	}

	callee, err := t.transformExpression(expr.Callee)
	if err != nil {
		return nil, err
	}

	args := make([]goast.Expr, 0, len(expr.Arguments))
	for _, arg := range expr.Arguments {
		transformedArg, err := t.transformExpression(arg)
		if err != nil {
			return nil, err
		}
		if litArg, ok := transformedArg.(*goast.BasicLit); ok && litArg.Kind == token.STRING {
			unquoted, err := strconv.Unquote(litArg.Value)
			if err == nil {
				transformedArg = &goast.BasicLit{
					Kind:  token.STRING,
					Value: strconv.Quote(unquoted),
				}
			}
		}
		args = append(args, transformedArg)
	}

	return &goast.CallExpr{
		Fun:  callee,
		Args: args,
	}, nil
}

func (t *Transformer) transformBinaryExpression(expr *ast.JSBinaryExpression) (goast.Expr, error) {
	left, err := t.transformExpression(expr.Left)
	if err != nil {
		return nil, err
	}
	right, err := t.transformExpression(expr.Right)
	if err != nil {
		return nil, err
	}

	if expr.Operator == "+" {
		if leftLit, leftOk := left.(*goast.BasicLit); leftOk && leftLit.Kind == token.STRING {
			if rightLit, rightOk := right.(*goast.BasicLit); rightOk && rightLit.Kind == token.STRING {
				leftVal, _ := strconv.Unquote(leftLit.Value)
				rightVal, _ := strconv.Unquote(rightLit.Value)
				return &goast.BasicLit{
					Kind:  token.STRING,
					Value: strconv.Quote(leftVal + rightVal),
				}, nil
			}
		}
		return &goast.BinaryExpr{
			X:  left,
			Op: token.ADD,
			Y:  right,
		}, nil
	}

	op, err := convertBinaryOperator(expr.Operator)
	if err != nil {
		return nil, err
	}

	return &goast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, nil
}

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
	case "%":
		return token.REM, nil
	case "==":
		return token.EQL, nil
	case "!=":
		return token.NEQ, nil
	case "<":
		return token.LSS, nil
	case "<=":
		return token.LEQ, nil
	case ">":
		return token.GTR, nil
	case ">=":
		return token.GEQ, nil
	case "&&":
		return token.LAND, nil
	case "||":
		return token.LOR, nil
	case "&":
		return token.AND, nil
	case "|":
		return token.OR, nil
	case "^":
		return token.XOR, nil
	case "<<":
		return token.SHL, nil
	case ">>":
		return token.SHR, nil
	default:
		return token.ILLEGAL, fmt.Errorf("unsupported binary operator: %s", op)
	}
}

func (t *Transformer) transformUnaryExpression(expr *ast.JSUnaryExpression) (goast.Expr, error) {
	operand, err := t.transformExpression(expr.Argument)
	if err != nil {
		return nil, err
	}

	op, err := convertUnaryOperator(expr.Operator)
	if err != nil {
		return nil, err
	}

	return &goast.UnaryExpr{
		Op: op,
		X:  operand,
	}, nil
}

func convertUnaryOperator(op string) (token.Token, error) {
	switch op {
	case "+":
		return token.ADD, nil
	case "-":
		return token.SUB, nil
	case "!":
		return token.NOT, nil
	case "~":
		return token.XOR, nil
	default:
		return token.ILLEGAL, fmt.Errorf("unsupported unary operator: %s", op)
	}
}

func createParamConversionStmt(paramName string) goast.Stmt {
	return &goast.AssignStmt{
		Lhs: []goast.Expr{
			goast.NewIdent(paramName + "Str"),
			goast.NewIdent("_"),
		},
		Tok: token.DEFINE,
		Rhs: []goast.Expr{
			&goast.CallExpr{
				Fun: goast.NewIdent("fmt.Sprintf"),
				Args: []goast.Expr{
					&goast.BasicLit{Kind: token.STRING, Value: "\"%v\""},
					goast.NewIdent(paramName),
				},
			},
		},
	}
}

func (t *Transformer) transformReturnStatement(stmt *ast.JSReturnStatement) (goast.Stmt, error) {
	expr, err := t.transformExpression(stmt.Value)
	if err != nil {
		return nil, err
	}
	return &goast.ReturnStmt{Results: []goast.Expr{expr}}, nil
}

func (t *Transformer) transformIfStatement(stmt *ast.JSIfStatement) (goast.Stmt, error) {
	cond, err := t.transformExpression(stmt.Test)
	if err != nil {
		return nil, err
	}

	body, err := t.transformBlockStatement(stmt.Consequent)
	if err != nil {
		return nil, err
	}

	var elseBody goast.Stmt
	if stmt.Alternate != nil {
		elseNode, err := t.transformNode(stmt.Alternate)
		if err != nil {
			return nil, err
		}
		if elseStmt, ok := elseNode.(goast.Stmt); ok {
			elseBody = elseStmt
		} else {
			return nil, fmt.Errorf("expected statement for else body, got %T", elseNode)
		}
	}

	return &goast.IfStmt{
		Cond: cond,
		Body: body,
		Else: elseBody,
	}, nil
}

func (t *Transformer) transformForStatement(stmt *ast.JSForStatement) (goast.Stmt, error) {
	var init goast.Stmt
	var cond goast.Expr
	var post goast.Stmt
	var err error

	if stmt.Init != nil {
		initNode, err := t.transformNode(stmt.Init)
		if err != nil {
			return nil, err
		}
		if initStmt, ok := initNode.(goast.Stmt); ok {
			init = initStmt
		} else {
			return nil, fmt.Errorf("expected statement for for loop init, got %T", initNode)
		}
	}

	if stmt.Test != nil {
		cond, err = t.transformExpression(stmt.Test)
		if err != nil {
			return nil, err
		}
	}

	if stmt.Update != nil {
		updateExpr, err := t.transformExpression(stmt.Update)
		if err != nil {
			return nil, err
		}
		post = &goast.ExprStmt{X: updateExpr}
	}

	body, err := t.transformBlockStatement(stmt.Body)
	if err != nil {
		return nil, err
	}

	return &goast.ForStmt{
		Init: init,
		Cond: cond,
		Post: post,
		Body: body,
	}, nil
}

func (t *Transformer) transformBlockStatement(block *ast.BlockStatement) (*goast.BlockStmt, error) {
	stmts := make([]goast.Stmt, len(block.Statements))
	for i, stmt := range block.Statements {
		transformed, err := t.transformNode(stmt)
		if err != nil {
			return nil, err
		}
		stmts[i] = transformed.(goast.Stmt)
	}
	return &goast.BlockStmt{List: stmts}, nil
}

func (t *Transformer) transformMemberExpression(expr *ast.JSMemberExpression) (goast.Expr, error) {
	object, err := t.transformExpression(expr.Object)
	if err != nil {
		return nil, err
	}

	if expr.Computed {
		t.markHelperAsUsed("GetIndexedValue")
		property, err := t.transformExpression(expr.Property)
		if err != nil {
			return nil, err
		}

		return &goast.CallExpr{
			Fun: &goast.SelectorExpr{
				X:   goast.NewIdent("runtime"),
				Sel: goast.NewIdent("GetIndexedValue"),
			},
			Args: []goast.Expr{object, property},
		}, nil
	} else {
		if ident, ok := expr.Property.(*ast.Identifier); ok {
			return &goast.SelectorExpr{
				X:   object,
				Sel: goast.NewIdent(ident.Name),
			}, nil
		}
		return nil, fmt.Errorf("non-identifier property in non-computed member expression")
	}
}

func (t *Transformer) transformArrowFunction(fn *ast.JSArrowFunction) (goast.Expr, error) {
	params := &goast.FieldList{}
	for _, p := range fn.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(p.Name)},
			Type:  goast.NewIdent("interface{}"),
		})
	}

	var body *goast.BlockStmt
	var err error
	if blockStmt, ok := fn.Body.(*ast.BlockStatement); ok {
		body, err = t.transformBlockStatement(blockStmt)
		if err != nil {
			return nil, err
		}
	} else {
		expr, err := t.transformExpression(fn.Body.(ast.Expression))
		if err != nil {
			return nil, err
		}
		body = &goast.BlockStmt{
			List: []goast.Stmt{
				&goast.ReturnStmt{
					Results: []goast.Expr{expr},
				},
			},
		}
	}

	var bodyStmts []goast.Stmt
	for _, p := range fn.Parameters {
		bodyStmts = append(bodyStmts, createParamConversionStmt(p.Name))
	}
	bodyStmts = append(bodyStmts, body.List...)

	return &goast.FuncLit{
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
		},
		Body: &goast.BlockStmt{List: bodyStmts},
	}, nil
}

func (t *Transformer) transformObjectExpression(obj *ast.JSObjectExpression) (goast.Expr, error) {
	compLit := &goast.CompositeLit{
		Type: &goast.MapType{
			Key:   goast.NewIdent("string"),
			Value: goast.NewIdent("interface{}"),
		},
		Elts: []goast.Expr{},
	}

	for _, prop := range obj.Properties {
		key, err := t.transformObjectKey(prop.Key)
		if err != nil {
			return nil, err
		}

		value, err := t.transformExpression(prop.Value)
		if err != nil {
			return nil, err
		}

		compLit.Elts = append(compLit.Elts, &goast.KeyValueExpr{
			Key:   key,
			Value: value,
		})
	}

	return compLit, nil
}

func (t *Transformer) transformObjectKey(key ast.Expression) (goast.Expr, error) {
	switch k := key.(type) {
	case *ast.Identifier:
		return &goast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", k.Name),
		}, nil
	case *ast.StringLiteral:
		return &goast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", k.Value),
		}, nil
	case *ast.NumberLiteral:
		return &goast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", fmt.Sprintf("%v", k.Value)),
		}, nil
	default:
		keyExpr, err := t.transformExpression(k)
		if err != nil {
			return nil, err
		}
		return &goast.CallExpr{
			Fun: &goast.SelectorExpr{
				X:   goast.NewIdent("fmt"),
				Sel: goast.NewIdent("Sprintf"),
			},
			Args: []goast.Expr{
				&goast.BasicLit{Kind: token.STRING, Value: "%v"},
				keyExpr,
			},
		}, nil
	}
}

func (t *Transformer) transformArrayExpression(arr *ast.JSArrayExpression) (goast.Expr, error) {
	compLit := &goast.CompositeLit{
		Type: &goast.ArrayType{
			Elt: goast.NewIdent("interface{}"),
		},
		Elts: []goast.Expr{},
	}

	for _, elem := range arr.Elements {
		if elem == nil {
			compLit.Elts = append(compLit.Elts, goast.NewIdent("nil"))
			continue
		}

		if spread, ok := elem.(*ast.JSSpreadElement); ok {
			t.markHelperAsUsed("SpreadToArray")
			spreadExpr, err := t.transformExpression(spread.Argument)
			if err != nil {
				return nil, err
			}
			compLit.Elts = append(compLit.Elts, &goast.CallExpr{
				Fun: &goast.SelectorExpr{
					X:   goast.NewIdent("runtime"),
					Sel: goast.NewIdent("SpreadToArray"),
				},
				Args: []goast.Expr{spreadExpr},
			})
		} else {
			expr, err := t.transformExpression(elem)
			if err != nil {
				return nil, err
			}
			compLit.Elts = append(compLit.Elts, expr)
		}
	}

	return compLit, nil
}

func (t *Transformer) transformConditionalExpression(expr *ast.JSConditionalExpression) (goast.Expr, error) {
	t.markHelperAsUsed("Ternary")

	test, err := t.transformExpression(expr.Test)
	if err != nil {
		return nil, err
	}

	consequent, err := t.transformExpression(expr.Consequent)
	if err != nil {
		return nil, err
	}

	alternate, err := t.transformExpression(expr.Alternate)
	if err != nil {
		return nil, err
	}

	return &goast.CallExpr{
		Fun: &goast.SelectorExpr{
			X:   goast.NewIdent("runtime"),
			Sel: goast.NewIdent("Ternary"),
		},
		Args: []goast.Expr{
			test,
			&goast.FuncLit{
				Type: &goast.FuncType{
					Params:  &goast.FieldList{},
					Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
				},
				Body: &goast.BlockStmt{
					List: []goast.Stmt{
						&goast.ReturnStmt{Results: []goast.Expr{consequent}},
					},
				},
			},
			&goast.FuncLit{
				Type: &goast.FuncType{
					Params:  &goast.FieldList{},
					Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
				},
				Body: &goast.BlockStmt{
					List: []goast.Stmt{
						&goast.ReturnStmt{Results: []goast.Expr{alternate}},
					},
				},
			},
		},
	}, nil
}

func (t *Transformer) transformNewExpression(expr *ast.JSNewExpression) (goast.Expr, error) {
	constructor, err := t.transformExpression(expr.Callee)
	if err != nil {
		return nil, err
	}

	args := make([]goast.Expr, 0, len(expr.Arguments))
	for _, arg := range expr.Arguments {
		transformedArg, err := t.transformExpression(arg)
		if err != nil {
			return nil, err
		}
		args = append(args, transformedArg)
	}

	t.markHelperAsUsed("NewObject")

	return &goast.CallExpr{
		Fun: &goast.SelectorExpr{
			X:   goast.NewIdent("runtime"),
			Sel: goast.NewIdent("NewObject"),
		},
		Args: append([]goast.Expr{constructor}, args...),
	}, nil
}
