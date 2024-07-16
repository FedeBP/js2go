package transformer

import (
	"fmt"

	"github.com/FedeBP/js2go/pkg/ast"
	goast "go/ast"
	"go/token"
)

type MultiDeclNode struct {
	Decls []goast.Decl
}

func (n *MultiDeclNode) Pos() token.Pos { return token.NoPos }
func (n *MultiDeclNode) End() token.Pos { return token.NoPos }

func Transform(jsAST *ast.Program) (*goast.File, error) {
	goFile := &goast.File{
		Name: &goast.Ident{Name: "main"},
	}

	for _, stmt := range jsAST.Statements {
		goNode, err := transformNode(stmt)
		if err != nil {
			return nil, err
		}
		switch node := goNode.(type) {
		case goast.Decl:
			goFile.Decls = append(goFile.Decls, node)
		case goast.Stmt:
			goFile.Decls = append(goFile.Decls, &goast.FuncDecl{
				Name: goast.NewIdent("init"),
				Type: &goast.FuncType{},
				Body: &goast.BlockStmt{List: []goast.Stmt{node}},
			})
		case *MultiDeclNode:
			goFile.Decls = append(goFile.Decls, node.Decls...)
		default:
			return nil, fmt.Errorf("unexpected node type: %T", goNode)
		}
	}

	return goFile, nil
}

func transformNode(node ast.Node) (goast.Node, error) {
	switch n := node.(type) {
	case *ast.JSVariableDeclaration:
		return transformVariableDeclaration(n)
	case *ast.JSFunction:
		return transformFunctionDeclaration(n)
	case *ast.JSArrowFunction:
		return transformArrowFunction(n)
	case *ast.JSAssignmentExpression:
		return transformAssignmentExpression(n)
	case *ast.JSIfStatement:
		return transformIfStatement(n)
	case *ast.JSForStatement:
		return transformForStatement(n)
	case *ast.JSWhileStatement:
		return transformWhileStatement(n)
	case *ast.JSDoWhileStatement:
		return transformDoWhileStatement(n)
	case *ast.JSReturnStatement:
		return transformReturnStatement(n)
	case *ast.JSSwitchStatement:
		return transformSwitchStatement(n)
	case *ast.JSTryStatement:
		return transformTryStatement(n)
	case *ast.JSClassDeclaration:
		decls, err := transformClassDeclaration(n)
		if err != nil {
			return nil, err
		}
		// Since we can't return []goast.Decl directly as goast.Node,
		// we'll wrap it in a custom node type
		return &MultiDeclNode{Decls: decls}, nil
	case *ast.ExpressionStatement:
		return transformExpressionStatement(n)
	default:
		return nil, fmt.Errorf("unsupported node type: %T", n)
	}
}

func transformStatement(stmt ast.Statement) (goast.Stmt, error) {
	switch s := stmt.(type) {
	case *ast.JSVariableDeclaration:
		return transformVariableDeclarationStmt(s)
	case *ast.JSIfStatement:
		return transformIfStatement(s)
	case *ast.JSForStatement:
		return transformForStatement(s)
	case *ast.JSForInOfStatement:
		return transformForInOfStatement(s)
	case *ast.JSWhileStatement:
		return transformWhileStatement(s)
	case *ast.JSDoWhileStatement:
		return transformDoWhileStatement(s)
	case *ast.JSReturnStatement:
		return transformReturnStatement(s)
	case *ast.JSSwitchStatement:
		return transformSwitchStatement(s)
	case *ast.JSTryStatement:
		return transformTryStatement(s)
	case *ast.ExpressionStatement:
		return transformExpressionStatement(s)
	default:
		return nil, fmt.Errorf("unsupported statement type: %T", s)
	}
}

func transformExpression(expr ast.Expression) (goast.Expr, error) {
	switch e := expr.(type) {
	case *ast.Identifier:
		return goast.NewIdent(e.Name), nil
	case *ast.StringLiteral:
		return &goast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", e.Value),
		}, nil
	case *ast.NumberLiteral:
		if float64(int64(e.Value)) == e.Value {
			return &goast.BasicLit{
				Kind:  token.INT,
				Value: fmt.Sprintf("%d", int64(e.Value)),
			}, nil
		}
		return &goast.BasicLit{
			Kind:  token.FLOAT,
			Value: fmt.Sprintf("%g", e.Value),
		}, nil
	case *ast.BooleanLiteral:
		return goast.NewIdent(fmt.Sprintf("%v", e.Value)), nil
	case *ast.NullLiteral:
		return goast.NewIdent("nil"), nil
	case *ast.JSBinaryExpression:
		return transformBinaryExpression(e)
	case *ast.JSUnaryExpression:
		return transformUnaryExpression(e)
	case *ast.JSCallExpression:
		return transformCallExpression(e)
	case *ast.JSMemberExpression:
		return transformMemberExpression(e)
	case *ast.JSObjectExpression:
		return transformObjectExpression(e)
	case *ast.JSArrayExpression:
		return transformArrayExpression(e)
	case *ast.JSArrowFunction:
		return transformArrowFunction(e)
	case *ast.JSFunction:
		return transformFunctionExpression(e)
	case *ast.JSTemplateLiteral:
		return transformTemplateLiteral(e)
	case *ast.JSAssignmentExpression:
		return transformAssignmentExpression(e)
	case *ast.JSConditionalExpression:
		return transformConditionalExpression(e)
	case *ast.JSNewExpression:
		return transformNewExpression(e)
	case *ast.JSSpreadElement:
		return transformSpreadElement(e)
	default:
		return nil, fmt.Errorf("unsupported expression type: %T", e)
	}
}

func transformVariableDeclaration(decl *ast.JSVariableDeclaration) (goast.Decl, error) {
	var specs []goast.Spec

	for _, d := range decl.Declarations {
		valueExpr, err := transformExpression(d.Init)
		if err != nil {
			return nil, fmt.Errorf("failed to transform variable declaration: %w", err)
		}

		var spec goast.Spec
		switch decl.Kind {
		case "const":
			spec = &goast.ValueSpec{
				Names:  []*goast.Ident{goast.NewIdent(d.ID.Name)},
				Values: []goast.Expr{valueExpr},
				Type:   inferTypeFromExpression(valueExpr),
			}
		case "let", "var":
			spec = &goast.ValueSpec{
				Names:  []*goast.Ident{goast.NewIdent(d.ID.Name)},
				Values: []goast.Expr{valueExpr},
				Type:   inferTypeFromExpression(valueExpr),
			}
		default:
			return nil, fmt.Errorf("unknown variable declaration kind: %s", decl.Kind)
		}

		specs = append(specs, spec)
	}

	var tok token.Token
	if decl.Kind == "const" {
		tok = token.CONST
	} else {
		tok = token.VAR
	}

	return &goast.GenDecl{
		Tok:   tok,
		Specs: specs,
	}, nil
}

func inferTypeFromExpression(expr goast.Expr) goast.Expr {
	switch e := expr.(type) {
	case *goast.BasicLit:
		switch e.Kind {
		case token.INT:
			return goast.NewIdent("int")
		case token.FLOAT:
			return goast.NewIdent("float64")
		case token.STRING:
			return goast.NewIdent("string")
		case token.CHAR:
			return goast.NewIdent("rune")
		default:
			panic(fmt.Sprintf("unhandled basic literal: %v", e.Kind))
		}
	case *goast.CompositeLit:
		if arrayType, ok := e.Type.(*goast.ArrayType); ok {
			return arrayType
		}
		if mapType, ok := e.Type.(*goast.MapType); ok {
			return mapType
		}
	case *goast.FuncLit:
		return e.Type
	case *goast.Ident:
		if e.Name == "true" || e.Name == "false" {
			return goast.NewIdent("bool")
		}
		if e.Name == "nil" {
			return goast.NewIdent("interface{}")
		}
	}
	return goast.NewIdent("interface{}")
}

func transformFunctionDeclaration(fn *ast.JSFunction) (*goast.FuncDecl, error) {
	params := &goast.FieldList{}
	for _, p := range fn.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(p.Name)},
			Type:  goast.NewIdent("interface{}"),
		})
	}

	body, err := transformBlockStatement(fn.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to transform function body: %w", err)
	}

	// Analyze the function body to determine the return type
	returnType := inferReturnType(body)

	return &goast.FuncDecl{
		Name: goast.NewIdent(fn.Name.Name),
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{List: []*goast.Field{{Type: returnType}}},
		},
		Body: body,
	}, nil
}

func inferReturnType(body *goast.BlockStmt) goast.Expr {
	// This is a simplified version. In a real implementation, you'd need to
	// analyze all possible return statements and their types.
	for _, stmt := range body.List {
		if returnStmt, ok := stmt.(*goast.ReturnStmt); ok {
			if len(returnStmt.Results) > 0 {
				return inferTypeFromExpression(returnStmt.Results[0])
			}
		}
	}
	return goast.NewIdent("interface{}")
}

func transformForInOfStatement(stmt *ast.JSForInOfStatement) (goast.Stmt, error) {
	var key, value *goast.Ident
	var rangeExpr goast.Expr
	var err error

	if stmt.Left.(*ast.JSVariableDeclaration).Kind == "var" {
		key = goast.NewIdent(stmt.Left.(*ast.JSVariableDeclaration).Declarations[0].ID.Name)
	} else {
		return nil, fmt.Errorf("unsupported left-hand side in for...in/of loop")
	}

	rangeExpr, err = transformExpression(stmt.Right)
	if err != nil {
		return nil, fmt.Errorf("failed to transform range expression: %w", err)
	}

	body, err := transformBlockStatement(stmt.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to transform loop body: %w", err)
	}

	if stmt.Type == "ForInStatement" {
		// For...in iterates over object properties
		value = goast.NewIdent("_") // We don't use the value in a for...in loop
	} else if stmt.Type == "ForOfStatement" {
		// For...of iterates over iterable values
		value = key
		key = goast.NewIdent("_") // We don't use the key in a for...of loop
	} else {
		return nil, fmt.Errorf("unknown for loop type: %s", stmt.Type)
	}

	return &goast.RangeStmt{
		Key:   key,
		Value: value,
		Tok:   token.DEFINE,
		X:     rangeExpr,
		Body:  body,
	}, nil
}

func transformBlockStatement(block *ast.BlockStatement) (*goast.BlockStmt, error) {
	var stmts []goast.Stmt
	for _, stmt := range block.Statements {
		goStmt, err := transformStatement(stmt)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, goStmt)
	}
	return &goast.BlockStmt{List: stmts}, nil
}

func transformExpressionStatement(stmt *ast.ExpressionStatement) (*goast.ExprStmt, error) {
	expr, err := transformExpression(stmt.Expression)
	if err != nil {
		return nil, err
	}
	return &goast.ExprStmt{X: expr}, nil
}

func transformBinaryExpression(expr *ast.JSBinaryExpression) (goast.Expr, error) {
	left, err := transformExpression(expr.Left)
	if err != nil {
		return nil, err
	}
	right, err := transformExpression(expr.Right)
	if err != nil {
		return nil, err
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

func transformUnaryExpression(expr *ast.JSUnaryExpression) (goast.Expr, error) {
	argument, err := transformExpression(expr.Argument)
	if err != nil {
		return nil, err
	}
	op, err := convertUnaryOperator(expr.Operator)
	if err != nil {
		return nil, err
	}
	return &goast.UnaryExpr{
		Op: op,
		X:  argument,
	}, nil
}

func transformCallExpression(expr *ast.JSCallExpression) (goast.Expr, error) {
	callee, err := transformExpression(expr.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]goast.Expr, len(expr.Arguments))
	for i, arg := range expr.Arguments {
		args[i], err = transformExpression(arg)
		if err != nil {
			return nil, err
		}
	}
	return &goast.CallExpr{
		Fun:  callee,
		Args: args,
	}, nil
}

func transformMemberExpression(expr *ast.JSMemberExpression) (goast.Expr, error) {
	object, err := transformExpression(expr.Object)
	if err != nil {
		return nil, err
	}
	if expr.Computed {
		property, err := transformExpression(expr.Property)
		if err != nil {
			return nil, err
		}
		return &goast.IndexExpr{
			X:     object,
			Index: property,
		}, nil
	}
	propertyIdent, ok := expr.Property.(*ast.Identifier)
	if !ok {
		return nil, fmt.Errorf("non-computed property must be an identifier")
	}
	return &goast.SelectorExpr{
		X:   object,
		Sel: goast.NewIdent(propertyIdent.Name),
	}, nil
}

func transformObjectExpression(expr *ast.JSObjectExpression) (goast.Expr, error) {
	fields := make([]goast.Expr, len(expr.Properties))
	for i, prop := range expr.Properties {
		key, err := transformExpression(prop.Key)
		if err != nil {
			return nil, err
		}
		value, err := transformExpression(prop.Value)
		if err != nil {
			return nil, err
		}
		fields[i] = &goast.KeyValueExpr{
			Key:   key,
			Value: value,
		}
	}
	return &goast.CompositeLit{
		Type: &goast.MapType{
			Key:   goast.NewIdent("string"),
			Value: goast.NewIdent("interface{}"),
		},
		Elts: fields,
	}, nil
}

func transformArrayExpression(expr *ast.JSArrayExpression) (goast.Expr, error) {
	elements := make([]goast.Expr, len(expr.Elements))
	for i, elem := range expr.Elements {
		var err error
		elements[i], err = transformExpression(elem)
		if err != nil {
			return nil, err
		}
	}
	return &goast.CompositeLit{
		Type: &goast.ArrayType{Elt: goast.NewIdent("interface{}")},
		Elts: elements,
	}, nil
}

func transformArrowFunction(expr *ast.JSArrowFunction) (goast.Expr, error) {
	params := &goast.FieldList{}
	for _, param := range expr.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(param.Name)},
			Type:  goast.NewIdent("interface{}"),
		})
	}

	var body goast.Stmt
	if blockStmt, ok := expr.Body.(*ast.BlockStatement); ok {
		var err error
		body, err = transformBlockStatement(blockStmt)
		if err != nil {
			return nil, err
		}
	} else {
		returnExpr, err := transformExpression(expr.Body.(ast.Expression))
		if err != nil {
			return nil, err
		}
		body = &goast.ReturnStmt{Results: []goast.Expr{returnExpr}}
	}

	return &goast.FuncLit{
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
		},
		Body: body.(*goast.BlockStmt),
	}, nil
}

func transformFunctionExpression(expr *ast.JSFunction) (goast.Expr, error) {
	params := &goast.FieldList{}
	for _, param := range expr.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(param.Name)},
			Type:  goast.NewIdent("interface{}"),
		})
	}

	body, err := transformBlockStatement(expr.Body)
	if err != nil {
		return nil, err
	}

	return &goast.FuncLit{
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{List: []*goast.Field{{Type: goast.NewIdent("interface{}")}}},
		},
		Body: body,
	}, nil
}

func transformTemplateLiteral(expr *ast.JSTemplateLiteral) (goast.Expr, error) {
	elements := make([]goast.Expr, 0, len(expr.Quasis)+len(expr.Expressions))
	for i, quasi := range expr.Quasis {
		elements = append(elements, &goast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf("%q", quasi.Value),
		})
		if i < len(expr.Expressions) {
			exprValue, err := transformExpression(expr.Expressions[i])
			if err != nil {
				return nil, err
			}
			elements = append(elements, exprValue)
		}
	}
	return &goast.CallExpr{
		Fun:  goast.NewIdent("fmt.Sprintf"),
		Args: elements,
	}, nil
}

func transformAssignmentExpression(expr *ast.JSAssignmentExpression) (goast.Expr, error) {
	left, err := transformExpression(expr.Left)
	if err != nil {
		return nil, err
	}
	right, err := transformExpression(expr.Right)
	if err != nil {
		return nil, err
	}
	op, err := convertAssignmentToBinaryOperator(expr.Operator)
	if err != nil {
		return nil, err
	}
	return &goast.BinaryExpr{
		X:  left,
		Op: op,
		Y:  right,
	}, nil
}

func convertAssignmentToBinaryOperator(op string) (token.Token, error) {
	switch op {
	case "=":
		return token.ASSIGN, nil
	case "+=":
		return token.ADD, nil
	case "-=":
		return token.SUB, nil
	case "*=":
		return token.MUL, nil
	case "/=":
		return token.QUO, nil
	case "%=":
		return token.REM, nil
	case "<<=":
		return token.SHL, nil
	case ">>=":
		return token.SHR, nil
	case "&=":
		return token.AND, nil
	case "^=":
		return token.XOR, nil
	case "|=":
		return token.OR, nil
	default:
		return token.ILLEGAL, fmt.Errorf("unsupported assignment operator: %s", op)
	}
}

func transformConditionalExpression(expr *ast.JSConditionalExpression) (goast.Expr, error) {
	test, err := transformExpression(expr.Test)
	if err != nil {
		return nil, err
	}
	consequent, err := transformExpression(expr.Consequent)
	if err != nil {
		return nil, err
	}
	alternate, err := transformExpression(expr.Alternate)
	if err != nil {
		return nil, err
	}
	return &goast.CallExpr{
		Fun: goast.NewIdent("func() interface{} {\n\tif "),
		Args: []goast.Expr{
			&goast.BinaryExpr{
				X:  test,
				Op: token.LAND,
				Y: &goast.BinaryExpr{
					X:  consequent,
					Op: token.COLON,
					Y:  alternate,
				},
			},
		},
	}, nil
}

func transformNewExpression(expr *ast.JSNewExpression) (goast.Expr, error) {
	constructor, err := transformExpression(expr.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]goast.Expr, len(expr.Arguments))
	for i, arg := range expr.Arguments {
		args[i], err = transformExpression(arg)
		if err != nil {
			return nil, err
		}
	}
	return &goast.CallExpr{
		Fun:  constructor,
		Args: args,
	}, nil
}

func transformSpreadElement(expr *ast.JSSpreadElement) (goast.Expr, error) {
	arg, err := transformExpression(expr.Argument)
	if err != nil {
		return nil, err
	}
	return &goast.CallExpr{
		Fun:  goast.NewIdent("append"),
		Args: []goast.Expr{goast.NewIdent("..."), arg},
	}, nil
}

func transformIfStatement(stmt *ast.JSIfStatement) (*goast.IfStmt, error) {
	test, err := transformExpression(stmt.Test)
	if err != nil {
		return nil, err
	}

	body, err := transformBlockStatement(stmt.Consequent)
	if err != nil {
		return nil, err
	}

	var elseBody goast.Stmt
	if stmt.Alternate != nil {
		if jsIf, ok := stmt.Alternate.(*ast.JSIfStatement); ok {
			elseIf, err := transformIfStatement(jsIf)
			if err != nil {
				return nil, err
			}
			elseBody = elseIf
		} else {
			elseBlock, err := transformBlockStatement(stmt.Alternate.(*ast.BlockStatement))
			if err != nil {
				return nil, err
			}
			elseBody = elseBlock
		}
	}

	return &goast.IfStmt{
		Cond: test,
		Body: body,
		Else: elseBody,
	}, nil
}

func transformForStatement(stmt *ast.JSForStatement) (*goast.ForStmt, error) {
	var init goast.Stmt
	var cond goast.Expr
	var post goast.Stmt

	if stmt.Init != nil {
		switch initStmt := stmt.Init.(type) {
		case *ast.JSVariableDeclaration:
			var err error
			init, err = transformVariableDeclarationStmt(initStmt)
			if err != nil {
				return nil, err
			}
		case ast.Expression:
			initExpr, err := transformExpression(initStmt)
			if err != nil {
				return nil, err
			}
			init = &goast.ExprStmt{X: initExpr}
		default:
			return nil, fmt.Errorf("unsupported init statement type in for loop: %T", stmt.Init)
		}
	}

	if stmt.Test != nil {
		var err error
		cond, err = transformExpression(stmt.Test)
		if err != nil {
			return nil, err
		}
	}

	if stmt.Update != nil {
		updateExpr, err := transformExpression(stmt.Update)
		if err != nil {
			return nil, err
		}
		post = &goast.ExprStmt{X: updateExpr}
	}

	body, err := transformBlockStatement(stmt.Body)
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

func transformWhileStatement(stmt *ast.JSWhileStatement) (*goast.ForStmt, error) {
	cond, err := transformExpression(stmt.Test)
	if err != nil {
		return nil, err
	}

	body, err := transformBlockStatement(stmt.Body)
	if err != nil {
		return nil, err
	}

	return &goast.ForStmt{
		Cond: cond,
		Body: body,
	}, nil
}

func transformDoWhileStatement(stmt *ast.JSDoWhileStatement) (*goast.BlockStmt, error) {
	// Transform the loop body
	body, err := transformBlockStatement(stmt.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to transform do-while body: %w", err)
	}

	// Transform the condition
	condition, err := transformExpression(stmt.Test)
	if err != nil {
		return nil, fmt.Errorf("failed to transform do-while condition: %w", err)
	}

	// Create a label for the loop
	loopLabel := goast.NewIdent("doWhileLoop")

	// Create the loop structure
	loopStmt := &goast.ForStmt{
		Body: &goast.BlockStmt{
			List: append(body.List,
				&goast.IfStmt{
					Cond: &goast.UnaryExpr{
						Op: token.NOT,
						X:  condition,
					},
					Body: &goast.BlockStmt{
						List: []goast.Stmt{
							&goast.BranchStmt{
								Tok:   token.BREAK,
								Label: loopLabel,
							},
						},
					},
				},
			),
		},
	}

	// Wrap the loop in a labeled statement
	labeledStmt := &goast.LabeledStmt{
		Label: loopLabel,
		Stmt:  loopStmt,
	}

	// Return a block containing the labeled loop statement
	return &goast.BlockStmt{
		List: []goast.Stmt{labeledStmt},
	}, nil
}

func transformReturnStatement(stmt *ast.JSReturnStatement) (*goast.ReturnStmt, error) {
	if stmt.Value == nil {
		return &goast.ReturnStmt{}, nil
	}

	value, err := transformExpression(stmt.Value)
	if err != nil {
		return nil, err
	}

	return &goast.ReturnStmt{
		Results: []goast.Expr{value},
	}, nil
}

func transformSwitchStatement(stmt *ast.JSSwitchStatement) (*goast.SwitchStmt, error) {
	tag, err := transformExpression(stmt.Discriminant)
	if err != nil {
		return nil, err
	}

	cases := make([]goast.Stmt, len(stmt.Cases))
	for i, c := range stmt.Cases {
		caseClause, err := transformSwitchCase(c)
		if err != nil {
			return nil, err
		}
		cases[i] = caseClause
	}

	return &goast.SwitchStmt{
		Tag:  tag,
		Body: &goast.BlockStmt{List: cases},
	}, nil
}

func transformSwitchCase(c *ast.JSSwitchCase) (*goast.CaseClause, error) {
	var list []goast.Expr
	if c.Test != nil {
		expr, err := transformExpression(c.Test)
		if err != nil {
			return nil, err
		}
		list = []goast.Expr{expr}
	}

	body := make([]goast.Stmt, len(c.Consequent))
	for i, stmt := range c.Consequent {
		goStmt, err := transformStatement(stmt)
		if err != nil {
			return nil, err
		}
		body[i] = goStmt
	}

	return &goast.CaseClause{
		List: list,
		Body: body,
	}, nil
}

func transformTryStatement(stmt *ast.JSTryStatement) (*goast.BlockStmt, error) {
	tryBlock, err := transformBlockStatement(stmt.Block)
	if err != nil {
		return nil, fmt.Errorf("failed to transform try block: %w", err)
	}

	var catchBlock *goast.BlockStmt
	if stmt.Handler != nil {
		catchBlock, err = transformCatchClause(stmt.Handler)
		if err != nil {
			return nil, fmt.Errorf("failed to transform catch clause: %w", err)
		}
	}

	var finallyBlock *goast.BlockStmt
	if stmt.Finalizer != nil {
		finallyBlock, err = transformBlockStatement(stmt.Finalizer)
		if err != nil {
			return nil, fmt.Errorf("failed to transform finally block: %w", err)
		}
	}

	// Construct a Go-equivalent try-catch-finally using defer and panic
	stmts := []goast.Stmt{
		&goast.DeferStmt{
			Call: &goast.CallExpr{
				Fun: goast.NewIdent("func"),
				Args: []goast.Expr{
					&goast.FuncLit{
						Type: &goast.FuncType{},
						Body: &goast.BlockStmt{
							List: []goast.Stmt{
								&goast.IfStmt{
									Cond: &goast.BinaryExpr{
										X:  goast.NewIdent("r"),
										Op: token.NEQ,
										Y:  goast.NewIdent("nil"),
									},
									Body: catchBlock,
								},
							},
						},
					},
				},
			},
		},
	}

	if finallyBlock != nil {
		stmts = append(stmts, &goast.DeferStmt{
			Call: &goast.CallExpr{
				Fun: &goast.FuncLit{
					Type: &goast.FuncType{},
					Body: finallyBlock,
				},
			},
		})
	}

	stmts = append(stmts, tryBlock.List...)

	return &goast.BlockStmt{List: stmts}, nil
}

func transformCatchClause(clause *ast.JSCatchClause) (*goast.BlockStmt, error) {
	body, err := transformBlockStatement(clause.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to transform catch body: %w", err)
	}

	// Add a type assertion to convert the recovered value to the expected type
	typeAssert := &goast.AssignStmt{
		Lhs: []goast.Expr{goast.NewIdent(clause.Param.Name)},
		Tok: token.DEFINE,
		Rhs: []goast.Expr{
			&goast.TypeAssertExpr{
				X:    goast.NewIdent("r"),
				Type: goast.NewIdent("error"),
			},
		},
	}

	body.List = append([]goast.Stmt{typeAssert}, body.List...)

	return body, nil
}

func transformClassDeclaration(decl *ast.JSClassDeclaration) ([]goast.Decl, error) {
	structType := &goast.StructType{
		Fields: &goast.FieldList{},
	}

	var decls []goast.Decl

	// Handle superclass if present
	if decl.SuperClass != nil {
		superField := &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(decl.SuperClass.Name)},
			Type:  goast.NewIdent(decl.SuperClass.Name),
		}
		structType.Fields.List = append(structType.Fields.List, superField)
	}

	for _, member := range decl.Body.Body {
		field, funcDecl, err := transformMethodDefinition(decl.ID.Name, member)
		if err != nil {
			return nil, err
		}

		if field != nil {
			structType.Fields.List = append(structType.Fields.List, field)
		}

		if funcDecl != nil {
			decls = append(decls, funcDecl)
		}
	}

	// Create the type specification for the struct
	typeSpec := &goast.TypeSpec{
		Name: goast.NewIdent(decl.ID.Name),
		Type: structType,
	}

	// Add the type declaration to the decls slice
	decls = append([]goast.Decl{
		&goast.GenDecl{
			Tok:   token.TYPE,
			Specs: []goast.Spec{typeSpec},
		},
	}, decls...)

	return decls, nil
}

func transformMethodDefinition(className string, method ast.JSMethodDefinition) (*goast.Field, *goast.FuncDecl, error) {
	switch method.Kind {
	case "method":
		return transformMethod(className, method)
	case "constructor":
		return transformConstructor(className, method)
	case "get", "set":
		return transformAccessor(className, method)
	default:
		return nil, nil, fmt.Errorf("unsupported method kind: %s", method.Kind)
	}
}

func transformMethod(className string, method ast.JSMethodDefinition) (*goast.Field, *goast.FuncDecl, error) {
	funcName := method.Key.(*ast.Identifier).Name
	params, body, err := transformFunctionBody(method.Value)
	if err != nil {
		return nil, nil, err
	}

	receiver := &goast.Field{
		Names: []*goast.Ident{goast.NewIdent("self")},
		Type:  goast.NewIdent(className),
	}

	funcDecl := &goast.FuncDecl{
		Recv: &goast.FieldList{List: []*goast.Field{receiver}},
		Name: goast.NewIdent(funcName),
		Type: &goast.FuncType{
			Params:  params,
			Results: &goast.FieldList{}, // Adjust if you want to infer return types
		},
		Body: body,
	}

	return nil, funcDecl, nil
}

func transformConstructor(className string, method ast.JSMethodDefinition) (*goast.Field, *goast.FuncDecl, error) {
	params, body, err := transformFunctionBody(method.Value)
	if err != nil {
		return nil, nil, err
	}

	funcDecl := &goast.FuncDecl{
		Name: goast.NewIdent("New" + className),
		Type: &goast.FuncType{
			Params: params,
			Results: &goast.FieldList{
				List: []*goast.Field{
					{Type: &goast.StarExpr{X: goast.NewIdent(className)}},
				},
			},
		},
		Body: body,
	}

	// Add a return statement at the end of the constructor
	returnStmt := &goast.ReturnStmt{
		Results: []goast.Expr{
			&goast.UnaryExpr{
				Op: token.AND,
				X:  goast.NewIdent(className + "{}"),
			},
		},
	}
	funcDecl.Body.List = append(funcDecl.Body.List, returnStmt)

	return nil, funcDecl, nil
}

func transformAccessor(className string, method ast.JSMethodDefinition) (*goast.Field, *goast.FuncDecl, error) {
	// For simplicity, we'll transform getters and setters to regular methods
	// You might want to adjust this based on your specific needs
	return transformMethod(className, method)
}

func transformFunctionBody(fn *ast.JSFunction) (*goast.FieldList, *goast.BlockStmt, error) {
	params := &goast.FieldList{}
	for _, param := range fn.Parameters {
		params.List = append(params.List, &goast.Field{
			Names: []*goast.Ident{goast.NewIdent(param.Name)},
			Type:  goast.NewIdent("interface{}"), // You might want to infer types if possible
		})
	}

	body, err := transformBlockStatement(fn.Body)
	if err != nil {
		return nil, nil, err
	}

	return params, body, nil
}

func transformVariableDeclarationStmt(decl *ast.JSVariableDeclaration) (goast.Stmt, error) {
	genDecl, err := transformVariableDeclaration(decl)
	if err != nil {
		return nil, err
	}
	return &goast.DeclStmt{Decl: genDecl}, nil
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
	default:
		return token.ILLEGAL, fmt.Errorf("unsupported binary operator: %s", op)
	}
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
