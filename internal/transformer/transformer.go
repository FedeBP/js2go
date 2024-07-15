package transformer

import (
	"fmt"
	"github.com/FedeBP/js2go/pkg"
)

func TransformAST(jsAST []ast.JSNode) ([]ast.GoNode, error) {
	var goAST []ast.GoNode
	varDeclarations := make(map[string]ast.GoNode)

	for _, node := range jsAST {
		goNode, err := transformNode(node)
		if err != nil {
			return nil, err
		}
		if goNode != nil {
			if varDecl, ok := goNode.(*ast.GoVariableDeclaration); ok {
				for i, name := range varDecl.Names {
					varDeclarations[name.Name] = varDecl.Values[i]
				}
			}
			goAST = append(goAST, goNode)
		}
	}

	for i, node := range goAST {
		if exprStmt, ok := node.(*ast.GoExpressionStatement); ok {
			if callExpr, ok := exprStmt.Expression.(*ast.GoCallExpression); ok {
				if ident, ok := callExpr.Function.(*pkg.Identifier); ok && ident.Name == "console.log" {
					if len(callExpr.Arguments) > 0 {
						goAST[i] = &ast.GoCallExpression{
							Function:  &pkg.Identifier{Name: "fmt.Println"},
							Arguments: callExpr.Arguments,
						}
					} else if len(varDeclarations) > 0 {
						var lastVar ast.GoNode
						for _, v := range varDeclarations {
							lastVar = v
						}
						goAST[i] = &ast.GoCallExpression{
							Function:  &pkg.Identifier{Name: "fmt.Println"},
							Arguments: []ast.GoNode{lastVar},
						}
					}
				}
			} else if ident, ok := exprStmt.Expression.(*pkg.Identifier); ok {
				if value, exists := varDeclarations[ident.Name]; exists {
					goAST[i] = &ast.GoCallExpression{
						Function:  &pkg.Identifier{Name: "fmt.Println"},
						Arguments: []ast.GoNode{value},
					}
				}
			}
		}
	}

	return goAST, nil
}

func transformNode(node ast.JSNode) (ast.GoNode, error) {
	switch n := node.(type) {
	case *ast.JSFunction:
		return transformFunction(n)
	case *ast.JSVariableDeclaration:
		return transformVariableDeclaration(n)
	case *ast.JSReturnStatement:
		return transformReturnStatement(n)
	case *ast.JSBinaryExpression:
		return transformBinaryExpression(n)
	case *ast.JSCallExpression:
		callee, err := transformNode(n.Callee)
		if err != nil {
			return nil, err
		}
		args := make([]ast.GoNode, len(n.Arguments))
		for i, arg := range n.Arguments {
			args[i], err = transformNode(arg)
			if err != nil {
				return nil, err
			}
		}
		if memberExpr, ok := n.Callee.(*ast.JSMemberExpression); ok {
			if ident, ok := memberExpr.Object.(*pkg.Identifier); ok && ident.Name == "console" {
				if memberExpr.Property.Name == "log" {
					return &ast.GoCallExpression{
						Function:  &pkg.Identifier{Name: "fmt.Println"},
						Arguments: args,
					}, nil
				}
			}
		}
		return &ast.GoCallExpression{
			Function:  callee,
			Arguments: args,
		}, nil
	case *pkg.ExpressionStatement:
		return transformExpressionStatement(n)
	case *ast.JSMemberExpression:
		if ident, ok := n.Object.(*pkg.Identifier); ok && ident.Name == "console" {
			if n.Property.Name == "log" {
				return &pkg.Identifier{Name: "console.log"}, nil
			}
		}
		return &ast.GoSelectorExpr{
			X:   &pkg.Identifier{Name: n.Object.(*pkg.Identifier).Name},
			Sel: n.Property,
		}, nil
	case *pkg.Identifier:
		return n, nil
	case *pkg.Literal:
		return n, nil
	default:
		return nil, fmt.Errorf("unsupported node type: %T", node)
	}
}

func transformFunction(fn *ast.JSFunction) (*ast.GoFunction, error) {
	params := make([]*ast.GoParameter, len(fn.Parameters))
	for i, param := range fn.Parameters {
		params[i] = &ast.GoParameter{Name: param, Type: &pkg.Identifier{Name: "interface{}"}}
	}

	body := make([]ast.GoNode, 0, len(fn.Body))
	for _, stmt := range fn.Body {
		goStmt, err := transformNode(stmt)
		if err != nil {
			return nil, err
		}
		body = append(body, goStmt)
	}

	return &ast.GoFunction{
		Name:       fn.Name,
		Parameters: params,
		ReturnType: &pkg.Identifier{Name: "string"},
		Body:       body,
	}, nil
}

func transformVariableDeclaration(decl *ast.JSVariableDeclaration) (*ast.GoVariableDeclaration, error) {
	names := make([]*pkg.Identifier, len(decl.Declarations))
	values := make([]ast.GoNode, len(decl.Declarations))

	for i, d := range decl.Declarations {
		names[i] = d.ID
		if d.Init != nil {
			value, err := transformNode(d.Init)
			if err != nil {
				return nil, err
			}
			values[i] = value
		}
	}

	return &ast.GoVariableDeclaration{
		Names:  names,
		Type:   nil,
		Values: values,
	}, nil
}

func transformReturnStatement(stmt *ast.JSReturnStatement) (*ast.GoReturnStatement, error) {
	var results []ast.GoNode
	if stmt.Argument != nil {
		arg, err := transformNode(stmt.Argument)
		if err != nil {
			return nil, err
		}
		results = []ast.GoNode{arg}
	}

	return &ast.GoReturnStatement{Results: results}, nil
}

func transformBinaryExpression(expr *ast.JSBinaryExpression) (ast.GoNode, error) {
	left, err := transformNode(expr.Left)
	if err != nil {
		return nil, err
	}
	right, err := transformNode(expr.Right)
	if err != nil {
		return nil, err
	}

	if expr.Operator == "+" {
		return &ast.GoCallExpression{
			Function: &pkg.Identifier{Name: "fmt.Sprintf"},
			Arguments: []ast.GoNode{
				&pkg.Literal{Raw: "\"%s%s\""},
				left,
				right,
			},
		}, nil
	}

	return &ast.GoBinaryExpression{
		Left:     left,
		Operator: expr.Operator,
		Right:    right,
	}, nil
}

func transformExpressionStatement(stmt *pkg.ExpressionStatement) (ast.GoNode, error) {
	expr, err := transformNode(stmt.Expression)
	if err != nil {
		return nil, err
	}

	if callExpr, ok := expr.(*ast.GoCallExpression); ok {
		if ident, ok := callExpr.Function.(*pkg.Identifier); ok && ident.Name == "console.log" {
			return &ast.GoCallExpression{
				Function:  &pkg.Identifier{Name: "fmt.Println"},
				Arguments: callExpr.Arguments,
			}, nil
		}
	}

	return &ast.GoExpressionStatement{Expression: expr}, nil
}

func secondPass(nodes []ast.GoNode) []ast.GoNode {
	var result []ast.GoNode
	var pendingArg ast.GoNode

	for _, node := range nodes {
		switch n := node.(type) {
		case *ast.GoCallExpression:
			if pendingArg != nil {
				n.Arguments = append(n.Arguments, pendingArg)
				pendingArg = nil
			}
			result = append(result, n)
		case *ast.GoVariableDeclaration:
			if len(n.Values) > 0 {
				if callExpr, ok := n.Values[0].(*ast.GoCallExpression); ok && pendingArg != nil {
					callExpr.Arguments = append(callExpr.Arguments, pendingArg)
					pendingArg = nil
				}
			}
			result = append(result, n)
		case nil:
			pendingArg = node
		default:
			result = append(result, node)
		}
	}

	return result
}
