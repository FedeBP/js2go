package generator

import (
	"fmt"
	"github.com/FedeBP/js2go/pkg"
	"strings"

	"github.com/FedeBP/js2go/pkg/ast"
)

func GenerateGoCode(goAST []ast.GoNode) (string, error) {
	var output strings.Builder

	output.WriteString("package main\n\n")
	output.WriteString("import \"fmt\"\n\n")

	var mainFunctionBody strings.Builder

	for _, node := range goAST {
		code, err := generateNode(node)
		if err != nil {
			return "", err
		}

		if _, isFunction := node.(*ast.GoFunction); isFunction {
			output.WriteString(code)
			output.WriteString("\n\n")
		} else {
			mainFunctionBody.WriteString("\t" + code + "\n")
		}
	}

	output.WriteString("func main() {\n")
	output.WriteString(mainFunctionBody.String())
	output.WriteString("}\n")

	return output.String(), nil
}

func generateNode(node ast.GoNode) (string, error) {
	switch n := node.(type) {
	case *ast.GoFunction:
		return generateFunction(n)
	case *ast.GoVariableDeclaration:
		return generateVariableDeclaration(n)
	case *ast.GoReturnStatement:
		return generateReturnStatement(n)
	case *ast.GoCallExpression:
		return generateCallExpression(n)
	case *ast.GoExpressionStatement:
		return generateExpressionStatement(n)
	case *ast.GoBinaryExpression:
		return generateBinaryExpression(n)
	case *pkg.Literal:
		return generateLiteral(n), nil
	case *pkg.Identifier:
		return n.Name, nil
	default:
		return "", fmt.Errorf("unsupported node type: %T", node)
	}
}

func generateFunction(fn *ast.GoFunction) (string, error) {
	var output strings.Builder
	output.WriteString(fmt.Sprintf("func %s(", fn.Name.Name))

	params := make([]string, len(fn.Parameters))
	for i, param := range fn.Parameters {
		params[i] = fmt.Sprintf("%s %s", param.Name.Name, generateType(param.Type))
	}
	output.WriteString(strings.Join(params, ", "))
	output.WriteString(fmt.Sprintf(") %s {\n", generateType(fn.ReturnType)))

	for _, stmt := range fn.Body {
		code, err := generateNode(stmt)
		if err != nil {
			return "", err
		}
		output.WriteString("\t" + code + "\n")
	}

	output.WriteString("}")
	return output.String(), nil
}

func generateVariableDeclaration(decl *ast.GoVariableDeclaration) (string, error) {
	var output strings.Builder
	output.WriteString("var ")

	names := make([]string, len(decl.Names))
	for i, name := range decl.Names {
		names[i] = name.Name
	}
	output.WriteString(strings.Join(names, ", "))

	if len(decl.Values) > 0 {
		output.WriteString(" = ")
		values := make([]string, len(decl.Values))
		for i, value := range decl.Values {
			code, err := generateNode(value)
			if err != nil {
				return "", err
			}
			values[i] = code
		}
		output.WriteString(strings.Join(values, ", "))
	}

	return output.String(), nil
}

func generateReturnStatement(stmt *ast.GoReturnStatement) (string, error) {
	var output strings.Builder
	output.WriteString("return ")

	results := make([]string, len(stmt.Results))
	for i, result := range stmt.Results {
		code, err := generateNode(result)
		if err != nil {
			return "", err
		}
		results[i] = code
	}
	output.WriteString(strings.Join(results, ", "))

	return output.String(), nil
}

func generateCallExpression(expr *ast.GoCallExpression) (string, error) {
	function, err := generateNode(expr.Function)
	if err != nil {
		return "", err
	}

	args := make([]string, len(expr.Arguments))
	for i, arg := range expr.Arguments {
		argStr, err := generateNode(arg)
		if err != nil {
			return "", err
		}
		args[i] = argStr
	}

	return fmt.Sprintf("%s(%s)", function, strings.Join(args, ", ")), nil
}

func generateExpressionStatement(stmt *ast.GoExpressionStatement) (string, error) {
	return generateNode(stmt.Expression)
}

func generateBinaryExpression(expr *ast.GoBinaryExpression) (string, error) {
	left, err := generateNode(expr.Left)
	if err != nil {
		return "", err
	}
	right, err := generateNode(expr.Right)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%s %s %s", left, expr.Operator, right), nil
}

func generateLiteral(lit *pkg.Literal) string {
	return lit.Raw
}

func generateType(node ast.GoNode) string {
	if id, ok := node.(*pkg.Identifier); ok {
		return id.Name
	}
	return "interface{}"
}
