package parser_test

import (
	"github.com/FedeBP/js2go/pkg"
	"testing"

	"github.com/FedeBP/js2go/internal/parser"
	"github.com/FedeBP/js2go/pkg/ast"
)

func TestParseJavaScript(t *testing.T) {
	tests := []struct {
		name  string
		input string
		check func(t *testing.T, nodes []ast.JSNode)
	}{
		{
			name:  "Variable declaration",
			input: "var x = 5;",
			check: func(t *testing.T, nodes []ast.JSNode) {
				if len(nodes) != 1 {
					t.Fatalf("Expected 1 node, got %d", len(nodes))
				}
				decl, ok := nodes[0].(*ast.JSVariableDeclaration)
				if !ok {
					t.Fatalf("Expected JSVariableDeclaration, got %T", nodes[0])
				}
				if decl.Kind != "var" {
					t.Errorf("Expected kind 'var', got '%s'", decl.Kind)
				}
				if len(decl.Declarations) != 1 {
					t.Fatalf("Expected 1 declaration, got %d", len(decl.Declarations))
				}
				if decl.Declarations[0].ID.Name != "x" {
					t.Errorf("Expected variable name 'x', got '%s'", decl.Declarations[0].ID.Name)
				}
				lit, ok := decl.Declarations[0].Init.(*pkg.Literal)
				if !ok {
					t.Fatalf("Expected Literal, got %T", decl.Declarations[0].Init)
				}
				if lit.Raw != "5" {
					t.Errorf("Expected value '5', got '%s'", lit.Raw)
				}
			},
		},
		{
			name:  "Function declaration",
			input: "function greet(name) { return 'Hello, ' + name; }",
			check: func(t *testing.T, nodes []ast.JSNode) {
				if len(nodes) != 1 {
					t.Fatalf("Expected 1 node, got %d", len(nodes))
				}
				fn, ok := nodes[0].(*ast.JSFunction)
				if !ok {
					t.Fatalf("Expected JSFunction, got %T", nodes[0])
				}
				if fn.Name.Name != "greet" {
					t.Errorf("Expected function name 'greet', got '%s'", fn.Name.Name)
				}
				if len(fn.Parameters) != 1 || fn.Parameters[0].Name != "name" {
					t.Errorf("Expected one parameter 'name', got %v", fn.Parameters)
				}
				if len(fn.Body) != 1 {
					t.Fatalf("Expected 1 statement in function body, got %d", len(fn.Body))
				}
				ret, ok := fn.Body[0].(*ast.JSReturnStatement)
				if !ok {
					t.Fatalf("Expected JSReturnStatement, got %T", fn.Body[0])
				}
				bin, ok := ret.Argument.(*ast.JSBinaryExpression)
				if !ok {
					t.Fatalf("Expected JSBinaryExpression, got %T", ret.Argument)
				}
				if bin.Operator != "+" {
					t.Errorf("Expected '+' operator, got '%s'", bin.Operator)
				}
				left, ok := bin.Left.(*pkg.Literal)
				if !ok {
					t.Fatalf("Expected Literal, got %T", bin.Left)
				}
				if left.Raw != "'Hello, '" {
					t.Errorf("Expected 'Hello, ', got '%s'", left.Raw)
				}
				right, ok := bin.Right.(*pkg.Identifier)
				if !ok {
					t.Fatalf("Expected Identifier, got %T", bin.Right)
				}
				if right.Name != "name" {
					t.Errorf("Expected 'name', got '%s'", right.Name)
				}
				if bin.Operator != "+" {
					t.Errorf("Expected '+' operator, got '%s'", bin.Operator)
				}
				if left.Raw != "'Hello, '" {
					t.Errorf("Expected 'Hello, ', got '%s'", left.Raw)
				}
				if right.Name != "name" {
					t.Errorf("Expected 'name', got '%s'", right.Name)
				}
			},
		},
		{
			name:  "Multiple statements",
			input: "let a = 1; const b = 2; var c = a + b;",
			check: func(t *testing.T, nodes []ast.JSNode) {
				if len(nodes) != 3 {
					t.Fatalf("Expected 3 nodes, got %d", len(nodes))
				}
				checkVarDecl(t, nodes[0], "let", "a", "1")
				checkVarDecl(t, nodes[1], "const", "b", "2")
				checkVarDecl(t, nodes[2], "var", "c", "")
			},
		},
		{
			name:  "Unary operations",
			input: "let x = -5; let y = !true;",
			check: func(t *testing.T, nodes []ast.JSNode) {
				if len(nodes) != 2 {
					t.Fatalf("Expected 2 nodes, got %d", len(nodes))
				}

				// Check first declaration: let x = -5;
				decl1, ok := nodes[0].(*ast.JSVariableDeclaration)
				if !ok {
					t.Fatalf("Expected JSVariableDeclaration, got %T", nodes[0])
				}
				if decl1.Kind != "let" || len(decl1.Declarations) != 1 {
					t.Fatalf("Unexpected structure in first declaration")
				}
				unary1, ok := decl1.Declarations[0].Init.(*ast.JSUnaryExpression)
				if !ok {
					t.Fatalf("Expected JSUnaryExpression, got %T", decl1.Declarations[0].Init)
				}
				if unary1.Operator != "-" {
					t.Errorf("Expected operator '-', got '%s'", unary1.Operator)
				}
				lit1, ok := unary1.Argument.(*pkg.Literal)
				if !ok || lit1.Raw != "5" {
					t.Errorf("Expected literal '5', got %v", unary1.Argument)
				}

				// Check second declaration: let y = !true;
				decl2, ok := nodes[1].(*ast.JSVariableDeclaration)
				if !ok {
					t.Fatalf("Expected JSVariableDeclaration, got %T", nodes[1])
				}
				if decl2.Kind != "let" || len(decl2.Declarations) != 1 {
					t.Fatalf("Unexpected structure in second declaration")
				}
				unary2, ok := decl2.Declarations[0].Init.(*ast.JSUnaryExpression)
				if !ok {
					t.Fatalf("Expected JSUnaryExpression, got %T", decl2.Declarations[0].Init)
				}
				if unary2.Operator != "!" {
					t.Errorf("Expected operator '!', got '%s'", unary2.Operator)
				}
				lit2, ok := unary2.Argument.(*pkg.Identifier)
				if !ok || lit2.Name != "true" {
					t.Errorf("Expected identifier 'true', got %v", unary2.Argument)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			newParser := parser.NewParser(tt.input)
			result, err := newParser.Parse()
			if err != nil {
				t.Fatalf("ParseJavaScript() error = %v", err)
			}
			tt.check(t, result)
		})
	}
}

func checkVarDecl(t *testing.T, node ast.JSNode, expectedKind, expectedName, expectedValue string) {
	decl, ok := node.(*ast.JSVariableDeclaration)
	if !ok {
		t.Fatalf("Expected JSVariableDeclaration, got %T", node)
	}
	if decl.Kind != expectedKind {
		t.Errorf("Expected kind '%s', got '%s'", expectedKind, decl.Kind)
	}
	if len(decl.Declarations) != 1 {
		t.Fatalf("Expected 1 declaration, got %d", len(decl.Declarations))
	}
	if decl.Declarations[0].ID.Name != expectedName {
		t.Errorf("Expected variable name '%s', got '%s'", expectedName, decl.Declarations[0].ID.Name)
	}
	if expectedValue != "" {
		lit, ok := decl.Declarations[0].Init.(*pkg.Literal)
		if !ok {
			t.Fatalf("Expected Literal, got %T", decl.Declarations[0].Init)
		}
		if lit.Raw != expectedValue {
			t.Errorf("Expected value '%s', got '%s'", expectedValue, lit.Raw)
		}
	}
}
