package transformer_test

import (
	"github.com/FedeBP/js2go/pkg"
	"reflect"
	"testing"

	"github.com/FedeBP/js2go/internal/transformer"
	"github.com/FedeBP/js2go/pkg/ast"
)

func TestTransformAST(t *testing.T) {
	tests := []struct {
		name     string
		jsAST    []ast.JSNode
		expected []ast.GoNode
	}{
		{
			name: "Function Declaration",
			jsAST: []ast.JSNode{
				&ast.JSFunction{
					Name: &pkg.Identifier{Name: "greet"},
					Parameters: []*pkg.Identifier{
						{Name: "name"},
					},
					Body: []ast.JSNode{
						&ast.JSReturnStatement{
							Argument: &ast.JSBinaryExpression{
								Left:     &pkg.Literal{Raw: "'Hello, '"},
								Operator: "+",
								Right:    &pkg.Identifier{Name: "name"},
							},
						},
					},
				},
			},
			expected: []ast.GoNode{
				&ast.GoFunction{
					Name: &pkg.Identifier{Name: "greet"},
					Parameters: []*ast.GoParameter{
						{Name: &pkg.Identifier{Name: "name"}, Type: &pkg.Identifier{Name: "interface{}"}},
					},
					ReturnType: &pkg.Identifier{Name: "interface{}"},
					Body: []ast.GoNode{
						&ast.GoReturnStatement{
							Results: []ast.GoNode{
								&ast.GoCallExpression{
									Function: &pkg.Identifier{Name: "+"},
									Arguments: []ast.GoNode{
										&pkg.Literal{Raw: "'Hello, '"},
										&pkg.Identifier{Name: "name"},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "Variable Declaration",
			jsAST: []ast.JSNode{
				&ast.JSVariableDeclaration{
					Kind: "let",
					Declarations: []*ast.JSVariableDeclarator{
						{
							ID:   &pkg.Identifier{Name: "x"},
							Init: &pkg.Literal{Raw: "5"},
						},
					},
				},
			},
			expected: []ast.GoNode{
				&ast.GoVariableDeclaration{
					Names:  []*pkg.Identifier{{Name: "x"}},
					Type:   &pkg.Identifier{Name: "interface{}"},
					Values: []ast.GoNode{&pkg.Literal{Raw: "5"}},
				},
			},
		},
		{
			name: "If Statement",
			jsAST: []ast.JSNode{
				&ast.JSIfStatement{
					Test: &ast.JSBinaryExpression{
						Left:     &pkg.Identifier{Name: "x"},
						Operator: ">",
						Right:    &pkg.Literal{Raw: "0"},
					},
					Consequent: []ast.JSNode{
						&ast.JSReturnStatement{
							Argument: &pkg.Literal{Raw: "true"},
						},
					},
					Alternate: []ast.JSNode{
						&ast.JSReturnStatement{
							Argument: &pkg.Literal{Raw: "false"},
						},
					},
				},
			},
			expected: []ast.GoNode{
				&ast.GoIfStatement{
					Condition: &ast.GoCallExpression{
						Function: &pkg.Identifier{Name: ">"},
						Arguments: []ast.GoNode{
							&pkg.Identifier{Name: "x"},
							&pkg.Literal{Raw: "0"},
						},
					},
					Body: []ast.GoNode{
						&ast.GoReturnStatement{
							Results: []ast.GoNode{&pkg.Literal{Raw: "true"}},
						},
					},
					Else: []ast.GoNode{
						&ast.GoReturnStatement{
							Results: []ast.GoNode{&pkg.Literal{Raw: "false"}},
						},
					},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := transformer.TransformAST(tt.jsAST)
			if err != nil {
				t.Fatalf("TransformAST() error = %v", err)
			}
			if !reflect.DeepEqual(result, tt.expected) {
				t.Errorf("TransformAST() = %v, want %v", result, tt.expected)
			}
		})
	}
}
