package generator_test

import (
	"github.com/FedeBP/js2go/pkg"
	"testing"

	"github.com/FedeBP/js2go/internal/generator"
	"github.com/FedeBP/js2go/pkg/ast"
)

func TestGenerateGoCode(t *testing.T) {
	tests := []struct {
		name     string
		goAST    []ast.GoNode
		expected string
	}{
		{
			name: "Function Declaration",
			goAST: []ast.GoNode{
				&ast.GoFunction{
					Name: &pkg.Identifier{Name: "greet"},
					Parameters: []*ast.GoParameter{
						{Name: &pkg.Identifier{Name: "name"}, Type: &pkg.Identifier{Name: "string"}},
					},
					ReturnType: &pkg.Identifier{Name: "string"},
					Body: []ast.GoNode{
						&ast.GoReturnStatement{
							Results: []ast.GoNode{
								&ast.GoCallExpression{
									Function: &pkg.Identifier{Name: "+"},
									Arguments: []ast.GoNode{
										&pkg.Literal{Raw: "\"Hello, \""},
										&pkg.Identifier{Name: "name"},
									},
								},
							},
						},
					},
				},
			},
			expected: `func greet(name string) string {
	return "Hello, " + name
}
`,
		},
		{
			name: "Variable Declaration",
			goAST: []ast.GoNode{
				&ast.GoVariableDeclaration{
					Names:  []*pkg.Identifier{{Name: "x"}},
					Type:   &pkg.Identifier{Name: "int"},
					Values: []ast.GoNode{&pkg.Literal{Raw: "5"}},
				},
			},
			expected: "var x int = 5\n",
		},
		{
			name: "If Statement",
			goAST: []ast.GoNode{
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
			expected: `if x > 0 {
	return true
} else {
	return false
}
`,
		},
		{
			name: "For Statement",
			goAST: []ast.GoNode{
				&ast.GoForStatement{
					Init: &ast.GoVariableDeclaration{
						Names:  []*pkg.Identifier{{Name: "i"}},
						Type:   &pkg.Identifier{Name: "int"},
						Values: []ast.GoNode{&pkg.Literal{Raw: "0"}},
					},
					Condition: &ast.GoCallExpression{
						Function: &pkg.Identifier{Name: "<"},
						Arguments: []ast.GoNode{
							&pkg.Identifier{Name: "i"},
							&pkg.Literal{Raw: "10"},
						},
					},
					Post: &ast.GoCallExpression{
						Function: &pkg.Identifier{Name: "++"},
						Arguments: []ast.GoNode{
							&pkg.Identifier{Name: "i"},
						},
					},
					Body: []ast.GoNode{
						&ast.GoCallExpression{
							Function: &pkg.Identifier{Name: "println"},
							Arguments: []ast.GoNode{
								&pkg.Identifier{Name: "i"},
							},
						},
					},
				},
			},
			expected: `for var i int = 0; i < 10; i++ {
	println(i)
}
`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := generator.GenerateGoCode(tt.goAST)
			if err != nil {
				t.Fatalf("GenerateGoCode() error = %v", err)
			}
			if result != tt.expected {
				t.Errorf("GenerateGoCode() = %v, want %v", result, tt.expected)
			}
		})
	}
}
