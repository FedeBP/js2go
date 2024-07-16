package generator

import (
	"bytes"
	"go/printer"
	"go/token"

	goast "go/ast"
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

// GenerateGoCode generates Go code from a Go AST
func (g *Generator) GenerateGoCode(file *goast.File) (string, error) {
	var buf bytes.Buffer

	err := printer.Fprint(&buf, g.fset, file)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}
