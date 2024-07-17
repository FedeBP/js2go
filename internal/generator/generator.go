package generator

import (
	"bytes"
	"go/printer"
	"go/token"
	"strings"

	"go/ast"
)

type Generator struct {
	fset *token.FileSet
}

func NewGenerator() *Generator {
	return &Generator{
		fset: token.NewFileSet(),
	}
}

func (g *Generator) GenerateGoCode(file *ast.File) (string, error) {
	var buf bytes.Buffer

	err := printer.Fprint(&buf, g.fset, file)
	if err != nil {
		return "", err
	}

	// Remove the package declaration
	code := buf.String()
	lines := strings.Split(code, "\n")
	if len(lines) > 0 && strings.HasPrefix(lines[0], "package ") {
		code = strings.Join(lines[1:], "\n")
	}

	return strings.TrimSpace(code), nil
}
