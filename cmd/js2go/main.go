package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"reflect"

	"github.com/FedeBP/js2go/internal/generator"
	"github.com/FedeBP/js2go/internal/parser"
	"github.com/FedeBP/js2go/internal/transformer"
)

func main() {
	// Hardcode the input file path for debugging
	inputFile := "examples/simple.js"

	// Get the absolute path
	absInputFile, err := filepath.Abs(inputFile)
	if err != nil {
		log.Fatalf("Error getting absolute path: %v", err)
	}

	// Read JavaScript file
	jsCode, err := os.ReadFile(absInputFile)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}

	// Parse JavaScript to AST
	jsAST, err := parser.ParseJavaScript(string(jsCode))
	if err != nil {
		log.Fatalf("Error parsing JavaScript: %v", err)
	}
	fmt.Println("=== JavaScript AST ===")
	for i, node := range jsAST {
		debugPrintAST(fmt.Sprintf("Node[%d]", i), node, "")
	}

	// Transform JavaScript AST to Go AST
	goAST, err := transformer.TransformAST(jsAST)
	if err != nil {
		log.Fatalf("Error transforming AST: %v", err)
	}
	fmt.Println("\n=== Go AST ===")
	for i, node := range goAST {
		debugPrintAST(fmt.Sprintf("Node[%d]", i), node, "")
	}

	// Generate Go code from Go AST
	goCode, err := generator.GenerateGoCode(goAST)
	if err != nil {
		log.Fatalf("Error generating Go code: %v", err)
	}

	// Print the generated Go code
	fmt.Println(goCode)
}

func debugPrintAST(name string, node interface{}, indent string) {
	if node == nil {
		fmt.Printf("%s%s: nil\n", indent, name)
		return
	}

	value := reflect.ValueOf(node)
	if value.Kind() == reflect.Ptr {
		value = value.Elem()
	}

	fmt.Printf("%s%s: %s {\n", indent, name, value.Type())

	for i := 0; i < value.NumField(); i++ {
		field := value.Field(i)
		fieldType := value.Type().Field(i)

		if field.Kind() == reflect.Slice {
			fmt.Printf("%s  %s: [\n", indent, fieldType.Name)
			for j := 0; j < field.Len(); j++ {
				debugPrintAST(fmt.Sprintf("[%d]", j), field.Index(j).Interface(), indent+"    ")
			}
			fmt.Printf("%s  ]\n", indent)
		} else if field.Kind() == reflect.Struct || (field.Kind() == reflect.Ptr && field.Elem().Kind() == reflect.Struct) {
			debugPrintAST(fieldType.Name, field.Interface(), indent+"  ")
		} else {
			fmt.Printf("%s  %s: %v\n", indent, fieldType.Name, field.Interface())
		}
	}

	fmt.Printf("%s}\n", indent)
}
