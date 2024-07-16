package main

import (
	"fmt"
	"github.com/FedeBP/js2go/internal/generator"
	"github.com/FedeBP/js2go/internal/parser"
	"log"
	"os"
	"reflect"
)

func main() {
	// Read the simple.js file
	jsCode, err := os.ReadFile("examples/simple.js")
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}

	// Parse the JavaScript code
	p := parser.New(parser.NewLexer(string(jsCode)))
	jsAst := p.ParseProgram()

	// Create a new generator
	newGenerator := generator.NewGenerator()

	// Generate Go AST
	goAst, err := newGenerator.GenerateGoAST(jsAst)
	if err != nil {
		log.Fatalf("Error generating Go AST: %v", err)
	}

	// Generate Go code
	goCode, err := newGenerator.GenerateGoCode(goAst)
	if err != nil {
		log.Fatalf("Error generating Go code: %v", err)
	}

	// Print the generated Go code
	fmt.Println("Generated Go code:")
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
