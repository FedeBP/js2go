package main

import (
	"fmt"
	"log"
	"os"
	"reflect"

	"github.com/FedeBP/js2go/internal/generator"
	"github.com/FedeBP/js2go/internal/parser"
	"github.com/FedeBP/js2go/internal/transformer"
)

func main() {
	jsCode, err := os.ReadFile("examples/simple.js")
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}

	p := parser.New(parser.NewLexer(string(jsCode)))
	jsAst := p.ParseProgram()

	if len(p.Errors()) > 0 {
		log.Fatalf("Parsing errors: %v", p.Errors())
	}

	//fmt.Println("JavaScript AST:")
	//debugPrintAST("Program", jsAst, "")

	trans := transformer.NewTransformer()

	goAst, err := trans.Transform(jsAst)
	if err != nil {
		log.Fatalf("Error transforming AST: %v", err)
	}

	//fmt.Println("\nGo AST:")
	//debugPrintAST("File", goAst, "")

	gen := generator.NewGenerator()

	goCode, err := gen.GenerateGoCode(goAst)
	if err != nil {
		log.Fatalf("Error generating Go code: %v", err)
	}

	fmt.Println("\nGenerated Go Code:")
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
