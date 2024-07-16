package ast

import (
	"fmt"
)

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// JavaScript-specific structures

type JSFunction struct {
	Name       *Identifier
	Parameters []*Identifier
	Body       *BlockStatement
}

func (f *JSFunction) expressionNode()      {}
func (f *JSFunction) statementNode()       {}
func (f *JSFunction) TokenLiteral() string { return "function" }

type JSArrowFunction struct {
	Parameters []*Identifier
	Body       Node // Can be BlockStatement or Expression
}

func (af *JSArrowFunction) expressionNode()      {}
func (af *JSArrowFunction) TokenLiteral() string { return "=>" }

type JSVariableDeclaration struct {
	Kind         string // "var", "let", or "const"
	Declarations []*JSVariableDeclarator
}

func (vd *JSVariableDeclaration) statementNode()       {}
func (vd *JSVariableDeclaration) TokenLiteral() string { return vd.Kind }

type JSVariableDeclarator struct {
	ID   *Identifier
	Init Expression
}

type JSAssignmentExpression struct {
	Left     Expression
	Right    Expression
	Operator string
}

func (ae *JSAssignmentExpression) expressionNode()      {}
func (ae *JSAssignmentExpression) TokenLiteral() string { return ae.Operator }

type JSBinaryExpression struct {
	Left     Expression
	Right    Expression
	Operator string
}

func (be *JSBinaryExpression) expressionNode()      {}
func (be *JSBinaryExpression) TokenLiteral() string { return be.Operator }

type JSIfStatement struct {
	Test       Expression
	Consequent *BlockStatement
	Alternate  Node // Can be BlockStatement or IfStatement (for else if)
}

func (is *JSIfStatement) expressionNode()      {}
func (is *JSIfStatement) statementNode()       {}
func (is *JSIfStatement) TokenLiteral() string { return "if" }

type JSForStatement struct {
	Init   Node // Can be Expression, VariableDeclaration, or nil
	Test   Expression
	Update Expression
	Body   *BlockStatement
}

func (fs *JSForStatement) statementNode()       {}
func (fs *JSForStatement) TokenLiteral() string { return "for" }

type JSWhileStatement struct {
	Test Expression
	Body *BlockStatement
}

func (ws *JSWhileStatement) statementNode()       {}
func (ws *JSWhileStatement) TokenLiteral() string { return "while" }

type JSDoWhileStatement struct {
	Test Expression
	Body *BlockStatement
}

func (dw *JSDoWhileStatement) statementNode()       {}
func (dw *JSDoWhileStatement) TokenLiteral() string { return "do while" }

type JSReturnStatement struct {
	Value Expression
}

func (rs *JSReturnStatement) statementNode()       {}
func (rs *JSReturnStatement) TokenLiteral() string { return "return" }

type JSCallExpression struct {
	Callee    Expression
	Arguments []Expression
}

func (ce *JSCallExpression) expressionNode()      {}
func (ce *JSCallExpression) TokenLiteral() string { return "call" }

type JSObjectExpression struct {
	Properties []*JSProperty
}

func (oe *JSObjectExpression) expressionNode()      {}
func (oe *JSObjectExpression) TokenLiteral() string { return "object" }

type JSProperty struct {
	Key   Expression // Can be Identifier or Literal
	Value Expression
}

type JSArrayExpression struct {
	Elements []Expression
}

func (ae *JSArrayExpression) expressionNode()      {}
func (ae *JSArrayExpression) TokenLiteral() string { return "array" }

type JSUnaryExpression struct {
	Operator string
	Argument Expression
}

func (ue *JSUnaryExpression) expressionNode()      {}
func (ue *JSUnaryExpression) TokenLiteral() string { return ue.Operator }

type JSMemberExpression struct {
	Object   Expression
	Property Expression // Can be Identifier or Expression (for computed properties)
	Computed bool
}

func (e *JSMemberExpression) expressionNode()      {}
func (e *JSMemberExpression) TokenLiteral() string { return "." }

type JSSwitchStatement struct {
	Discriminant Expression
	Cases        []*JSSwitchCase
}

func (ss *JSSwitchStatement) statementNode()       {}
func (ss *JSSwitchStatement) TokenLiteral() string { return "switch" }

type JSSwitchCase struct {
	Test       Expression // nil for default case
	Consequent []Statement
}

type JSTryStatement struct {
	Block     *BlockStatement
	Handler   *JSCatchClause
	Finalizer *BlockStatement
}

func (ts *JSTryStatement) statementNode()       {}
func (ts *JSTryStatement) TokenLiteral() string { return "try" }

type JSCatchClause struct {
	Param *Identifier
	Body  *BlockStatement
}

type JSClassDeclaration struct {
	ID         *Identifier
	SuperClass *Identifier
	Body       *JSClassBody
}

func (cd *JSClassDeclaration) statementNode()       {}
func (cd *JSClassDeclaration) TokenLiteral() string { return "class" }

type JSClassBody struct {
	Body []JSMethodDefinition
}

type JSMethodDefinition struct {
	Key      Expression // Can be Identifier or Literal
	Value    *JSFunction
	Kind     string // "method", "constructor", or "get/set"
	Static   bool
	Computed bool
}

type JSTemplateLiteral struct {
	Quasis      []*JSTemplateElement
	Expressions []Expression
}

func (tl *JSTemplateLiteral) expressionNode()      {}
func (tl *JSTemplateLiteral) TokenLiteral() string { return "`" }

type JSTemplateElement struct {
	Value string
	Tail  bool
}

type Identifier struct {
	Name string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Name }

type NumberLiteral struct {
	Value float64
}

func (nl *NumberLiteral) expressionNode()      {}
func (nl *NumberLiteral) TokenLiteral() string { return fmt.Sprintf("%v", nl.Value) }

type StringLiteral struct {
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Value }

type BooleanLiteral struct {
	Value bool
}

func (bl *BooleanLiteral) expressionNode()      {}
func (bl *BooleanLiteral) TokenLiteral() string { return fmt.Sprintf("%v", bl.Value) }

type NullLiteral struct{}

func (nl *NullLiteral) expressionNode()      {}
func (nl *NullLiteral) TokenLiteral() string { return "null" }

type BlockStatement struct {
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return "{" }

type ExpressionStatement struct {
	Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return "expr" }

type JSConditionalExpression struct {
	Test       Expression
	Consequent Expression
	Alternate  Expression
}

func (ce *JSConditionalExpression) expressionNode()      {}
func (ce *JSConditionalExpression) TokenLiteral() string { return "?:" }

type JSNewExpression struct {
	Callee    Expression
	Arguments []Expression
}

func (ne *JSNewExpression) expressionNode()      {}
func (ne *JSNewExpression) TokenLiteral() string { return "new" }

type JSSpreadElement struct {
	Argument Expression
}

func (se *JSSpreadElement) expressionNode()      {}
func (se *JSSpreadElement) TokenLiteral() string { return "..." }

type JSForInOfStatement struct {
	Left  Node // Can be a VariableDeclaration or an Expression
	Right Expression
	Body  *BlockStatement
	Type  string // "ForInStatement" or "ForOfStatement"
}

func (s *JSForInOfStatement) statementNode()       {}
func (s *JSForInOfStatement) TokenLiteral() string { return "for" }
