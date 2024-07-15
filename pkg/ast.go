package ast

type JSNode interface {
	IsJSNode()
}

type GoNode interface {
	IsGoNode()
}

// Common structures

type Identifier struct {
	Name string
}

func (Identifier) IsJSNode() {}
func (Identifier) IsGoNode() {}

type Literal struct {
	Raw string
}

func (Literal) IsJSNode() {}
func (Literal) IsGoNode() {}

// JavaScript-specific structures

type JSFunction struct {
	Name       *Identifier
	Parameters []*Identifier
	Body       []JSNode
}

func (JSFunction) IsJSNode() {}

type JSVariableDeclaration struct {
	Kind         string
	Declarations []*JSVariableDeclarator
}

func (JSVariableDeclaration) IsJSNode() {}

type JSVariableDeclarator struct {
	ID   *Identifier
	Init JSNode
}

type JSAssignmentExpression struct {
	Left     JSNode
	Right    JSNode
	Operator string
}

func (JSAssignmentExpression) IsJSNode() {}

type JSBinaryExpression struct {
	Left     JSNode
	Right    JSNode
	Operator string
}

func (JSBinaryExpression) IsJSNode() {}

type JSIfStatement struct {
	Test       JSNode
	Consequent []JSNode
	Alternate  []JSNode
}

func (JSIfStatement) IsJSNode() {}

type JSForStatement struct {
	Init   JSNode
	Test   JSNode
	Update JSNode
	Body   []JSNode
}

func (JSForStatement) IsJSNode() {}

type JSWhileStatement struct {
	Test JSNode
	Body []JSNode
}

func (JSWhileStatement) IsJSNode() {}

type JSReturnStatement struct {
	Argument JSNode
}

func (JSReturnStatement) IsJSNode() {}

type JSCallExpression struct {
	Callee    JSNode
	Arguments []JSNode
}

func (JSCallExpression) IsJSNode() {}

type JSObjectExpression struct {
	Properties []*JSProperty
}

func (JSObjectExpression) IsJSNode() {}

type JSProperty struct {
	Key   JSNode
	Value JSNode
}

type JSArrayExpression struct {
	Elements []JSNode
}

func (JSArrayExpression) IsJSNode() {}

type JSUnaryExpression struct {
	Operator string
	Argument JSNode
}

func (JSUnaryExpression) IsJSNode() {}

type JSMemberExpression struct {
	Object   JSNode
	Property *Identifier
}

func (JSMemberExpression) IsJSNode() {}

// Go-specific structures

type GoFunction struct {
	Name       *Identifier
	Parameters []*GoParameter
	ReturnType GoNode
	Body       []GoNode
}

func (GoFunction) IsGoNode() {}

type GoParameter struct {
	Name *Identifier
	Type GoNode
}

type GoVariableDeclaration struct {
	Names  []*Identifier
	Type   GoNode
	Values []GoNode
}

func (GoVariableDeclaration) IsGoNode() {}

type GoAssignmentStatement struct {
	Left  []GoNode
	Right []GoNode
}

func (GoAssignmentStatement) IsGoNode() {}

type GoIfStatement struct {
	Init      GoNode
	Condition GoNode
	Body      []GoNode
	Else      []GoNode
}

func (GoIfStatement) IsGoNode() {}

type GoForStatement struct {
	Init      GoNode
	Condition GoNode
	Post      GoNode
	Body      []GoNode
}

func (GoForStatement) IsGoNode() {}

type GoReturnStatement struct {
	Results []GoNode
}

func (GoReturnStatement) IsGoNode() {}

type GoCallExpression struct {
	Function  GoNode
	Arguments []GoNode
}

func (GoCallExpression) IsGoNode() {}

type GoStructType struct {
	Fields []*GoStructField
}

func (GoStructType) IsGoNode() {}

type GoStructField struct {
	Names []*Identifier
	Type  GoNode
	Tag   *Literal
}

type GoInterfaceType struct {
	Methods []*GoMethod
}

func (GoInterfaceType) IsGoNode() {}

type GoMethod struct {
	Name       *Identifier
	Parameters []*GoParameter
	Results    []*GoParameter
}

type GoSelectorExpr struct {
	X   GoNode
	Sel *Identifier
}

func (GoSelectorExpr) IsGoNode() {}

type GoExpressionStatement struct {
	Expression GoNode
}

func (GoExpressionStatement) IsGoNode() {}

type GoBinaryExpression struct {
	Left     GoNode
	Operator string
	Right    GoNode
}

func (GoBinaryExpression) IsGoNode() {}

// Additional types for both languages

type BlockStatement struct {
	Body []JSNode // or []GoNode, depending on the context
}

func (BlockStatement) IsJSNode() {}
func (BlockStatement) IsGoNode() {}

type ExpressionStatement struct {
	Expression JSNode // or GoNode, depending on the context
}

func (ExpressionStatement) IsJSNode() {}
func (ExpressionStatement) IsGoNode() {}
