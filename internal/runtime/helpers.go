package runtime

import (
	"fmt"
	"reflect"
)

func Ternary(condition bool, consequent, alternate func() interface{}) interface{} {
	if condition {
		return consequent()
	}
	return alternate()
}

func SpreadToArray(v interface{}) []interface{} {
	switch arr := v.(type) {
	case []interface{}:
		return arr
	case string:
		result := make([]interface{}, len(arr))
		for i, ch := range arr {
			result[i] = string(ch)
		}
		return result
	default:
		return []interface{}{v}
	}
}

func GetIndexedValue(obj interface{}, index interface{}) (interface{}, error) {
	switch o := obj.(type) {
	case []interface{}:
		i, ok := index.(int)
		if !ok {
			return nil, fmt.Errorf("index must be an integer for array access")
		}
		if i < 0 || i >= len(o) {
			return nil, fmt.Errorf("index out of range")
		}
		return o[i], nil
	case map[string]interface{}:
		key, ok := index.(string)
		if !ok {
			return nil, fmt.Errorf("key must be a string for object property access")
		}
		val, exists := o[key]
		if !exists {
			return nil, fmt.Errorf("property %s does not exist", key)
		}
		return val, nil
	default:
		return nil, fmt.Errorf("cannot index into type %T", obj)
	}
}

func NewObject(constructor interface{}, args ...interface{}) interface{} {
	constructorValue := reflect.ValueOf(constructor)
	if constructorValue.Kind() != reflect.Func {
		panic(fmt.Sprintf("Cannot use %T as a constructor", constructor))
	}

	constructedType := constructorValue.Type().Out(0)
	instance := reflect.New(constructedType).Elem()

	reflectArgs := make([]reflect.Value, len(args))
	for i, arg := range args {
		reflectArgs[i] = reflect.ValueOf(arg)
	}

	constructorValue.Call(reflectArgs)

	return instance.Interface()
}
