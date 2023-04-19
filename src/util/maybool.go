package util

type MayBool struct {
	// Wrapped instead of using the state as an enum
	// so that the user cannot use wrong values.
	state mayBoolState
}

type mayBoolState byte

const (
	mayBoolTrue mayBoolState = iota
	mayBoolFalse
	mayBoolNone
)

func NewMayBool[T bool | *bool](v T) MayBool {
	switch t := any(v).(type) {
	case bool:
		if t {
			return MayBool{mayBoolTrue}
		}
		return MayBool{mayBoolFalse}
	case *bool:
		if t == nil {
			return MayBool{mayBoolNone}
		}
		return NewMayBool(*t)
	default:
		panic("Unknown type. This is a bug.")
	}
}

func (self MayBool) String() string {
	switch self.state {
	case mayBoolTrue:
		return "true"
	case mayBoolFalse:
		return "false"
	case mayBoolNone:
		return "none"
	default:
		panic("Unknown type. This is a bug.")
	}
}

func True() MayBool {
	return MayBool{mayBoolTrue}
}

func False() MayBool {
	return MayBool{mayBoolFalse}
}

func None() MayBool {
	return MayBool{mayBoolNone}
}

func (self MayBool) Ptr() *bool {
	switch self.state {
	case mayBoolTrue:
		true := true
		return &true
	case mayBoolFalse:
		false := false
		return &false
	case mayBoolNone:
		return nil
	default:
		panic("Unknown state. This is a bug.")
	}
}

func (self MayBool) Neg() MayBool {
	switch self.state {
	case mayBoolTrue:
		return False()
	case mayBoolFalse:
		return True()
	case mayBoolNone:
		return None()
	default:
		panic("Unknown state. This is a bug.")
	}
}

func (self MayBool) TrueElseNone() MayBool {
	if self.state == mayBoolTrue {
		return self
	}
	return None()
}

func (self MayBool) FalseElseNone() MayBool {
	if self.state == mayBoolFalse {
		return self
	}
	return None()
}
