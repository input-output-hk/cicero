package util

import "fmt"

type MD5Sum [16]byte

func (self *MD5Sum) Scan(value interface{}) error {
	if b, ok := value.(string); !ok {
		return fmt.Errorf("Cannot scan %T into MD5Sum", value)
	} else if copied := copy(self[:], b); copied != len(*self) {
		return fmt.Errorf("Could only copy %d/%d bytes into MD5Sum", copied, len(*self))
	}
	return nil
}
