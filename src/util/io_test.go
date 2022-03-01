package util

import (
	"bytes"
	"io"
	"testing"
)

func TestSkipLeadingWhitespaceReader(t *testing.T) {
	given := []byte(" \t\n\rabc d")
	expected := []byte("abc d")

	r := &skipLeadingWhitespaceReader{Reader: bytes.NewReader(given)}

	actual, err := io.ReadAll(r)
	if err != nil {
		t.Fatal(actual, err)
	}

	if !bytes.HasPrefix(actual, expected) {
		t.Fatal(expected, actual)
	}
}
