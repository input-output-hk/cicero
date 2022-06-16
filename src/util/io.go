package util

import (
	"io"
	"unicode"
	"unicode/utf8"
)

func SkipLeadingWhitespaceReader(reader io.Reader) io.Reader {
	return &skipLeadingWhitespaceReader{Reader: reader}
}

type skipLeadingWhitespaceReader struct {
	io.Reader
	passthrough bool
}

func (f *skipLeadingWhitespaceReader) Read(p []byte) (int, error) {
	if f.passthrough {
		return f.Reader.Read(p)
	}

	var bufArr [utf8.UTFMax]byte
	buf := bufArr[:]

	n, err := f.Reader.Read(buf)
	if err != nil {
		return n, err
	}
	buf = buf[:n]

	for len(buf) > 0 {
		r, rs := utf8.DecodeRune(buf)

		if r == utf8.RuneError || !unicode.IsSpace(r) {
			f.passthrough = true
			break
		}

		buf = buf[rs:]
	}

	copy(p, buf)
	return len(buf), nil
}

type CompositeReadCloser struct {
	r io.Reader
	c io.Closer
}

func (self CompositeReadCloser) Read(p []byte) (int, error) {
	return self.r.Read(p)
}

func (self CompositeReadCloser) Close() error {
	return self.c.Close()
}
