package util

import (
	"bytes"
	"io"
	"os/exec"
)

func BufStdout(cmd *exec.Cmd) (stdout io.ReadCloser, stdoutBuf *bytes.Buffer, err error) {
	if stdout, err = cmd.StdoutPipe(); err == nil {
		stdout, stdoutBuf = bufReadCloser(stdout)
	}
	return
}

func BufStderr(cmd *exec.Cmd) (stderr io.ReadCloser, stderrBuf *bytes.Buffer, err error) {
	if stderr, err = cmd.StderrPipe(); err == nil {
		stderr, stderrBuf = bufReadCloser(stderr)
	}
	return
}

func BufPipes(cmd *exec.Cmd) (stdout io.ReadCloser, stdoutBuf *bytes.Buffer, stderr io.ReadCloser, stderrBuf *bytes.Buffer, err error) {
	stdout, stdoutBuf, err = BufStdout(cmd)
	if err != nil {
		return
	}

	stderr, stderrBuf, err = BufStderr(cmd)
	return
}

func bufReadCloser(in io.ReadCloser) (rc io.ReadCloser, buf *bytes.Buffer) {
	var bufVal bytes.Buffer
	buf = &bufVal

	rc = CompositeReadCloser{
		io.TeeReader(in, buf),
		in,
	}
	return
}
