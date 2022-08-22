package util

import (
	"bytes"
	"io"
	"os/exec"
)

func BufStdout(cmd *exec.Cmd) (stdout io.ReadCloser, stdoutBuf *bytes.Buffer, err error) {
	stdout, err = cmd.StdoutPipe()
	if err != nil {
		return
	}

	var stdoutBufVal bytes.Buffer
	stdoutBuf = &stdoutBufVal

	stdout = CompositeReadCloser{
		io.TeeReader(stdout, stdoutBuf),
		stdout,
	}
	return
}

func BufStderr(cmd *exec.Cmd) (stderr io.ReadCloser, stderrBuf *bytes.Buffer, err error) {
	stderr, err = cmd.StderrPipe()
	if err != nil {
		return
	}

	var stderrBufVal bytes.Buffer
	stderrBuf = &stderrBufVal

	stderr = CompositeReadCloser{
		io.TeeReader(stderr, stderrBuf),
		stderr,
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
