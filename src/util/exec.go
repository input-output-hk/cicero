package util

import (
	"bytes"
	"io"
	"os/exec"
)

func BufStdout(cmd *exec.Cmd) (stdout io.Reader, stdoutBuf bytes.Buffer, err error) {
	if stdout_, err_ := cmd.StdoutPipe(); err != nil {
		err = err_
		return
	} else {
		stdout = io.TeeReader(stdout_, &stdoutBuf)
	}
	return
}

func BufStderr(cmd *exec.Cmd) (stderr io.Reader, stderrBuf bytes.Buffer, err error) {
	if stderr_, err_ := cmd.StderrPipe(); err != nil {
		err = err_
		return
	} else {
		stderr = io.TeeReader(stderr_, &stderrBuf)
	}
	return
}

func BufPipes(cmd *exec.Cmd) (stdout io.Reader, stdoutBuf bytes.Buffer, stderr io.Reader, stderrBuf bytes.Buffer, err error) {
	stdout, stdoutBuf, err = BufStdout(cmd)
	if err != nil {
		return
	}

	stderr, stderrBuf, err = BufStderr(cmd)
	return
}
