// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"errors"
	"fmt"
)

const (
	ExitOK          = 0
	ExitFailed      = 1
	ExitUsage       = 2
	ExitRefused     = 3
	ExitInterrupted = 130
)

type ExitError struct {
	Code int
	Msg  string
	Fix  string
	Err  error
}

func (e *ExitError) Error() string {
	if e.Msg != "" {
		return e.Msg
	}
	if e.Err != nil {
		return e.Err.Error()
	}
	return fmt.Sprintf("exit %d", e.Code)
}

func (e *ExitError) Unwrap() error { return e.Err }

func Failed(msg, fix string) *ExitError { return &ExitError{Code: ExitFailed, Msg: msg, Fix: fix} }

func Usage(msg string) *ExitError { return &ExitError{Code: ExitUsage, Msg: msg} }

func Refused(msg, fix string) *ExitError { return &ExitError{Code: ExitRefused, Msg: msg, Fix: fix} }

func MissingFlag(flag string) *ExitError {
	return &ExitError{Code: ExitUsage, Msg: fmt.Sprintf("Pass --%s.", flag)}
}

func Wrap(err error, msg, fix string) *ExitError {
	return &ExitError{Code: ExitFailed, Msg: msg, Fix: fix, Err: err}
}

func CodeOf(err error) int {
	if err == nil {
		return ExitOK
	}
	var ee *ExitError
	if errors.As(err, &ee) {
		return ee.Code
	}
	return ExitFailed
}
