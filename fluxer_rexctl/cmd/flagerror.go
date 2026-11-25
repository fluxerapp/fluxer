// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"fmt"
	"strings"

	"github.com/spf13/pflag"
)

func flagErrorText(err error) string {
	var missing *pflag.ValueRequiredError
	var invalid *pflag.InvalidValueError
	var unknown *pflag.NotExistError
	var syntax *pflag.InvalidSyntaxError
	switch {
	case errors.As(err, &missing):
		name := missing.GetSpecifiedName()
		if f := missing.GetFlag(); f != nil {
			name = f.Name
		}
		return "--" + name + " needs a value."
	case errors.As(err, &invalid):
		f, v := invalid.GetFlag(), invalid.GetValue()
		switch f.Value.Type() {
		case "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "count":
			return fmt.Sprintf("--%s takes a whole number, like 200, and %s isn't one.", f.Name, v)
		case "bool":
			return fmt.Sprintf("--%s takes true or false, and %s isn't either.", f.Name, v)
		case "duration":
			return fmt.Sprintf("--%s takes a length of time, like 30s or 10m, and %s isn't one.", f.Name, v)
		case "float32", "float64":
			return fmt.Sprintf("--%s takes a number, and %s isn't one.", f.Name, v)
		}
		if cause := errors.Unwrap(invalid); cause != nil && !strings.Contains(cause.Error(), ":") {
			return sentence(cause.Error())
		}
		return fmt.Sprintf("--%s can't be %s.", f.Name, v)
	case errors.As(err, &unknown):
		name := unknown.GetSpecifiedName()
		if unknown.GetSpecifiedShortnames() != "" {
			return "There's no flag called -" + name + "."
		}
		return "There's no flag called --" + name + "."
	case errors.As(err, &syntax):
		return "rexctl can't read the flag " + syntax.GetSpecifiedFlag() + "."
	}
	return sentence(strings.SplitN(err.Error(), "\n", 2)[0])
}
