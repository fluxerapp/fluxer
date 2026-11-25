// SPDX-License-Identifier: AGPL-3.0-or-later

package lock

import (
	"errors"
	"os"
)

func flock(*os.File) (bool, error) {
	return false, errors.ErrUnsupported
}

func probe(*os.File) (bool, error) {
	return false, errors.ErrUnsupported
}
