// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import "io"

func watchEnter(io.Reader) (<-chan struct{}, func()) {
	return nil, func() {}
}
