// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import "os"

func fileInode(path string) (uint64, bool) {
	_, err := os.Stat(path)
	return 0, err == nil
}
