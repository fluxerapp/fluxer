// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import "io/fs"

func dirOwner(string) string { return "" }

func FileOwner(string) (int, int, bool) { return 0, 0, false }

func OwnerOf(fs.FileInfo) (int, int, bool) { return 0, 0, false }
