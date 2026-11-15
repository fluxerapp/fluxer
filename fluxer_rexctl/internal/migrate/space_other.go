// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !unix

package migrate

func FreeBytes(string) (int64, bool) { return 0, false }

func device(string) (uint64, bool) { return 0, false }
