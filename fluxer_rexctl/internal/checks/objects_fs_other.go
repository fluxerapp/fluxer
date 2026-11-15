// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package checks

func statFS(string) (fsInfo, bool) { return fsInfo{}, false }
