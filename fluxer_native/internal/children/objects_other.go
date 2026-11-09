// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package children

func nameMax(string) (int, bool) { return 0, false }

func networkFS(string) string { return "" }
