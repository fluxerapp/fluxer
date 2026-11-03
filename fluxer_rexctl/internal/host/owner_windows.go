// SPDX-License-Identifier: AGPL-3.0-or-later

package host

func dirOwner(string) string { return "" }

func fileOwner(string) (int, int, bool) { return 0, 0, false }
