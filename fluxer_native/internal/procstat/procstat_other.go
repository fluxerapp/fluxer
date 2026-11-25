// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package procstat

const clockTicks = 100

func groups(map[string]int) (map[int]Usage, error) {
	return map[int]Usage{}, nil
}
