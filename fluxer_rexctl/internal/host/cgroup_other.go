// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package host

func cgroupLimits() (memoryBytes uint64, cpus int) { return 0, 0 }
