// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import "runtime"

func Hardware() (memoryBytes uint64, cpus int) {
	memoryBytes, _ = Memory()
	cpus = runtime.NumCPU()
	limit, quota := cgroupLimits()
	if limit > 0 && (memoryBytes == 0 || limit < memoryBytes) {
		memoryBytes = limit
	}
	if quota > 0 && quota < cpus {
		cpus = quota
	}
	return memoryBytes, cpus
}
