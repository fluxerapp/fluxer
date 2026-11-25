// SPDX-License-Identifier: AGPL-3.0-or-later

package procstat

import (
	"bytes"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

const clockTicks = 100

func groups(pids map[string]int) (map[int]Usage, error) {
	wanted := map[int]bool{}
	for _, pid := range pids {
		if pid > 0 {
			wanted[pid] = true
		}
	}
	out := map[int]Usage{}
	entries, err := os.ReadDir("/proc")
	if err != nil {
		return nil, err
	}
	page := int64(os.Getpagesize())
	for _, e := range entries {
		if _, err := strconv.Atoi(e.Name()); err != nil {
			continue
		}
		data, err := os.ReadFile(filepath.Join("/proc", e.Name(), "stat"))
		if err != nil {
			continue
		}
		pgrp, ticks, rss, ok := parseStat(data)
		if !ok || !wanted[pgrp] {
			continue
		}
		u := out[pgrp]
		u.CPUTicks += ticks
		u.RSSBytes += rss * page
		out[pgrp] = u
	}
	return out, nil
}

func parseStat(data []byte) (pgrp int, ticks uint64, rss int64, ok bool) {
	end := bytes.LastIndexByte(data, ')')
	if end < 0 {
		return 0, 0, 0, false
	}
	f := strings.Fields(string(data[end+1:]))
	if len(f) < 22 {
		return 0, 0, 0, false
	}
	pgrp, err1 := strconv.Atoi(f[2])
	utime, err2 := strconv.ParseUint(f[11], 10, 64)
	stime, err3 := strconv.ParseUint(f[12], 10, 64)
	pages, err4 := strconv.ParseInt(f[21], 10, 64)
	if err1 != nil || err2 != nil || err3 != nil || err4 != nil {
		return 0, 0, 0, false
	}
	return pgrp, utime + stime, pages, true
}
