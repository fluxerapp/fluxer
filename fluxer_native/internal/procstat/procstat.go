// SPDX-License-Identifier: AGPL-3.0-or-later

package procstat

import (
	"context"
	"time"
)

type Usage struct {
	CPUTicks uint64
	RSSBytes int64
}

type Stat struct {
	Name       string  `json:"name"`
	PID        int     `json:"pid"`
	CPUPercent float64 `json:"cpu_percent"`
	RSSBytes   int64   `json:"rss_bytes"`
}

func Sample(ctx context.Context, pids map[string]int, window time.Duration) ([]Stat, error) {
	before, err := groups(pids)
	if err != nil {
		return nil, err
	}
	start := time.Now()
	timer := time.NewTimer(window)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case <-timer.C:
	}
	after, err := groups(pids)
	if err != nil {
		return nil, err
	}
	elapsed := time.Since(start).Seconds()
	out := make([]Stat, 0, len(pids))
	for name, pid := range pids {
		st := Stat{Name: name, PID: pid, CPUPercent: -1, RSSBytes: -1}
		if a, ok := after[pid]; ok && pid > 0 {
			st.RSSBytes = a.RSSBytes
			if b, ok := before[pid]; ok && a.CPUTicks >= b.CPUTicks && elapsed > 0 {
				st.CPUPercent = float64(a.CPUTicks-b.CPUTicks) / float64(clockTicks) / elapsed * 100
			}
		}
		out = append(out, st)
	}
	return out, nil
}
