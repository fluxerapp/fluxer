// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"context"
	"net/http"
	"time"
)

const (
	pathUpdate    = "/v1/update"
	updateTimeout = 5 * time.Second
)

type UpdateRelease struct {
	Version string    `json:"version"`
	Date    time.Time `json:"date"`
}

type Update struct {
	Running  string          `json:"running"`
	Method   string          `json:"method"`
	Enabled  bool            `json:"enabled"`
	Latest   string          `json:"latest"`
	Newer    []UpdateRelease `json:"newer"`
	Command  string          `json:"command"`
	Outdated bool            `json:"outdated"`
}

func (s *Stack) Update(ctx context.Context) (Update, error) {
	var u Update
	return u, s.callWithin(ctx, updateTimeout, http.MethodGet, pathUpdate, &u)
}
