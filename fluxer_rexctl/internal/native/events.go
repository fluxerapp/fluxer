// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"time"
)

const (
	pathEvents = "/v1/events"
	pathReload = "/v1/reload"
)

var errRestartNeeded = errors.New("native: the change needs a full restart of Fluxer")

func (s *Stack) Reload(ctx context.Context) ([]string, error) {
	var res struct {
		Restarted []string `json:"restarted"`
		Edge      bool     `json:"edge"`
	}
	err := s.callWithin(ctx, 0, http.MethodPost, pathReload, &res)
	var se *statusError
	if errors.As(err, &se) && (se.code == http.StatusConflict || se.code == http.StatusNotFound || se.code == http.StatusMethodNotAllowed) {
		return nil, fmt.Errorf("%w: %s", errRestartNeeded, se.body)
	}
	if res.Edge {
		res.Restarted = append(res.Restarted, "edge")
	}
	return res.Restarted, err
}

func (s *Stack) events(ctx context.Context, fn func()) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://fluxerd"+pathEvents, nil)
	if err != nil {
		return err
	}
	resp, err := s.client(0).Do(req)
	if err != nil {
		return requestFailed(ctx, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return &statusError{code: resp.StatusCode, body: resp.Status}
	}
	sc := bufio.NewScanner(resp.Body)
	sc.Buffer(make([]byte, 64<<10), 4<<20)
	for sc.Scan() {
		if strings.HasPrefix(sc.Text(), "data: ") {
			fn()
		}
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	return sc.Err()
}

func (s *Stack) watchEvents(ctx context.Context, kick chan<- struct{}) {
	for ctx.Err() == nil {
		s.events(ctx, func() {
			select {
			case kick <- struct{}{}:
			default:
			}
		})
		select {
		case <-ctx.Done():
		case <-time.After(time.Second):
		}
	}
}
