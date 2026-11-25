// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/stamp"
)

const (
	DefaultURL  = "https://fluxer.dev/releases.json"
	UserAgent   = "Fluxer"
	Schema      = 1
	MaxBody     = 256 << 10
	Timeout     = 20 * time.Second
	maxRedirect = 5
)

var (
	ErrFormat      = errors.New("the release list has a format this Fluxer doesn't read")
	ErrNewerFormat = errors.New("the release list has a newer format, which only a newer Fluxer reads")
	errEmpty       = errors.New("the release list has no releases")
)

type Release struct {
	Version string    `json:"version"`
	Date    time.Time `json:"date,omitzero"`
}

func Decode(r io.Reader) ([]Release, error) {
	data, err := io.ReadAll(io.LimitReader(r, MaxBody+1))
	if err != nil {
		return nil, err
	}
	if len(data) > MaxBody {
		return nil, fmt.Errorf("the release list is larger than %d KiB", MaxBody>>10)
	}
	var head struct {
		Schema int `json:"schema"`
	}
	if err := json.Unmarshal(data, &head); err != nil {
		return nil, fmt.Errorf("the release list isn't JSON: %w", err)
	}
	switch {
	case head.Schema > Schema:
		return nil, ErrNewerFormat
	case head.Schema != Schema:
		return nil, ErrFormat
	}
	var feed struct {
		Releases []struct {
			Version string `json:"version"`
			Date    string `json:"date"`
		} `json:"releases"`
	}
	if err := json.NewDecoder(bytes.NewReader(data)).Decode(&feed); err != nil {
		return nil, fmt.Errorf("the release list isn't JSON: %w", err)
	}
	var out []Release
	for _, r := range feed.Releases {
		if !stamp.Valid(r.Version) {
			continue
		}
		date, _ := time.Parse(time.RFC3339, r.Date)
		out = append(out, Release{Version: r.Version, Date: date.UTC()})
	}
	if len(out) == 0 {
		return nil, errEmpty
	}
	sortNewest(out)
	return slices.CompactFunc(out, func(a, b Release) bool { return a.Version == b.Version }), nil
}

func sortNewest(rs []Release) {
	slices.SortStableFunc(rs, func(a, b Release) int {
		c, _ := stamp.Compare(b.Version, a.Version)
		return c
	})
}

func NewClient() *http.Client {
	transport := http.DefaultTransport.(*http.Transport).Clone()
	transport.Proxy = http.ProxyFromEnvironment
	return &http.Client{
		Timeout:   Timeout,
		Transport: transport,
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			if len(via) >= maxRedirect {
				return errors.New("too many redirects")
			}
			if req.URL.Scheme != "https" && req.URL.Scheme != via[0].URL.Scheme {
				return fmt.Errorf("refused a redirect to %s", req.URL.Redacted())
			}
			return nil
		},
	}
}

func Fetch(ctx context.Context, client *http.Client, address string) ([]Release, error) {
	u, err := url.Parse(address)
	if err != nil || (u.Scheme != "https" && u.Scheme != "http") || u.Host == "" {
		return nil, fmt.Errorf("%s is not an http or https address", address)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, address, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/json")
	req.Header.Set("User-Agent", UserAgent)
	resp, err := client.Do(req)
	if err != nil {
		var uerr *url.Error
		if errors.As(err, &uerr) {
			return nil, uerr.Err
		}
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("the server returned %s", resp.Status)
	}
	return Decode(resp.Body)
}
