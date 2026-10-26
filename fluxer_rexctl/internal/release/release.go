// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"time"
)

const (
	ResolverURL    = "https://fluxer.dev/rexctl/release.json"
	ResolverURLEnv = "REXCTL_RELEASE_URL"
	ResumeFlag     = "resume"
	ResumeRecovery = "recovery"
)

const (
	PhaseResolved    = "resolved"
	PhaseSelfUpdated = "self-updated"
	PhasePulled      = "pulled"
	PhaseBackedUp    = "backed-up"
	PhaseMigrated    = "migrated"
	PhaseRecreated   = "recreated"
	PhaseVerified    = "verified"
)

var Phases = []string{PhaseResolved, PhaseSelfUpdated, PhasePulled, PhaseBackedUp, PhaseMigrated, PhaseRecreated, PhaseVerified}

var (
	ErrTooOld         = errors.New("release: current version is older than min_from")
	ErrUnknownVersion = errors.New("release: no such version")
	ErrBadResponse    = errors.New("release: the resolver sent an unexpected response")
)

type Asset struct {
	OS     string `json:"os"`
	Arch   string `json:"arch"`
	URL    string `json:"url"`
	SHA256 string `json:"sha256"`
}

type Release struct {
	Version       string  `json:"version"`
	MinFrom       string  `json:"min_from"`
	SchemaChanged bool    `json:"schema_changed"`
	MinPodman     string  `json:"min_podman,omitempty"`
	MinDocker     string  `json:"min_docker,omitempty"`
	StateSchema   int     `json:"state_schema,omitempty"`
	Assets        []Asset `json:"assets"`
}

type TooOldError struct {
	From    string
	To      string
	MinFrom string
}

func (e *TooOldError) Error() string {
	return fmt.Sprintf("Fluxer %s can't upgrade directly to %s.", e.From, e.To)
}

func (e *TooOldError) Command() string { return "rexctl upgrade --to " + e.MinFrom }

func (e *TooOldError) Is(target error) bool { return target == ErrTooOld }

var httpClient = &http.Client{Timeout: 60 * time.Second}

func resolverURL() string {
	if u := os.Getenv(ResolverURLEnv); u != "" {
		return u
	}
	return ResolverURL
}

func Resolve(ctx context.Context, to string) (*Release, error) {
	u, err := url.Parse(resolverURL())
	if err != nil {
		return nil, err
	}
	if to != "" {
		q := u.Query()
		q.Set("version", to)
		u.RawQuery = q.Encode()
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/json")
	req.Header.Set("User-Agent", "rexctl")
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound && to != "" {
		return nil, ErrUnknownVersion
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%w: status %d", ErrBadResponse, resp.StatusCode)
	}
	return decode(io.LimitReader(resp.Body, 1<<20), to)
}

func decode(r io.Reader, to string) (*Release, error) {
	var rel Release
	if err := json.NewDecoder(r).Decode(&rel); err != nil {
		return nil, fmt.Errorf("%w: %v", ErrBadResponse, err)
	}
	if !Valid(rel.Version) {
		return nil, fmt.Errorf("%w: version %q", ErrBadResponse, rel.Version)
	}
	if rel.MinFrom != "" && !Valid(rel.MinFrom) {
		return nil, fmt.Errorf("%w: min_from %q", ErrBadResponse, rel.MinFrom)
	}
	if to != "" && Compare(rel.Version, to) != 0 {
		return nil, ErrUnknownVersion
	}
	return &rel, nil
}

func parse(v string) ([]int, bool) {
	v = strings.TrimPrefix(v, "v")
	parts := strings.Split(v, ".")
	if len(parts) < 2 || len(parts) > 3 {
		return nil, false
	}
	out := make([]int, 3)
	for i, p := range parts {
		if p == "" {
			return nil, false
		}
		n, err := strconv.Atoi(p)
		if err != nil || n < 0 {
			return nil, false
		}
		out[i] = n
	}
	if out[0] < 2000 {
		return nil, false
	}
	return out, true
}

func Valid(v string) bool {
	_, ok := parse(v)
	return ok
}

func Compare(a, b string) int {
	pa, oka := parse(a)
	pb, okb := parse(b)
	switch {
	case !oka && !okb:
		return strings.Compare(a, b)
	case !oka:
		return -1
	case !okb:
		return 1
	}
	for i := range pa {
		if pa[i] != pb[i] {
			if pa[i] < pb[i] {
				return -1
			}
			return 1
		}
	}
	return 0
}

func Short(v string) string {
	p, ok := parse(v)
	if !ok {
		return v
	}
	return fmt.Sprintf("%d.%d", p[0], p[1])
}

func CheckPath(from string, to *Release) error {
	if to == nil || to.MinFrom == "" || !Valid(from) {
		return nil
	}
	if Compare(from, to.MinFrom) < 0 {
		return &TooOldError{From: from, To: to.Version, MinFrom: to.MinFrom}
	}
	return nil
}

func (r *Release) AssetFor(goos, goarch string) (Asset, error) {
	for _, a := range r.Assets {
		if a.OS == goos && a.Arch == goarch {
			return a, nil
		}
	}
	return Asset{}, errors.New("release: no build for " + goos + "/" + goarch)
}
