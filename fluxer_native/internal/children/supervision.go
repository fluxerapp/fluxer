// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

type healthExprs struct {
	interval, timeout, startPeriod, retries string
}

var defaultHealth = healthExprs{
	interval:    "${FLUXER_HEALTHCHECK_INTERVAL:-10s}",
	timeout:     "${FLUXER_HEALTHCHECK_TIMEOUT:-5s}",
	startPeriod: "${FLUXER_HEALTHCHECK_START_PERIOD:-60s}",
	retries:     "${FLUXER_HEALTHCHECK_RETRIES:-10}",
}

var appHealth = healthExprs{
	interval:    defaultHealth.interval,
	timeout:     defaultHealth.timeout,
	startPeriod: "${FLUXER_APP_HEALTHCHECK_START_PERIOD:-90s}",
	retries:     "${FLUXER_APP_HEALTHCHECK_RETRIES:-30}",
}

var svcHealth = healthExprs{
	interval:    defaultHealth.interval,
	timeout:     defaultHealth.timeout,
	startPeriod: "${FLUXER_SVC_HEALTHCHECK_START_PERIOD:-60s}",
	retries:     appHealth.retries,
}

var childHealth = map[string]healthExprs{
	spec.SeaweedFS: {
		interval:    defaultHealth.interval,
		timeout:     defaultHealth.timeout,
		startPeriod: "${FLUXER_SEAWEEDFS_HEALTHCHECK_START_PERIOD:-60s}",
		retries:     "${FLUXER_SEAWEEDFS_HEALTHCHECK_RETRIES:-20}",
	},
	spec.API:     appHealth,
	spec.Gateway: appHealth,
	spec.MediaProxy: {
		interval:    "${FLUXER_MEDIA_PROXY_HEALTHCHECK_INTERVAL:-30s}",
		timeout:     defaultHealth.timeout,
		startPeriod: defaultHealth.startPeriod,
		retries:     defaultHealth.retries,
	},
	spec.AppProxy: {
		interval:    "${FLUXER_APP_PROXY_HEALTHCHECK_INTERVAL:-30s}",
		timeout:     defaultHealth.timeout,
		startPeriod: defaultHealth.startPeriod,
		retries:     defaultHealth.retries,
	},
	spec.RustServer: svcHealth,
	spec.Snowflakes: svcHealth,
	spec.Users:      svcHealth,
	spec.Messages:   svcHealth,
	spec.Gifs:       svcHealth,
	spec.Unfurl:     svcHealth,
	spec.Push:       svcHealth,
	spec.Admin:      svcHealth,
}

var keepOnUnhealthy = []string{spec.Postgres, spec.Valkey, spec.SeaweedFS, spec.SeaweedFSFiler}

func (c Config) supervision(children []spec.Child) error {
	vars := c.vars()
	restart, err := restartPolicy(vars["FLUXER_RESTART_POLICY"])
	if err != nil {
		return err
	}
	for i := range children {
		ch := &children[i]
		exprs, ok := childHealth[ch.Name]
		if !ok {
			exprs = defaultHealth
		}
		if ch.Name == spec.SeaweedFSFiler {
			exprs = childHealth[spec.SeaweedFS]
		}
		if ch.Health, err = healthCheck(exprs, vars); err != nil {
			return fmt.Errorf("%s: %w", ch.Name, err)
		}
		ch.RestartUnhealthy = !slices.Contains(keepOnUnhealthy, ch.Name)
		if !ch.Oneshot {
			ch.Restart = restart
		}
	}
	return nil
}

func healthCheck(e healthExprs, vars map[string]string) (spec.HealthCheck, error) {
	var h spec.HealthCheck
	durations := []struct {
		expr string
		out  *time.Duration
	}{{e.interval, &h.Interval}, {e.timeout, &h.Timeout}, {e.startPeriod, &h.StartPeriod}}
	for _, d := range durations {
		v, err := interpolate(d.expr, vars)
		if err != nil {
			return h, err
		}
		if *d.out, err = time.ParseDuration(strings.TrimSpace(v)); err != nil || *d.out <= 0 {
			return h, fmt.Errorf("%q is not a duration like 10s or 1m30s", v)
		}
	}
	v, err := interpolate(e.retries, vars)
	if err != nil {
		return h, err
	}
	if h.Retries, err = strconv.Atoi(strings.TrimSpace(v)); err != nil || h.Retries <= 0 {
		return h, fmt.Errorf("%q is not a number of retries", v)
	}
	return h, nil
}

func restartPolicy(v string) (spec.Restart, error) {
	name, _, _ := strings.Cut(strings.TrimSpace(v), ":")
	switch name {
	case "", "always", "unless-stopped":
		return spec.RestartAlways, nil
	case "on-failure":
		return spec.RestartOnFailure, nil
	case "no":
		return spec.RestartNever, nil
	}
	return "", fmt.Errorf("FLUXER_RESTART_POLICY is %q, use no, on-failure, always or unless-stopped", v)
}

var ignoredPrefixes = []string{"FLUXER_WORKER_"}

var ignoredVars = map[string]string{
	"FLUXER_NATS_URL":           "fluxerd runs its own NATS server",
	"FLUXER_NATS_JETSTREAM_URL": "fluxerd runs its own NATS server",
	"FLUXER_NATS_GOMEMLIMIT":    "the NATS server runs inside fluxerd",
	"FLUXER_LOG_MAX_SIZE":       "logs go to the systemd journal",
	"FLUXER_LOG_MAX_FILE":       "logs go to the systemd journal",
	"FLUXER_IMAGE_TAG":          "native releases have no container images",
	"FLUXER_REGISTRY":           "native releases have no container images",
}

func Ignored(vars map[string]string) []string {
	var out []string
	for _, k := range slices.Sorted(maps.Keys(vars)) {
		if strings.TrimSpace(vars[k]) == "" {
			continue
		}
		if why, ok := ignoredVars[k]; ok {
			out = append(out, k+" is ignored, because "+why)
			continue
		}
		if slices.ContainsFunc(ignoredPrefixes, func(p string) bool { return strings.HasPrefix(k, p) }) {
			out = append(out, k+" is ignored, because the worker runs inside the api process")
		}
	}
	return out
}
