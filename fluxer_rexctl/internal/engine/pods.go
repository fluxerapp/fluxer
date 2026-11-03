// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"slices"
	"strings"

	cerrdefs "github.com/containerd/errdefs"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/moby/moby/client"
)

const PodmanComposeLabel = "io.podman.compose.project"

func (p *Project) podmanCompose(ctx context.Context, services []string) ([]Container, error) {
	if p.Engine.Kind != Podman {
		return nil, nil
	}
	all, err := p.PS(ctx)
	if err != nil {
		return nil, err
	}
	var out []Container
	for _, c := range all {
		if c.Labels[PodmanComposeLabel] != "" && (len(services) == 0 || slices.Contains(services, c.Service)) {
			out = append(out, c)
		}
	}
	return out, nil
}

func (p *Project) stopPodmanCompose(ctx context.Context, services []string) error {
	cs, err := p.podmanCompose(ctx, services)
	if err != nil || len(cs) == 0 {
		return err
	}
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	for _, ct := range cs {
		if ct.State != "running" {
			continue
		}
		if _, err := c.ContainerStop(ctx, ct.ID, client.ContainerStopOptions{}); err != nil && !cerrdefs.IsNotFound(err) {
			return &Error{Op: "stop", Class: ClassOf(err), Err: err}
		}
	}
	return nil
}

func (p *Project) RemovePodmanCompose(ctx context.Context) error {
	cs, err := p.podmanCompose(ctx, nil)
	if err != nil || len(cs) == 0 {
		return err
	}
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	dial := c.Dialer()
	hc := &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) { return dial(ctx) },
	}}
	var pods []string
	for _, ct := range cs {
		var in struct {
			Pod string `json:"Pod"`
		}
		if err := libpod(ctx, hc, http.MethodGet, "/libpod/containers/"+url.PathEscape(ct.ID)+"/json", &in); err != nil {
			return &Error{Op: "down", Class: ClassOf(err), Err: err}
		}
		if in.Pod != "" && !slices.Contains(pods, in.Pod) {
			pods = append(pods, in.Pod)
		}
	}
	for _, pod := range pods {
		if err := libpod(ctx, hc, http.MethodDelete, "/libpod/pods/"+url.PathEscape(pod)+"?force=true", nil); err != nil {
			return &Error{Op: "down", Class: ClassOf(err), Err: err}
		}
	}
	for _, ct := range cs {
		if _, err := c.ContainerRemove(ctx, ct.ID, client.ContainerRemoveOptions{Force: true}); err != nil && !cerrdefs.IsNotFound(err) {
			return &Error{Op: "down", Class: ClassOf(err), Err: err}
		}
	}
	return nil
}

func (p *Project) RemoveReplacements(ctx context.Context) error {
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	f := projectFilter(p.Name).Add("label", api.ContainerReplaceLabel).Add("status", "created")
	res, err := c.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: f})
	if err != nil {
		return &Error{Op: "ps", Class: ClassOf(err), Err: err}
	}
	for _, ct := range res.Items {
		if len(ct.Names) == 0 || !isReplacementName(ct.Names[0]) {
			continue
		}
		if _, err := c.ContainerRemove(ctx, ct.ID, client.ContainerRemoveOptions{Force: true}); err != nil && !cerrdefs.IsNotFound(err) {
			return &Error{Op: "rm", Class: ClassOf(err), Err: err}
		}
	}
	return nil
}

func isReplacementName(name string) bool {
	prefix, _, ok := strings.Cut(strings.TrimPrefix(name, "/"), "_")
	if !ok || len(prefix) != 12 {
		return false
	}
	for _, r := range prefix {
		if !strings.ContainsRune("0123456789abcdef", r) {
			return false
		}
	}
	return true
}

func libpod(ctx context.Context, hc *http.Client, method, path string, out any) error {
	req, err := http.NewRequestWithContext(ctx, method, "http://d"+path, nil)
	if err != nil {
		return err
	}
	res, err := hc.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	body, err := io.ReadAll(io.LimitReader(res.Body, 4<<20))
	if err != nil {
		return err
	}
	if res.StatusCode == http.StatusNotFound && method == http.MethodDelete {
		return nil
	}
	if res.StatusCode >= 300 {
		return fmt.Errorf("podman %s %s: %s: %s", method, path, res.Status, body)
	}
	if out == nil {
		return nil
	}
	return json.Unmarshal(body, out)
}
