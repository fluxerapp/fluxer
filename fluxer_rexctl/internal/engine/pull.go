// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/distribution/reference"
	"github.com/docker/cli/cli/command"
	"github.com/moby/moby/client"
)

func (p *Project) Pull(ctx context.Context, onEvent func(Event)) error {
	if err := p.needModel("pull"); err != nil {
		return err
	}
	return p.pullMissing(ctx, onEvent)
}

func (p *Project) pullMissing(ctx context.Context, onEvent func(Event)) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	emit := func(ev Event) {
		if onEvent != nil {
			onEvent(ev)
		}
	}
	var missing []string
	for _, ref := range p.Images() {
		_, found, err := p.Engine.LocalImage(ctx, ref)
		if err != nil {
			return err
		}
		if !found {
			missing = append(missing, ref)
		}
	}
	for _, ref := range missing {
		emit(Event{ID: "Image " + ref, Kind: "image", Text: "Waiting", Status: EventWorking})
	}
	var errs []error
	var failed []string
	downHosts := map[string]bool{}
	for _, ref := range missing {
		id := "Image " + ref
		host := registryHost(ref)
		if downHosts[host] {
			emit(Event{ID: id, Kind: "image", Text: "Skipped because " + host + " failed for an earlier image", Status: EventError})
			failed = append(failed, ref)
			continue
		}
		emit(Event{ID: id, Kind: "image", Text: "Pulling", Status: EventWorking})
		if err := pullOne(ctx, c, ref, id, emit); err != nil {
			if ctx.Err() != nil {
				return ctx.Err()
			}
			emit(Event{ID: id, Kind: "image", Text: err.Error(), Status: EventError})
			errs = append(errs, fmt.Errorf("%s: %w", ref, err))
			failed = append(failed, ref)
			if registryDown(err) {
				downHosts[host] = true
			}
			continue
		}
		emit(Event{ID: id, Kind: "image", Text: "Pulled", Status: EventDone})
	}
	if len(errs) > 0 {
		err := errors.Join(errs...)
		return &Error{Op: "pull", Class: ClassOf(&Error{Op: "pull", Err: err}), Images: failed, Err: err}
	}
	return nil
}

func registryHost(ref string) string {
	named, err := reference.ParseNormalizedNamed(ref)
	if err != nil {
		return ref
	}
	return reference.Domain(named)
}

func registryDown(err error) bool {
	switch ClassOf(&Error{Op: "pull", Err: err}) {
	case ClassTimeout, ClassDNS, ClassTLS, ClassProxy, ClassRateLimit:
		return true
	}
	return strings.Contains(strings.ToLower(err.Error()), "connection refused")
}

func pullOne(ctx context.Context, c *conn, ref, id string, emit func(Event)) error {
	auth, err := command.RetrieveAuthTokenFromImage(c.cli.ConfigFile(), ref)
	if err != nil {
		detailf("engine: %s: registry credentials: %v. Pulling without them.", ref, err)
		auth = ""
	}
	api := c.cli.Client()
	resp, err := api.ImagePull(ctx, ref, client.ImagePullOptions{RegistryAuth: auth})
	if err != nil {
		return err
	}
	defer resp.Close()
	for msg, err := range resp.JSONMessages(ctx) {
		if err != nil {
			return err
		}
		if msg.Error != nil {
			return msg.Error
		}
		if msg.ID == "" || strings.HasPrefix(msg.Status, "Pulling from") {
			continue
		}
		ev := Event{ID: msg.ID, Parent: id, Kind: "layer", Text: msg.Status, Status: EventWorking}
		if msg.Progress != nil {
			ev.Current, ev.Total = msg.Progress.Current, msg.Progress.Total
		}
		switch msg.Status {
		case "Pull complete", "Already exists", "Download complete":
			ev.Status = EventDone
		}
		emit(ev)
	}
	return nil
}
