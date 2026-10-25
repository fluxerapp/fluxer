// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
)

const (
	searchService = "meilisearch"
	searchVolume  = "meilisearch-data"
)

func runningImage(ctx context.Context, p *engine.Project, service string) string {
	containers, err := p.PS(ctx)
	if err != nil {
		return ""
	}
	for _, c := range containers {
		if c.Service == service {
			return c.Image
		}
	}
	return ""
}

func keepSearchReadable(ctx context.Context, app *App, inst *instance.Instance, p *engine.Project, from, version string) error {
	to := p.Image(searchService)
	if from == "" || to == "" || from == to || !olderSearch(to, from) {
		return nil
	}
	if err := p.Stop(ctx, searchService); err != nil {
		return err
	}
	name := capitalise(fluxerName(release.Short(version)))
	list, _ := backup.List(inst.BackupDir())
	if b := backup.NewestOf(list, version); b != nil {
		err := backup.RestoreVolume(ctx, b.Path, inst, p, searchVolume)
		if err == nil {
			app.UI.Info("%s can't read the search index that a newer Meilisearch wrote, so rexctl put back the copy from the backup from %s. Messages sent since then don't show up in search.", name, backupTime(b.CreatedAt))
			return p.Up(ctx, engine.UpOptions{Services: []string{searchService}, NoDeps: true})
		}
		app.UI.Detail("%v", err)
	}
	if err := backup.ClearVolume(ctx, inst, p, searchVolume); err != nil {
		return err
	}
	app.UI.Warn(fmt.Sprintf("%s can't read the search index that a newer Meilisearch wrote, and no backup has a copy it can read, so rexctl emptied it.", name),
		`Rebuild it with the "Reindex" buttons on the admin panel's search index page, and with "Refresh Channel Messages" on each community's page there.`)
	return p.Up(ctx, engine.UpOptions{Services: []string{searchService}, NoDeps: true})
}

func olderSearch(to, from string) bool {
	a, okA := searchVersion(to)
	b, okB := searchVersion(from)
	if !okA || !okB {
		return true
	}
	return a[0] < b[0] || a[0] == b[0] && a[1] < b[1]
}

func searchVersion(ref string) ([2]int, bool) {
	ref, _, _ = strings.Cut(ref, "@")
	i := strings.LastIndex(ref, ":")
	if i < 0 || strings.Contains(ref[i:], "/") {
		return [2]int{}, false
	}
	parts := strings.Split(strings.TrimPrefix(ref[i+1:], "v"), ".")
	if len(parts) < 2 {
		return [2]int{}, false
	}
	var v [2]int
	for j := range v {
		n, err := strconv.Atoi(parts[j])
		if err != nil {
			return [2]int{}, false
		}
		v[j] = n
	}
	return v, true
}
