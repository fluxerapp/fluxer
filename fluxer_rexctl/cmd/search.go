// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	searchService = "meilisearch"
	searchVolume  = "meilisearch-data"
	reindexPoll   = 2 * time.Second
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
		"Run rexctl search reindex to rebuild it.")
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

func newSearchCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "search", Short: "Manage search on your instance", Args: noArgs}
	var noWait bool
	reindex := &cobra.Command{
		Use:   "reindex",
		Short: "Rebuild search from your data",
		Long:  "Rebuild every search index from your data, with the messages in every channel and DM. Search shows partial results until it finishes. If a rebuild is already running, rexctl follows that one, and stopping rexctl doesn't stop the rebuild.",
		Args:  noArgs,
	}
	reindex.Flags().BoolVar(&noWait, "no-wait", false, "start the rebuild and return without waiting for it")
	reindex.RunE = func(c *cobra.Command, _ []string) error {
		var jobID string
		err := adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			id, err := api.ReindexSearch(ctx)
			jobID = id
			return err
		})
		if err != nil {
			return err
		}
		if noWait {
			app.UI.OK("Started rebuilding search. Run rexctl search reindex to follow it.")
			return nil
		}
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			return followReindex(ctx, app, api, jobID)
		})
	}
	c.AddCommand(reindex)
	return c
}

func followReindex(ctx context.Context, app *App, api *adminapi.Client, jobID string) error {
	return app.UI.Step(ctx, "Rebuilding search", func(ctx context.Context, s *ui.Step) error {
		for {
			st, err := api.SearchReindexStatus(ctx, jobID)
			if err != nil {
				return err
			}
			switch st.Status {
			case "completed":
				s.Done("Rebuilt search.")
				return nil
			case "failed":
				msg := "Rebuilding search failed."
				if st.Error != "" {
					msg = "Rebuilding search failed. " + sentenceOf(st.Error)
				}
				return ui.Failed(msg, "Run rexctl logs worker to see why, then run rexctl search reindex again.")
			case "not_found":
				return ui.Failed("rexctl lost track of the rebuild.", "Run rexctl search reindex again.")
			}
			s.Title(reindexTitle(st))
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(reindexPoll):
			}
		}
	})
}

func reindexTitle(st *adminapi.SearchReindexStatus) string {
	switch st.Phase {
	case "queued":
		return "Waiting for a worker to start rebuilding search"
	case "guild_members":
		return fmt.Sprintf("Rebuilding search for members, %s of %s communities", thousands(st.Indexed), thousands(st.Total))
	case "threads":
		return fmt.Sprintf("Rebuilding search for threads, %s of %s communities", thousands(st.Indexed), thousands(st.Total))
	case "messages":
		return fmt.Sprintf("Rebuilding search for messages, %s of %s channels", thousands(st.Indexed), thousands(st.Total))
	}
	what, ok := reindexPhases[st.Phase]
	if !ok {
		return "Rebuilding search"
	}
	return fmt.Sprintf("Rebuilding search for %s, %s done", what, thousands(st.Indexed))
}

var reindexPhases = map[string]string{
	"users":      "accounts",
	"guilds":     "communities",
	"discovery":  "Explore",
	"reports":    "reports",
	"audit_logs": "the admin audit log",
}

func thousands(n int64) string {
	s := fmt.Sprint(n)
	for i := len(s) - 3; i > 0; i -= 3 {
		s = s[:i] + "," + s[i:]
	}
	return s
}
