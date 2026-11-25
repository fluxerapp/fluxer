// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"runtime"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/children"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/ctl"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/stamp"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/updates"
)

func updateCheckOn(vars spec.Env) bool {
	switch strings.ToLower(strings.TrimSpace(vars[spec.UpdateCheckEnv])) {
	case "true", "1", "yes", "on":
		return true
	}
	return false
}

func updateInstance(vars spec.Env, running string) updates.Instance {
	return methodInstance(vars, running, updates.DetectMethod(vars[spec.InstallMethodEnv], vars[spec.ContainerEnv], os.Getenv("INVOCATION_ID")))
}

func methodInstance(vars spec.Env, running, method string) updates.Instance {
	return updates.Instance{
		Running:  running,
		Method:   method,
		Override: vars[spec.UpgradeCommandEnv],
		GOARCH:   runtime.GOARCH,
		Enabled:  updateCheckOn(vars),
		URL:      cmp.Or(strings.TrimSpace(vars[spec.UpdateCheckURLEnv]), updates.DefaultURL),
		Backup:   updates.BackupNote(method, backupOnUpgrade(vars), strings.TrimSpace(vars["FLUXER_POSTGRES_URL"]) != ""),
	}
}

func newUpdateChecker(cf *configFlags, root string, l spec.Layout, out *logs.Output) *updates.Checker {
	instance := func() (updates.Instance, error) {
		cfg, err := cf.load()
		if err != nil {
			return updates.Instance{}, err
		}
		return updateInstance(cfg.Env, releaseVersion(root)), nil
	}
	return updates.NewChecker(updates.FilePath(l.Data), instance, func(line string) { out.Logf(logs.PrioInfo, "%s", line) })
}

func runUpdateCheck(ctx context.Context, checker *updates.Checker, release string) {
	if stamp.Valid(release) {
		checker.Run(ctx)
	}
}

func printUpdateFooter(ctx context.Context, c *ctl.Client) {
	r, err := c.Update(ctx)
	switch {
	case err != nil:
	case r.Outdated:
		fmt.Print("\nA newer Fluxer is out. Run fluxerd version for the steps.\n")
	case len(r.Newer) > 0:
		fmt.Printf("\nFluxer %s is available. Run fluxerd version for the upgrade command.\n", r.Newer[0].Version)
	}
}

func versionReport(l spec.Layout, vars spec.Env, running string) (updates.Report, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	if rep, err := client(l.Runtime).Update(ctx); err == nil {
		return rep, nil
	}
	path := updates.FilePath(l.Data)
	s, err := updates.Load(path)
	if err != nil {
		return updates.Report{}, err
	}
	in := updateInstance(vars, running)
	if saved, err := updates.LoadReport(path); err == nil && in.Method == "" {
		in = methodInstance(vars, running, saved.Method)
	}
	return s.Report(in), nil
}

func cmdVersion(args []string) error {
	fs := flag.NewFlagSet("version", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	asJSON := fs.Bool("json", false, "print JSON")
	check := fs.Bool("check", false, "ask for the list of releases now, through the running fluxerd")
	runtimeDir := fs.String("runtime-dir", "", "runtime directory that holds fluxerd.sock, when it isn't the one in the env file")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.load()
	if err != nil {
		return err
	}
	if *runtimeDir != "" {
		cfg.Layout.Runtime = *runtimeDir
	}
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return err
	}
	running := releaseVersion(root)
	if voiceOnly(cfg) {
		fmt.Printf("Fluxer   %s\n", cmp.Or(running, "development build"))
		return nil
	}
	var rep updates.Report
	if *check {
		ctx, cancel := context.WithTimeout(context.Background(), updates.Timeout+10*time.Second)
		defer cancel()
		if rep, err = client(cfg.Layout.Runtime).UpdateCheck(ctx); err != nil {
			return fmt.Errorf("fluxerd has to be running to check, with its socket in %s or the folder --runtime-dir gives: %w", cfg.Layout.Runtime, err)
		}
	} else if rep, err = versionReport(cfg.Layout, cfg.Env, running); err != nil {
		return err
	}
	if *asJSON {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(rep); err != nil {
			return err
		}
	} else {
		if rep.Running == "" {
			rep.Running = "development build"
		}
		fmt.Print(rep.Text(time.Now()))
	}
	if *check && rep.Failures > 0 {
		return errors.New("couldn't get the list of releases: " + rep.Error)
	}
	return nil
}
