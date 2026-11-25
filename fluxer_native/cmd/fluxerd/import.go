// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"bytes"
	"cmp"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"maps"
	"net/url"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"slices"
	"strings"
	"syscall"

	"github.com/nats-io/nats.go"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/children"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/natsembed"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const importUsage = "usage: fluxerd import [--dir d | --out -] <file | ->"

type kubeObject struct {
	Kind  string            `json:"kind"`
	Data  map[string]string `json:"data"`
	Items []kubeObject      `json:"items"`
}

func readKubeEnv(r io.Reader) (map[string]string, error) {
	var top kubeObject
	if err := json.NewDecoder(r).Decode(&top); err != nil {
		return nil, fmt.Errorf("expected the output of kubectl get -o json: %w", err)
	}
	objects := []kubeObject{top}
	if top.Kind == "List" {
		objects = top.Items
	}
	env := map[string]string{}
	for _, kind := range []string{"ConfigMap", "Secret"} {
		for _, o := range objects {
			if o.Kind != kind {
				continue
			}
			for k, v := range o.Data {
				if kind == "Secret" {
					b, err := base64.StdEncoding.DecodeString(v)
					if err != nil {
						return nil, fmt.Errorf("the Secret value %s is not base64: %w", k, err)
					}
					v = string(b)
				}
				env[k] = v
			}
		}
	}
	if len(env) == 0 {
		return nil, errors.New("found no Secret or ConfigMap with data. Pass the 0.x release's fluxer-env Secret")
	}
	return env, nil
}

type oldRelease map[string]string

func (o oldRelease) value(k, def string) string { return cmp.Or(strings.TrimSpace(o[k]), def) }

func (o oldRelease) postgresURL() string {
	if u := o.value("FLUXER_POSTGRES_URL", ""); u != "" {
		return u
	}
	u := url.URL{
		Scheme: "postgres",
		User:   url.UserPassword(o.value("FLUXER_POSTGRES_USERNAME", "fluxer"), o["FLUXER_POSTGRES_PASSWORD"]),
		Host:   o.value("FLUXER_POSTGRES_HOST", "postgres") + ":" + o.value("FLUXER_POSTGRES_PORT", "5432"),
		Path:   "/" + o.value("FLUXER_POSTGRES_DATABASE", "fluxer"),
	}
	if o.value("FLUXER_POSTGRES_SSL", "") == "true" {
		u.RawQuery = "sslmode=require"
	}
	return u.String()
}

func (o oldRelease) kvURL() string {
	if u := o.value("FLUXER_KV_URL", ""); u != "" {
		return u
	}
	u := url.URL{Scheme: "redis", Host: "valkey:6379", Path: "/0"}
	if p := o.value(secrets.ValkeyPassword, ""); p != "" {
		u.User = url.UserPassword("", p)
	}
	return u.String()
}

func (o oldRelease) s3Env() map[string]string {
	endpoint := o.value("FLUXER_S3_ENDPOINT", "http://seaweedfs:8333")
	env := map[string]string{
		spec.StorageBackendEnv:        string(spec.StorageS3),
		spec.S3EndpointEnv:            endpoint,
		"FLUXER_S3_PUBLIC_ENDPOINT":   endpoint,
		"FLUXER_S3_REGION":            o.value("FLUXER_S3_REGION", "us-east-1"),
		"FLUXER_S3_FORCE_PATH_STYLE":  o.value("FLUXER_S3_FORCE_PATH_STYLE", "true"),
		"FLUXER_S3_ACCESS_KEY_ID":     o.value("FLUXER_S3_ACCESS_KEY_ID", "fluxer"),
		"FLUXER_S3_SECRET_ACCESS_KEY": o["FLUXER_S3_SECRET_ACCESS_KEY"],
		"FLUXER_S3_SESSION_TOKEN":     o["FLUXER_S3_SESSION_TOKEN"],
	}
	for k, def := range map[string]string{"CDN": "fluxer", "UPLOADS": "fluxer-uploads", "REPORTS": "fluxer-reports", "HARVESTS": "fluxer-harvests"} {
		env["FLUXER_S3_BUCKET_"+k] = o.value("FLUXER_S3_BUCKET_"+k, def)
	}
	return env
}

func bucketMoves(from, to map[string]string) (map[string]string, error) {
	moves := map[string]string{}
	kinds := map[string]string{}
	for _, k := range []string{"CDN", "UPLOADS", "REPORTS", "HARVESTS"} {
		key := "FLUXER_S3_BUCKET_" + k
		f, t := from[key], to[key]
		if t == "" {
			return nil, fmt.Errorf("%s is empty on this instance", key)
		}
		if prev, ok := moves[f]; ok && prev != t {
			return nil, fmt.Errorf("the old instance kept %s and %s files in one bucket %s, so set both FLUXER_S3_BUCKET_%s and FLUXER_S3_BUCKET_%s to %s", kinds[f], k, f, kinds[f], k, prev)
		}
		moves[f], kinds[f] = t, k
	}
	into := map[string]string{}
	for f, t := range moves {
		if other, ok := into[t]; ok {
			return nil, fmt.Errorf("the old buckets %s and %s would both land in %s. Set each FLUXER_S3_BUCKET_* of this instance to its own bucket", other, f, t)
		}
		into[t] = f
	}
	return moves, nil
}

func renameBuckets(root string, moves map[string]string) error {
	for _, dir := range []string{root, filepath.Join(root, ".meta")} {
		staged := map[string]string{}
		for f, t := range moves {
			if f == t {
				continue
			}
			tmp := filepath.Join(dir, ".import-"+f)
			if err := os.Rename(filepath.Join(dir, f), tmp); errors.Is(err, os.ErrNotExist) {
				continue
			} else if err != nil {
				return err
			}
			staged[tmp] = filepath.Join(dir, t)
		}
		for tmp, t := range staged {
			if err := os.Rename(tmp, t); err != nil {
				return err
			}
		}
	}
	return nil
}

var freshSecrets = []string{"POSTGRES_PASSWORD", "MEILI_MASTER_KEY", "FLUXER_S3_SECRET_KEY", "LIVEKIT_API_SECRET", secrets.NATSAuthToken, secrets.ValkeyPassword}

func (o oldRelease) keptSecrets() map[string]string {
	out := map[string]string{}
	for _, s := range append(slices.Clone(secrets.Required), secrets.Native...) {
		if s.Kind == secrets.KindFixed || slices.Contains(freshSecrets, s.Key) || secrets.IsUnset(o[s.Key]) {
			continue
		}
		out[s.Key] = strings.TrimSpace(o[s.Key])
	}
	return out
}

func cmdImport(args []string) error {
	fs := flag.NewFlagSet("import", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	dir := fs.String("dir", "", "directory for the archive (default <data>/backups)")
	out := fs.String("out", "", "- writes the archive to standard output")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if fs.NArg() != 1 {
		return errors.New(importUsage)
	}
	if *out != "" && *out != "-" {
		return errors.New("--out takes only -. Use --dir for a folder")
	}
	if *out == "-" && isTerminal(os.Stdout) {
		return errors.New("--out - writes the archive to standard output, so redirect it to a file")
	}
	in := os.Stdin
	if name := fs.Arg(0); name != "-" {
		f, err := os.Open(name)
		if err != nil {
			return err
		}
		defer f.Close()
		in = f
	} else if isTerminal(os.Stdin) {
		return errors.New("import - reads the output of kubectl get -o json from standard input, so pipe it in")
	}
	env, err := readKubeEnv(in)
	if err != nil {
		return err
	}
	old := oldRelease(env)
	cfg, err := cf.load()
	if err != nil {
		return err
	}
	if voiceOnly(cfg) {
		return errors.New("a voice server keeps no data, so there is nothing to import into")
	}
	if cfg.Env["FLUXER_POSTGRES_URL"] != "" && *out != "-" {
		return errors.New("this instance uses an outside PostgreSQL, and fluxerd restore can't load an import into it. Do the move with PostgreSQL in the pod")
	}
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return err
	}
	if was, now := old.value("FLUXER_BASE_DOMAIN", ""), strings.TrimSpace(cfg.Env["FLUXER_DOMAIN"]); was != "" && now != "" && was != now {
		fmt.Fprintf(os.Stderr, "import: warning: the old instance used %s and this one uses %s. Passkeys and links in old messages still point at %s\n", was, now, was)
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	opts := backup.CreateOptions{
		Dir:      cmp.Or(*dir, cfg.Layout.BackupsDir()),
		Release:  releaseVersion(root),
		Reason:   "import",
		Progress: func(stage string) { fmt.Fprintf(os.Stderr, "import: archive %s\n", stage) },
	}
	if *out == "-" {
		opts.Out = os.Stdout
	}
	path, err := importOld(ctx, cf, cfg, old, opts)
	if err != nil {
		return err
	}
	if opts.Out == nil {
		fmt.Println(path)
		fmt.Fprintf(os.Stderr, "import: done. Restore it with fluxerd restore %s\n", path)
	}
	return nil
}

func importOld(ctx context.Context, cf *configFlags, cfg *config.Config, old oldRelease, opts backup.CreateOptions) (string, error) {
	scratch := spec.Layout{Data: cfg.Layout.Tmp("import")}
	scratch.Runtime = filepath.Join(scratch.Data, "run")
	if err := os.RemoveAll(scratch.Data); err != nil {
		return "", err
	}
	defer os.RemoveAll(scratch.Data)
	if err := os.MkdirAll(scratch.Runtime, 0o700); err != nil {
		return "", err
	}
	sc := *cfg
	sc.Layout = scratch
	sc.Env = maps.Clone(cfg.Env)
	for _, k := range []string{"FLUXER_POSTGRES_URL", spec.KVURLEnv, spec.S3EndpointEnv} {
		delete(sc.Env, k)
	}
	maps.Copy(sc.Env, spec.InstallDefaults)
	if err := config.WriteModes(scratch, spec.InstallDefaults); err != nil {
		return "", err
	}
	if err := writeSecrets(scratch.SecretsFile(), old.keptSecrets()); err != nil {
		return "", err
	}
	if _, err := secrets.Ensure(scratch, sc.Env); err != nil {
		return "", err
	}
	cc, _, err := cf.children(&sc)
	if err != nil {
		return "", err
	}
	if err := children.PreparePostgres(ctx, cc); err != nil {
		return "", err
	}
	if err := children.PrepareObjects(scratch); err != nil {
		return "", err
	}
	t, err := backupTarget(&sc)
	if err != nil {
		return "", err
	}
	if err := importData(ctx, t, cc, old); err != nil {
		return "", err
	}
	if err := importStreams(ctx, cc, old); err != nil {
		return "", err
	}
	return backup.Create(ctx, t, nil, opts)
}

func importData(ctx context.Context, t backup.Target, cc children.Config, old oldRelease) (err error) {
	api, err := apiChild(cc)
	if err != nil {
		return err
	}
	from := old.s3Env()
	moves, err := bucketMoves(from, api.Env)
	if err != nil {
		return err
	}
	fmt.Fprintln(os.Stderr, "import: copying the database")
	dump := filepath.Join(t.Layout.Data, "old.dump")
	if err := t.DumpRemote(ctx, old.postgresURL(), dump); err != nil {
		return err
	}
	pg, err := t.StartPostgres(ctx, t.Layout.PGSocketDir())
	if err != nil {
		return err
	}
	defer func() {
		if serr := pg.Stop(); err == nil {
			err = serr
		}
	}()
	f, err := os.Open(dump)
	if err != nil {
		return err
	}
	err = t.RestoreDump(ctx, pg.Sock, f)
	f.Close()
	os.Remove(dump)
	if err != nil {
		return err
	}
	fmt.Fprintln(os.Stderr, "import: copying Valkey into PostgreSQL")
	var kv struct {
		Copied int `json:"copied"`
	}
	if err := runMigration(ctx, api, nil, &kv, "kv", "--from-url", old.kvURL()); err != nil {
		return fmt.Errorf("copying Valkey: %w", err)
	}
	fmt.Fprintf(os.Stderr, "import: copied %d keys\n", kv.Copied)
	fmt.Fprintln(os.Stderr, "import: copying uploads")
	var files struct {
		Buckets []struct {
			Objects int `json:"objects"`
		} `json:"buckets"`
	}
	if err := runMigration(ctx, api, from, &files, "storage", "--root", t.Layout.ObjectsDir()); err != nil {
		return fmt.Errorf("copying uploads: %w", err)
	}
	if err := renameBuckets(t.Layout.ObjectsDir(), moves); err != nil {
		return fmt.Errorf("moving the uploads into this instance's bucket folders: %w", err)
	}
	n := 0
	for _, b := range files.Buckets {
		n += b.Objects
	}
	fmt.Fprintf(os.Stderr, "import: copied %d files\n", n)
	return nil
}

func apiChild(cc children.Config) (spec.Child, error) {
	specs, err := children.Specs(cc)
	if err != nil {
		return spec.Child{}, err
	}
	for _, c := range specs {
		if c.Name == spec.API && len(c.Argv) == 2 {
			return c, nil
		}
	}
	return spec.Child{}, errors.New("this release runs no API to copy the data with")
}

func runMigration(ctx context.Context, api spec.Child, extra map[string]string, report any, args ...string) error {
	env := maps.Clone(api.Env)
	maps.Copy(env, extra)
	for _, k := range []string{"HOME", "TMPDIR"} {
		if err := os.MkdirAll(env[k], 0o700); err != nil {
			return err
		}
	}
	script := filepath.Join(filepath.Dir(api.Argv[1]), "MigrateEntrypoint.js")
	cmd := exec.CommandContext(ctx, api.Argv[0], append([]string{script}, args...)...)
	cmd.Dir, cmd.Env = api.Cwd, env.List()
	var stdout bytes.Buffer
	cmd.Stdout, cmd.Stderr = &stdout, os.Stderr
	runErr := cmd.Run()
	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	var verified struct {
		Verified bool `json:"verified"`
	}
	last := []byte(lines[len(lines)-1])
	if json.Unmarshal(last, &verified) != nil || json.Unmarshal(last, report) != nil {
		return cmp.Or(runErr, errors.New("the copy printed no report"))
	}
	if !verified.Verified {
		return fmt.Errorf("the copy doesn't match the source: %s", last)
	}
	return runErr
}

func importStreams(ctx context.Context, cc children.Config, old oldRelease) error {
	fmt.Fprintln(os.Stderr, "import: copying NATS streams")
	o, err := children.NATS(cc)
	if err != nil {
		return err
	}
	o.Port, o.Quiet = -1, true
	srv, err := natsembed.Start(o)
	if err != nil {
		return err
	}
	defer func() { srv.Shutdown(); srv.WaitForShutdown() }()
	var tok []nats.Option
	if t := old.value(secrets.NATSAuthToken, ""); t != "" {
		tok = append(tok, nats.Token(t))
	}
	src, err := nats.Connect(old.value("FLUXER_NATS_URL", "nats://nats:4222"), tok...)
	if err != nil {
		return fmt.Errorf("connecting to the old NATS: %w", err)
	}
	defer src.Close()
	dst, err := nats.Connect(srv.ClientURL(), nats.Token(o.Token))
	if err != nil {
		return err
	}
	defer dst.Close()
	c, err := natsembed.CopyStreams(ctx, src, dst)
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "import: copied %d messages in %d streams\n", c.Messages, c.Streams)
	return nil
}

func writeSecrets(path string, values map[string]string) error {
	var b strings.Builder
	for _, k := range slices.Sorted(maps.Keys(values)) {
		b.WriteString(k + "=" + config.Quote(values[k]) + "\n")
	}
	return os.WriteFile(path, []byte(b.String()), 0o600)
}
