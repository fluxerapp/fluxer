// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"bytes"
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

type fakeSupervisor struct {
	mu    sync.Mutex
	ps    []spec.PsEntry
	calls []string
	hooks map[string]func(action string)
}

func (f *fakeSupervisor) PS(context.Context) ([]spec.PsEntry, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.ps), nil
}

func (f *fakeSupervisor) Service(_ context.Context, name, action string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, action+" "+name)
	if hook := f.hooks[name]; hook != nil {
		hook(action)
	}
	for i := range f.ps {
		if f.ps[i].Name == name {
			f.ps[i].State, f.ps[i].Health = spec.StateStopped, spec.HealthNone
			if action == spec.ActionStart {
				f.ps[i].State, f.ps[i].Health = spec.StateRunning, spec.HealthHealthy
			}
		}
	}
	return nil
}

func (f *fakeSupervisor) JetStream(_ context.Context, action string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, action+" jetstream")
	return nil
}

func run(t *testing.T, env []string, name string, args ...string) string {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Env = append(os.Environ(), env...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("%s %v: %v\n%s", name, args, err, out)
	}
	return strings.TrimSpace(string(out))
}

func writeFile(t *testing.T, path, data string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(data), 0o600); err != nil {
		t.Fatal(err)
	}
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func startValkey(t *testing.T, dir, password string) *exec.Cmd {
	t.Helper()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("valkey-server", "--port", strconv.Itoa(spec.PortValkey), "--bind", spec.Loopback, "--requirepass", password, "--dir", dir, "--appendonly", "yes", "--save", "")
	var out bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &out
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { cmd.Process.Kill(); cmd.Wait() })
	for range 100 {
		if r, err := exec.Command("valkey-cli", "-p", strconv.Itoa(spec.PortValkey), "-a", password, "--no-auth-warning", "PING").Output(); err == nil && strings.TrimSpace(string(r)) == "PONG" {
			return cmd
		}
		time.Sleep(100 * time.Millisecond)
	}
	t.Fatalf("valkey did not start: %s", out.String())
	return nil
}

func stopValkey(t *testing.T, cmd *exec.Cmd) {
	t.Helper()
	cmd.Process.Signal(syscall.SIGTERM)
	cmd.Wait()
}

func valkeyCLI(t *testing.T, password string, args ...string) string {
	return run(t, nil, "valkey-cli", append([]string{"-p", strconv.Itoa(spec.PortValkey), "-a", password, "--no-auth-warning"}, args...)...)
}

func TestBackupRestoreRoundTrip(t *testing.T) {
	for _, bin := range []string{"initdb", "postgres", "pg_isready", "pg_dump", "pg_restore", "psql", "valkey-server", "valkey-cli"} {
		if _, err := exec.LookPath(bin); err != nil {
			t.Skipf("%s is not installed", bin)
		}
	}
	ctx := context.Background()
	base := t.TempDir()
	l := spec.Layout{Data: filepath.Join(base, "data"), Runtime: filepath.Join(base, "run")}
	envFile := filepath.Join(base, "etc", "fluxer.env")
	const pgPassword, kvPassword = "pgsecret", "kvsecret"
	writeFile(t, envFile, "FLUXER_DOMAIN=chat.example.com\n")
	writeFile(t, l.SecretsFile(), "POSTGRES_PASSWORD="+pgPassword+"\nFLUXER_VALKEY_PASSWORD="+kvPassword+"\nFLUXER_NATS_AUTH_TOKEN=old\n")
	tg := Target{Layout: l, Env: spec.Env{"FLUXER_DOMAIN": "chat.example.com", "POSTGRES_PASSWORD": pgPassword, "FLUXER_VALKEY_PASSWORD": kvPassword}}

	pwfile := filepath.Join(base, "pw")
	writeFile(t, pwfile, pgPassword)
	run(t, nil, "initdb", "-D", l.PostgresDir(), "-U", "fluxer", "--auth-local=scram-sha-256", "--auth-host=scram-sha-256", "--pwfile="+pwfile, "-E", "UTF8", "--locale=C.UTF-8")
	pg, err := tg.startPostgres(ctx, l.PGSocketDir())
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if pg != nil {
			pg.stop()
		}
	}()
	psqlEnv := []string{"PGPASSWORD=" + pgPassword}
	psql := func(db, sql string) string {
		return run(t, psqlEnv, "psql", "-X", "-tA", "-h", l.PGSocketDir(), "-p", strconv.Itoa(spec.PortPostgres), "-U", "fluxer", "-d", db, "-c", sql)
	}
	psql("postgres", "CREATE DATABASE fluxer")
	psql("fluxer", "CREATE TABLE messages (id int PRIMARY KEY, body text); INSERT INTO messages VALUES (1, 'hello')")

	kv := startValkey(t, l.ValkeyDir(), kvPassword)
	valkeyCLI(t, kvPassword, "SET", "queue:item", "pending-deletion")

	writeFile(t, filepath.Join(l.SeaweedFSDir(), "volume", "1.dat"), "attachment-bytes")
	if err := os.Symlink("volume/1.dat", filepath.Join(l.SeaweedFSDir(), "latest")); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(l.NATSDir(), "jetstream", "meta.inf"), "stream")
	writeFile(t, filepath.Join(l.MeilisearchDir(), "data.ms", "data.mdb"), "index")

	sup := &fakeSupervisor{ps: []spec.PsEntry{
		{Name: spec.Postgres, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.Valkey, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.SeaweedFS, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.SeaweedFSFiler, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.SeaweedFSInit, State: spec.StateExited, Oneshot: true},
		{Name: spec.Users, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.API, State: spec.StateRunning, Health: spec.HealthHealthy},
		{Name: spec.Gateway, State: spec.StateRunning, Health: spec.HealthHealthy},
	}, hooks: map[string]func(string){spec.Valkey: func(action string) {
		if action == spec.ActionStop {
			stopValkey(t, kv)
		} else {
			kv = startValkey(t, l.ValkeyDir(), kvPassword)
		}
	}}}
	path, err := Create(ctx, tg, sup, CreateOptions{EnvFile: envFile, Release: "2026.1107.0", Reason: "test"})
	if err != nil {
		t.Fatal(err)
	}
	wantCalls := []string{"stop gateway", "stop api", "stop users", "stop seaweedfs-filer", "stop seaweedfs", "stop valkey", "stop jetstream", "start jetstream", "start valkey", "start seaweedfs", "start seaweedfs-filer", "start users", "start api", "start gateway"}
	if !slices.Equal(sup.calls, wantCalls) {
		t.Fatalf("supervisor calls %v, want %v", sup.calls, wantCalls)
	}
	m, err := Verify(path)
	if err != nil {
		t.Fatal(err)
	}
	if m.Database != DumpName || !slices.Equal(m.Volumes, []string{"seaweedfs-data", "valkey-data", "nats-data", "meilisearch-data"}) {
		t.Fatalf("manifest database %q volumes %v", m.Database, m.Volumes)
	}
	if !slices.ContainsFunc(m.Entries, func(e Entry) bool {
		return strings.HasPrefix(e.Path, "volumes/valkey-data/appendonlydir/") && strings.HasSuffix(e.Path, ".manifest")
	}) {
		t.Fatal("the backup has no valkey append-only file")
	}

	psql("fluxer", "DELETE FROM messages; CREATE TABLE stray (id int)")
	const targetPassword = "target"
	psql("postgres", "ALTER ROLE fluxer PASSWORD '"+targetPassword+"'")
	psqlEnv[0] = "PGPASSWORD=" + targetPassword
	tg.Env["POSTGRES_PASSWORD"] = targetPassword
	if err := pg.stop(); err != nil {
		t.Fatal(err)
	}
	pg = nil
	stopValkey(t, kv)
	writeFile(t, filepath.Join(l.SeaweedFSDir(), "volume", "1.dat"), "overwritten")
	writeFile(t, filepath.Join(l.SeaweedFSDir(), "stray"), "x")
	writeFile(t, l.SecretsFile(), "POSTGRES_PASSWORD="+targetPassword+"\nFLUXER_VALKEY_PASSWORD="+kvPassword+"\nFLUXER_NATS_AUTH_TOKEN=new\nFLUXER_SETUP_TOKEN=kept\n")

	held, err := lockData(l)
	if err != nil {
		t.Fatal(err)
	}
	if _, _, err := Restore(ctx, tg, path, RestoreOptions{}); !errors.Is(err, lock.ErrHeld) {
		t.Fatalf("restore under a running fluxerd: %v", err)
	}
	held.Release()

	restoredEnv := filepath.Join(base, "etc", "restored.env")
	if _, _, err := Restore(ctx, tg, path, RestoreOptions{EnvFile: restoredEnv}); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, filepath.Join(l.SeaweedFSDir(), "latest")); got != "attachment-bytes" {
		t.Fatalf("attachment %q", got)
	}
	if _, err := os.Stat(filepath.Join(l.SeaweedFSDir(), "stray")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("a file the backup does not have survived: %v", err)
	}
	if got := readFile(t, restoredEnv); got != "FLUXER_DOMAIN=chat.example.com\n" {
		t.Fatalf("env file %q", got)
	}
	secrets := readFile(t, l.SecretsFile())
	if !strings.Contains(secrets, "FLUXER_NATS_AUTH_TOKEN=old\n") || !strings.Contains(secrets, "FLUXER_SETUP_TOKEN=kept\n") || !strings.Contains(secrets, "POSTGRES_PASSWORD="+targetPassword+"\n") {
		t.Fatalf("secrets %q", secrets)
	}

	if got := readFile(t, l.ModesFile()); got != "FLUXER_KV_MODE=standalone\nFLUXER_STORAGE_BACKEND=s3\n" {
		t.Fatalf("an archive with SeaweedFS and Valkey data restored modes %q", got)
	}

	if pg, err = tg.startPostgres(ctx, l.PGSocketDir()); err != nil {
		t.Fatal(err)
	}
	if got := psql("fluxer", "SELECT body FROM messages"); got != "hello" {
		t.Fatalf("restored message %q", got)
	}
	startValkey(t, l.ValkeyDir(), kvPassword)
	if got := valkeyCLI(t, kvPassword, "GET", "queue:item"); got != "pending-deletion" {
		t.Fatalf("restored queue item %q", got)
	}

}

func TestFilesystemModeBackup(t *testing.T) {
	ctx := context.Background()
	base := t.TempDir()
	l := spec.Layout{Data: filepath.Join(base, "data"), Runtime: filepath.Join(base, "run")}
	envFile := filepath.Join(base, "fluxer.env")
	writeFile(t, envFile, "FLUXER_DOMAIN=chat.example.com\nFLUXER_STORAGE_BACKEND=filesystem\n")
	writeFile(t, l.ModesFile(), "FLUXER_KV_MODE=postgres\nFLUXER_RUST_PROCESS=single\n")
	writeFile(t, filepath.Join(l.ObjectsDir(), "fluxer-uploads", "a"), "upload")
	writeFile(t, filepath.Join(l.ObjectsDir(), ".layout"), spec.ObjectsLayoutContents)
	writeFile(t, filepath.Join(l.ObjectsDir(), ".staging", "tmp", "partial"), "half")
	writeFile(t, filepath.Join(l.SeaweedFSDir(), "volume", "1.dat"), "old weed data")
	writeFile(t, filepath.Join(l.ValkeyDir(), "appendonly.aof"), "old valkey data")
	tg := Target{Layout: l, Env: spec.Env{
		"FLUXER_DOMAIN":        "chat.example.com",
		"FLUXER_POSTGRES_URL":  "postgres://db.example/fluxer",
		spec.StorageBackendEnv: "filesystem",
		spec.KVModeEnv:         "postgres",
	}}
	path, err := Create(ctx, tg, nil, CreateOptions{EnvFile: envFile})
	if err != nil {
		t.Fatal(err)
	}
	m, err := Verify(path)
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(m.Volumes, []string{spec.ObjectsVolume}) || m.HasSkipped(SkippedUploads) {
		t.Fatalf("volumes %v skipped %v", m.Volumes, m.Skipped)
	}
	for _, e := range m.Entries {
		if strings.Contains(e.Path, ".staging") {
			t.Errorf("staging file %s was backed up", e.Path)
		}
	}

	target := spec.Layout{Data: filepath.Join(base, "target")}
	writeFile(t, target.ModesFile(), "FLUXER_KV_MODE=standalone\nFLUXER_STORAGE_BACKEND=s3\n")
	writeFile(t, filepath.Join(target.ValkeyDir(), "appendonly.aof"), "x")
	writeFile(t, filepath.Join(target.SeaweedFSDir(), "volume", "1.dat"), "weed data kept for an undo")
	tg.Layout = target
	if _, _, err := Restore(ctx, tg, path, RestoreOptions{}); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, filepath.Join(target.ObjectsDir(), "fluxer-uploads", "a")); got != "upload" {
		t.Fatalf("restored upload %q", got)
	}
	if got := readFile(t, filepath.Join(target.SeaweedFSDir(), "volume", "1.dat")); got != "weed data kept for an undo" {
		t.Fatalf("SeaweedFS data after restoring a filesystem backup %q", got)
	}
	if got := readFile(t, target.ModesFile()); got != "FLUXER_KV_MODE=postgres\nFLUXER_RUST_PROCESS=single\nFLUXER_STORAGE_BACKEND=filesystem\n" {
		t.Fatalf("restored modes %q", got)
	}
	if _, err := spec.ResolveModes(map[string]string{spec.StorageBackendEnv: "filesystem", spec.KVModeEnv: "postgres"}, target.Data); err != nil {
		t.Fatalf("the restored data dir trips a mode guard: %v", err)
	}
}
