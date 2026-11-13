// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestSudoCommand(t *testing.T) {
	cases := []struct {
		name string
		exe  string
		args []string
		dir  string
		want string
	}{
		{"home install keeps its full path", "/home/u/.local/bin/rexctl", []string{"migrate", "--experimental-native"}, "/home/u/fluxer", "sudo /home/u/.local/bin/rexctl migrate --experimental-native --dir /home/u/fluxer"},
		{"sudo finds rexctl by name", "/usr/bin/rexctl", []string{"status"}, "/opt/fluxer", "sudo rexctl status --dir /opt/fluxer"},
		{"names /usr/local/bin, which some sudo setups skip", "/usr/local/bin/rexctl", []string{"status"}, "/opt/fluxer", "sudo /usr/local/bin/rexctl status --dir /opt/fluxer"},
		{"leaves out a binary in the temporary folder", filepath.Join(os.TempDir(), "x", "out", "rexctl"), []string{"status"}, "", "sudo rexctl status"},
		{"keeps the folder the user passed", "/home/u/.local/bin/rexctl", []string{"install", "--experimental-native", "--dir", "x", "--domain", "chat.example.org"}, "/opt/fluxer", "sudo /home/u/.local/bin/rexctl install --experimental-native --dir x --domain chat.example.org"},
		{"keeps --dir=", "/usr/bin/rexctl", []string{"backup", "--dir=/srv/fluxer"}, "/srv/fluxer", "sudo rexctl backup --dir=/srv/fluxer"},
		{"no instance yet", "/usr/bin/rexctl", []string{"install", "--experimental-native"}, "", "sudo rexctl install --experimental-native"},
		{"quotes what the shell would split", "/home/my user/bin/rexctl", []string{"config", "set", "x=it's here"}, "/home/my user/fluxer", `sudo '/home/my user/bin/rexctl' config set 'x=it'\''s here' --dir '/home/my user/fluxer'`},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := sudoCommand(c.exe, c.args, c.dir); got != c.want {
				t.Fatalf("got %q, want %q", got, c.want)
			}
		})
	}
}

func TestConfigSetHintLeavesOutValues(t *testing.T) {
	got := configSetFlags([]string{"storage.secret_key=hunter2", "--dir", "/opt/fluxer", "--yes", "smtp.password", "hunter3"})
	want := []string{"config", "set", "--dir", "/opt/fluxer", "--yes"}
	if strings.Join(got, " ") != strings.Join(want, " ") {
		t.Fatalf("got %q, want %q", got, want)
	}
}

func TestDefaultNativeDir(t *testing.T) {
	cases := map[string]string{
		"/home/u/fluxer": "/opt/fluxer",
		"/opt/fluxer":    "/opt/fluxer-native",
	}
	if os.Geteuid() != 0 {
		cases[t.TempDir()] = "/opt/fluxer"
	}
	for dir, want := range cases {
		if got := defaultNativeDir(&instance.Instance{Dir: dir}); got != want {
			t.Errorf("for %s got %s, want %s", dir, got, want)
		}
	}
}

func TestUntrustedRefusal(t *testing.T) {
	cases := []struct {
		name string
		err  *instance.UntrustedError
		msg  string
		fix  string
	}{
		{"found under a folder others can change", &instance.UntrustedError{Path: "/home/u", Dir: "/home/u/fluxer", Shared: true}, "other accounts can change /home/u, so root doesn't use it on its own", "pass --dir /home/u/fluxer."},
		{"instance folder others can change", &instance.UntrustedError{Path: "/mnt/user/appdata/fluxer", Dir: "/mnt/user/appdata/fluxer", Shared: true, Inside: true}, "Other accounts can change /mnt/user/appdata/fluxer, so root doesn't use the instance in it.", "chmod go-w /mnt/user/appdata/fluxer,"},
		{"state folder others can change", &instance.UntrustedError{Path: "/srv/fluxer/.rexctl", Dir: "/srv/fluxer", Shared: true, Inside: true}, "Other accounts can change /srv/fluxer/.rexctl, so root doesn't use the instance in /srv/fluxer.", "chmod go-w /srv/fluxer/.rexctl,"},
		{"link in a folder others can change", &instance.UntrustedError{Path: "/srv/shared", Dir: "/srv/shared/fluxer", Shared: true, Link: "/srv/shared/fluxer"}, "/srv/shared/fluxer is a link in /srv/shared", "Pass --dir"},
		{"another owner", &instance.UntrustedError{Path: "/home/v/fluxer", Dir: "/home/v/fluxer"}, "another account owns /home/v/fluxer", "pass --dir /home/v/fluxer."},
		{"record file", &instance.UntrustedError{Path: "/etc/rexctl"}, "Accounts other than root can change /etc/rexctl", "Make root its owner"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			var ee *ui.ExitError
			if !errors.As(untrustedRefusal(c.err), &ee) || !strings.Contains(ee.Msg, c.msg) || !strings.Contains(ee.Fix, c.fix) {
				t.Fatalf("got %+v, want %q and %q", ee, c.msg, c.fix)
			}
		})
	}
}

func TestMovedRefusalPointsAtTheNewInstance(t *testing.T) {
	r := movedRefusal(&instance.MovedError{Dir: "/home/u/fluxer", To: "/opt/fluxer"})
	if !strings.Contains(r.Msg, "/opt/fluxer") || !strings.HasSuffix(r.Fix, "status --dir /opt/fluxer.") || !strings.Contains(r.Fix, "sudo ") {
		t.Fatalf("got %q and %q", r.Msg, r.Fix)
	}
}

func TestMarkMovedRecordsTheNewFolder(t *testing.T) {
	old := instance.MachineRecordPath
	t.Cleanup(func() { instance.MachineRecordPath = old })
	instance.MachineRecordPath = filepath.Join(t.TempDir(), "etc", instance.RecordFileName)
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, instance.StateDirName), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, instance.StateDirName, instance.StateFileName), []byte(`{"schema":6,"project":"fluxer","runtime":"compose"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	app := &App{UI: ui.New(ui.Options{Stdout: io.Discard, Stderr: io.Discard})}
	markMoved(app, &instance.Instance{Dir: dir}, "/opt/fluxer")
	got, err := instance.Find(dir)
	if err != nil || got.State.MovedTo != "/opt/fluxer" {
		t.Fatalf("got %+v, %v, want the state to name /opt/fluxer", got, err)
	}
}
