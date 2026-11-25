// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"os"
	"os/user"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var sudoPath = []string{"/usr/sbin", "/usr/bin", "/sbin", "/bin"}

var plainWord = regexp.MustCompile(`^[A-Za-z0-9_@%+=:,./-]+$`)

func sudoCommand(exe string, args []string, dir string) string {
	name := "rexctl"
	if exe != "" && !slices.Contains(sudoPath, filepath.Dir(exe)) && !temporary(exe) {
		name = exe
	}
	return sudoLine(name, args, dir)
}

func temporary(exe string) bool {
	tmp := filepath.Clean(os.TempDir())
	return tmp != "/" && strings.HasPrefix(exe, tmp+"/")
}

func sudoLine(name string, args []string, dir string) string {
	words := []string{"sudo", shellWord(name)}
	for _, a := range args {
		words = append(words, shellWord(a))
	}
	if dir != "" && !slices.ContainsFunc(args, func(a string) bool { return a == "--dir" || strings.HasPrefix(a, "--dir=") }) {
		words = append(words, "--dir", shellWord(dir))
	}
	return strings.Join(words, " ")
}

func shellWord(s string) string {
	if plainWord.MatchString(s) {
		return s
	}
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

func runningBinary() string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	return exe
}

func sudoAgain(dir string) string {
	args := os.Args[1:]
	if len(args) >= 2 && args[0] == "config" && args[1] == "set" {
		return sudoCommand(runningBinary(), configSetFlags(args[2:]), dir) + " followed by the same settings"
	}
	return sudoCommand(runningBinary(), args, dir)
}

func rootAgain(dir string) string {
	return strings.TrimPrefix(sudoAgain(dir), "sudo ")
}

func configSetFlags(args []string) []string {
	out := []string{"config", "set"}
	for i := 0; i < len(args); i++ {
		switch a := args[i]; {
		case a == "--dir" && i+1 < len(args):
			out = append(out, a, args[i+1])
			i++
		case strings.HasPrefix(a, "--"):
			out = append(out, a)
		}
	}
	return out
}

func sudoRun(dir string, args ...string) string {
	return sudoCommand(runningBinary(), args, dir)
}

func ranWithSudo() bool {
	uid, err := strconv.Atoi(os.Getenv("SUDO_UID"))
	return euid() == 0 && err == nil && uid > 0
}

func ownersInstance() string {
	if euid() != 0 || ranWithSudo() {
		return ""
	}
	uid, _, ok := host.FileOwner(runningBinary())
	if !ok || uid == 0 {
		return ""
	}
	u, err := user.LookupId(strconv.Itoa(uid))
	if err != nil || !filepath.IsAbs(u.HomeDir) {
		return ""
	}
	dir := filepath.Join(u.HomeDir, "fluxer")
	if _, err := os.Stat(filepath.Join(dir, instance.StateDirName, instance.StateFileName)); err != nil {
		return ""
	}
	return dir
}

func absoluteBinary() string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	if real, err := filepath.EvalSymlinks(exe); err == nil {
		return real
	}
	return exe
}

func sudoArgs(exe string, args []string, dir string, environ []string) []string {
	out := append([]string{exe}, args...)
	if dir != "" && !slices.ContainsFunc(args, func(a string) bool { return a == "--dir" || strings.HasPrefix(a, "--dir=") }) {
		out = append(out, "--dir", dir)
	}
	for _, kv := range environ {
		if name, value, ok := strings.Cut(kv, "="); ok && terminalVar(name) && safeEnvValue(value) {
			out = append(out, sudoEnvFlag+kv)
		}
	}
	return out
}

const sudoEnvFlag = "--sudo-env="

const maxEnvValue = 256

func terminalVar(name string) bool {
	switch name {
	case "TERM", "COLORTERM", "LANG", "LANGUAGE", "NO_COLOR", "CLICOLOR_FORCE", "FORCE_COLOR", "COLORFGBG", "ACCESSIBLE", "TERM_PROGRAM",
		"REXCTL_GLYPHS", "REXCTL_NO_REX", "REXCTL_REDUCED_MOTION":
		return true
	}
	rest, ok := strings.CutPrefix(name, "LC_")
	return ok && rest != "" && strings.IndexFunc(rest, func(r rune) bool { return (r < 'A' || r > 'Z') && r != '_' }) < 0
}

func safeEnvValue(v string) bool {
	return len(v) <= maxEnvValue && strings.IndexFunc(v, func(r rune) bool { return r < 0x20 || r == 0x7f }) < 0
}

func takeSudoEnv(args []string, fromSudo bool, setenv func(string, string) error) []string {
	out := make([]string, 0, len(args))
	for _, a := range args {
		kv, ok := strings.CutPrefix(a, sudoEnvFlag)
		if !ok {
			out = append(out, a)
			continue
		}
		name, value, ok := strings.Cut(kv, "=")
		if fromSudo && ok && terminalVar(name) && safeEnvValue(value) {
			_ = setenv(name, value)
		}
	}
	return out
}

func (a *App) asRoot(dir string) (bool, error) { return a.asRootFor(dir, "") }

func (a *App) asRootFor(dir, why string) (bool, error) {
	if euid() == 0 {
		return true, nil
	}
	if !a.UI.Interactive() {
		return false, ui.Refused("This needs root, because root manages this instance.", "Run "+sudoAgain(dir)+".")
	}
	ok, err := a.UI.ConfirmWith(ui.Question{Text: "This needs root. Run it with sudo now?", Detail: why, Default: true})
	if err != nil {
		return false, err
	}
	if !ok {
		a.UI.Info("Nothing changed.")
		return false, nil
	}
	exe := absoluteBinary()
	if exe == "" {
		return false, ui.Refused("rexctl can't find its own file to run it with sudo.", "Run "+sudoAgain(dir)+".")
	}
	if err := execSudoProcess(sudoArgs(exe, os.Args[1:], dir, os.Environ())); err != nil {
		return false, ui.Wrap(err, "Couldn't run rexctl with sudo.", "Run "+sudoAgain(dir)+".")
	}
	return false, nil
}

func (a *App) retryAsRoot(dir string, err error) error {
	if !errors.Is(err, rt.ErrNeedsRoot) {
		return err
	}
	ok, rerr := a.asRoot(dir)
	if ok {
		return err
	}
	return rerr
}

var errDeclined = errors.New("the question was declined")
