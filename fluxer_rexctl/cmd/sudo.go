// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"os"
	"os/user"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
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
	uid, ok := fileOwner(runningBinary())
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
