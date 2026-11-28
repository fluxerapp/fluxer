// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"fmt"
	"os"
	"strings"
)

const nixSystemPath = "/run/current-system/sw"

type UnitFile struct {
	Name string
	Data []byte
}

func NixOS() bool {
	if _, err := os.Stat("/etc/NIXOS"); err == nil {
		return true
	}
	id, _ := OSRelease()
	return id == "nixos"
}

func (a AutoUpgrade) UnitFiles() ([]UnitFile, error) {
	a.Scheduler = Systemd
	data, err := a.files()
	if err != nil {
		return nil, err
	}
	out := []UnitFile{{a.ServiceName(), data[0]}, {a.TimerName(), data[1]}}
	if a.FullBackup == "" {
		return out, nil
	}
	backup, err := a.backupFiles()
	if err != nil {
		return nil, err
	}
	return append(out, UnitFile{a.BackupServiceName(), backup[0]}, UnitFile{a.BackupTimerName(), backup[1]}), nil
}

func (b ScheduledBackup) UnitFiles(every string) ([]UnitFile, error) {
	b.Scheduler = Systemd
	data, err := b.files(every)
	if err != nil {
		return nil, err
	}
	return []UnitFile{{b.name() + ".service", data[0]}, {b.TimerName(), data[1]}}, nil
}

func Timers(units []UnitFile) []string {
	var out []string
	for _, u := range units {
		if strings.HasSuffix(u.Name, ".timer") {
			out = append(out, u.Name)
		}
	}
	return out
}

func UnitRuns(units []UnitFile) map[string]string {
	out := map[string]string{}
	for _, u := range units {
		if strings.HasSuffix(u.Name, ".service") {
			out[u.Name] = execStart(u.Data)
		}
	}
	return out
}

func execStart(data []byte) string {
	var out []string
	for _, line := range strings.Split(string(data), "\n") {
		if v, ok := strings.CutPrefix(strings.TrimSpace(line), "ExecStart="); ok {
			out = append(out, strings.TrimSpace(v))
		}
	}
	return strings.Join(out, "\n")
}

func TimersOnMachine(timers []string, runs map[string]string) (missing, elsewhere bool) {
	for _, t := range timers {
		if _, err := os.Stat(systemUnitDir + "/" + t); err != nil {
			missing = true
		}
	}
	for name, want := range runs {
		data, err := os.ReadFile(systemUnitDir + "/" + name)
		switch {
		case err != nil:
			missing = true
		case execStart(data) != want:
			elsewhere = true
		}
	}
	return missing, elsewhere
}

func UnitText(units []UnitFile) string {
	var b strings.Builder
	for i, u := range units {
		if i > 0 {
			b.WriteString("\n")
		}
		fmt.Fprintf(&b, "# %s/%s\n%s", systemUnitDir, u.Name, u.Data)
	}
	return b.String()
}

func NixConfig(units []UnitFile) string {
	var b strings.Builder
	for _, u := range units {
		kind, name := "services", strings.TrimSuffix(u.Name, ".service")
		if t, ok := strings.CutSuffix(u.Name, ".timer"); ok {
			kind, name = "timers", t
		}
		fmt.Fprintf(&b, "systemd.%s.%s = {\n", kind, nixString(name))
		writeNixUnit(&b, kind == "services", string(u.Data))
		b.WriteString("};\n")
	}
	return b.String()
}

var nixLists = map[string]string{"After": "after", "Wants": "wants", "Requires": "requires", "WantedBy": "wantedBy"}

func writeNixUnit(b *strings.Builder, service bool, data string) {
	sections := map[string][][2]string{}
	var order []string
	section := ""
	for _, line := range strings.Split(data, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]") {
			section = strings.Trim(line, "[]")
			order = append(order, section)
			continue
		}
		k, v, ok := strings.Cut(line, "=")
		if !ok || section == "" {
			continue
		}
		v = strings.ReplaceAll(v, "/usr/bin/install", nixSystemPath+"/bin/install")
		sections[section] = append(sections[section], [2]string{k, v})
	}
	var attrs [][2]string
	for _, name := range []string{"Unit", "Install"} {
		for _, kv := range sections[name] {
			switch {
			case kv[0] == "Description":
				fmt.Fprintf(b, "  description = %s;\n", nixString(kv[1]))
			case nixLists[kv[0]] != "":
				fmt.Fprintf(b, "  %s = %s;\n", nixLists[kv[0]], nixList(strings.Fields(kv[1])))
			default:
				attrs = append(attrs, kv)
			}
		}
	}
	if len(attrs) > 0 {
		writeNixAttrs(b, "unitConfig", attrs)
	}
	if service {
		fmt.Fprintf(b, "  path = %s;\n", nixList([]string{nixSystemPath}))
	}
	for _, name := range order {
		switch name {
		case "Service":
			writeNixAttrs(b, "serviceConfig", sections[name])
		case "Timer":
			writeNixAttrs(b, "timerConfig", sections[name])
		}
	}
}

func writeNixAttrs(b *strings.Builder, attr string, kvs [][2]string) {
	values := map[string][]string{}
	var keys []string
	for _, kv := range kvs {
		if _, ok := values[kv[0]]; !ok {
			keys = append(keys, kv[0])
		}
		values[kv[0]] = append(values[kv[0]], kv[1])
	}
	fmt.Fprintf(b, "  %s = {\n", attr)
	for _, k := range keys {
		v := nixString(values[k][0])
		if len(values[k]) > 1 {
			v = nixList(values[k])
		}
		fmt.Fprintf(b, "    %s = %s;\n", k, v)
	}
	b.WriteString("  };\n")
}

func nixList(items []string) string {
	quoted := make([]string, len(items))
	for i, s := range items {
		quoted[i] = nixString(s)
	}
	return "[ " + strings.Join(quoted, " ") + " ]"
}

func nixString(s string) string {
	s = strings.NewReplacer(`\`, `\\`, `"`, `\"`, "${", `\${`).Replace(s)
	return `"` + s + `"`
}
