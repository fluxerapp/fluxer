// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"encoding/json"
	"fmt"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const path = "internal/instance/shipped.json"

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func run() error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	var known map[string][]string
	if err := json.Unmarshal(data, &known); err != nil {
		return err
	}
	add := func(name string, data []byte) {
		h := instance.ShippedHash(data)
		if !slices.Contains(known[name], h) {
			known[name] = append(known[name], h)
			slices.Sort(known[name])
		}
	}
	set := stack.Embedded()
	names := []string{stack.ComposeFile, stack.Caddyfile}
	for _, name := range stack.OverlayFiles {
		names = append(names, name)
	}
	for _, name := range names {
		data, err := stack.ReadFile(set, name)
		if err != nil {
			return err
		}
		add(name, data)
	}
	add(".env.example", []byte(instance.BundleEnv()))
	return os.WriteFile(path, format(known), 0o644)
}

func format(known map[string][]string) []byte {
	var b strings.Builder
	b.WriteString("{\n")
	names := slices.Sorted(maps.Keys(known))
	for i, name := range names {
		quoted := make([]string, len(known[name]))
		for j, h := range known[name] {
			quoted[j] = strconv.Quote(h)
		}
		end := ",\n"
		if i == len(names)-1 {
			end = "\n"
		}
		line := fmt.Sprintf("%s: [%s]", strconv.Quote(name), strings.Join(quoted, ", "))
		if 2+len(line)+len(end)-1 <= 120 {
			b.WriteString("\t" + line + end)
			continue
		}
		fmt.Fprintf(&b, "\t%s: [\n\t\t%s\n\t]%s", strconv.Quote(name), strings.Join(quoted, ",\n\t\t"), end)
	}
	b.WriteString("}\n")
	return []byte(b.String())
}
