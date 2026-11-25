// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const docsHeader = `---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: rexctl commands
description: Every rexctl command and flag, from its help.
---

This is the help text rexctl prints for every command and flag.

Add ` + "`--help`" + ` to any command to see it there.
`

func newDocsCmd(app *App) *cobra.Command {
	var out, settingsPage string
	c := &cobra.Command{
		Use:    "docs",
		Short:  "Write the command reference and settings page",
		Hidden: true,
		Args:   noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			terms := docsTerms(c.Root())
			page := renderDocs(c.Root(), terms)
			if out == "" && settingsPage == "" {
				_, err := app.UI.Stdout().Write(page)
				return err
			}
			settingsData, err := renderSettings(terms)
			if err != nil {
				return err
			}
			for file, data := range map[string][]byte{out: page, settingsPage: settingsData} {
				if file == "" {
					continue
				}
				if err := os.WriteFile(file, data, 0o644); err != nil {
					return ui.Wrap(err, fmt.Sprintf("Couldn't write %s.", file), "Check that the folder exists and that you can write to it.")
				}
			}
			return nil
		},
	}
	c.Flags().StringVar(&out, "out", "", "file to write")
	c.Flags().StringVar(&settingsPage, "settings-page", "", "settings page to write")
	return c
}

func renderDocs(root *cobra.Command, t mdx.Terms) []byte {
	var b bytes.Buffer
	b.WriteString(docsHeader)
	b.WriteString("\n## Commands\n\n")
	writeCommandIndex(&b, root, t)
	b.WriteString("\n## Flags for every command\n\n")
	writeFlags(&b, root.PersistentFlags(), t)
	for _, c := range docsCommands(root) {
		writeCommand(&b, c, 2, t)
	}
	return b.Bytes()
}

func docsTerms(root *cobra.Command) mdx.Terms {
	keys := map[string][]string{}
	for _, k := range settings.Named() {
		keys[k.Name] = docsValues(k.Type, k.Options)
	}
	if schema, err := adminapi.ConfigSchema(); err == nil {
		for _, k := range schema {
			keys[k.Key] = docsValues(k.Type, k.Options)
		}
	}
	commands := map[string]bool{}
	valueFlags := map[string]bool{}
	var walk func(c *cobra.Command, path string)
	walk = func(c *cobra.Command, path string) {
		c.Flags().VisitAll(func(f *pflag.Flag) {
			if f.Value.Type() != "bool" {
				valueFlags["--"+f.Name] = true
			}
		})
		for _, sub := range c.Commands() {
			if sub.Hidden {
				continue
			}
			commands[path+" "+sub.Name()] = true
			walk(sub, path+" "+sub.Name())
		}
	}
	walk(root, root.Name())
	return mdx.Terms{Keys: keys, Commands: commands, ValueFlags: valueFlags}
}

func docsValues(kind string, options []string) []string {
	if kind == "bool" || kind == "boolean" {
		return []string{"true", "false"}
	}
	return options
}

func docsCommands(c *cobra.Command) []*cobra.Command {
	var out []*cobra.Command
	for _, sub := range c.Commands() {
		if sub.Hidden || sub.Deprecated != "" || sub.Name() == "help" || sub.Name() == "completion" {
			continue
		}
		out = append(out, sub)
	}
	return out
}

func writeCommand(b *bytes.Buffer, c *cobra.Command, level int, t mdx.Terms) {
	fmt.Fprintf(b, "\n%s %s\n\n", strings.Repeat("#", level), c.CommandPath())
	b.WriteString(mdx.Prose(mdx.Code(docsDescription(c), t)))
	b.WriteString("\n")
	if c.Runnable() && c.Annotations[helpOnly] == "" {
		fmt.Fprintf(b, "\nRun `%s`\n", strings.TrimSuffix(c.UseLine(), " [flags]"))
		if lines := exampleLines(c.Example); len(lines) > 0 {
			fmt.Fprintf(b, "\n```sh title=\"Examples\"\n%s\n```\n", strings.Join(lines, "\n\n"))
		}
		if c.HasAvailableLocalFlags() {
			b.WriteString("\n")
			writeFlags(b, c.LocalFlags(), t)
		}
	}
	next := level + 1
	if next > 3 {
		next = 3
	}
	for _, sub := range docsCommands(c) {
		writeCommand(b, sub, next, t)
	}
}

func exampleLines(example string) []string {
	var lines []string
	for _, line := range strings.Split(example, "\n") {
		if line = strings.TrimSpace(line); line != "" {
			lines = append(lines, line)
		}
	}
	return lines
}

func docsDescription(c *cobra.Command) string {
	text := strings.TrimSpace(c.Long)
	if text == "" {
		text = strings.TrimSpace(c.Short)
	}
	return sentence(text)
}

func writeCommandIndex(b *bytes.Buffer, root *cobra.Command, t mdx.Terms) {
	b.WriteString("| Command | What it does |\n| --- | --- |\n")
	for _, c := range docsCommands(root) {
		anchor := strings.ReplaceAll(c.CommandPath(), " ", "-")
		fmt.Fprintf(b, "| [`%s`](#%s) | %s |\n", c.CommandPath(), anchor, docsCell(t, firstSentence(sentence(c.Short))))
	}
}

func docsCell(t mdx.Terms, text string) string {
	return strings.ReplaceAll(mdx.Prose(mdx.Code(text, t)), "|", "\\|")
}

func writeFlags(b *bytes.Buffer, fs *pflag.FlagSet, t mdx.Terms) {
	var rows []string
	fs.VisitAll(func(f *pflag.Flag) {
		if f.Hidden || f.Deprecated != "" || f.Name == "help" {
			return
		}
		name, usage := pflag.UnquoteUsage(f)
		head := "`--" + f.Name
		if name != "" && f.Value.Type() != "bool" {
			head += " " + name
		}
		head += "`"
		if f.Shorthand != "" {
			head = "`-" + f.Shorthand + "`, " + head
		}
		def := ""
		switch d := flagDefault(f); {
		case d == "true" && f.Value.Type() == "bool":
			def = "On"
		case d != "":
			def = "`" + strings.ReplaceAll(d, "|", "\\|") + "`"
		}
		help := sentence(usage)
		if d := flagDefault(f); d == "true" && f.Value.Type() == "bool" && !strings.Contains(f.Usage, "--no-"+f.Name) {
			help += " Turn it off with `--" + f.Name + "=false`."
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s |\n", head, def, docsCell(t, help)))
	})
	if len(rows) == 0 {
		return
	}
	b.WriteString("| Flag | Default | What it does |\n| --- | --- | --- |\n")
	for _, row := range rows {
		b.WriteString(row)
	}
}

func flagDefault(f *pflag.Flag) string {
	switch f.DefValue {
	case "", "false", "[]", "0", "0s":
		return ""
	}
	return f.DefValue
}

func sentence(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return s
	}
	s = strings.ToUpper(s[:1]) + s[1:]
	if !strings.HasSuffix(s, ".") && !strings.HasSuffix(s, "?") && !strings.HasSuffix(s, "!") {
		s += "."
	}
	return s
}
