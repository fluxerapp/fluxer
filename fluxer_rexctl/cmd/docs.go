// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const docsHeader = `---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: rexctl commands
description: Every rexctl command and flag, made from rexctl's own help.
---

This page is made from the help that rexctl prints, and ` + "`rexctl <command> --help`" + ` shows the same text in the terminal.
`

func newDocsCmd(app *App) *cobra.Command {
	var out, envPage, envExample string
	c := &cobra.Command{
		Use:    "docs",
		Short:  "Write the command reference, environment variable page and reference .env.example",
		Hidden: true,
		Args:   noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			page := renderDocs(c.Root())
			if out == "" && envPage == "" && envExample == "" {
				_, err := app.UI.Stdout().Write(page)
				return err
			}
			for file, data := range map[string][]byte{out: page, envPage: []byte(instance.EnvPage()), envExample: []byte(instance.ReferenceEnv())} {
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
	c.Flags().StringVar(&envPage, "env-page", "", "environment variable page to write")
	c.Flags().StringVar(&envExample, "env-example", "", "reference .env.example to write")
	return c
}

func renderDocs(root *cobra.Command) []byte {
	var b bytes.Buffer
	b.WriteString(docsHeader)
	b.WriteString("\n## Flags for every command\n\n")
	writeFlags(&b, root.PersistentFlags())
	for _, c := range docsCommands(root) {
		writeCommand(&b, c, 2)
	}
	return b.Bytes()
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

func writeCommand(b *bytes.Buffer, c *cobra.Command, level int) {
	fmt.Fprintf(b, "\n%s %s\n\n", strings.Repeat("#", level), c.CommandPath())
	b.WriteString(mdx.Prose(docsDescription(c)))
	b.WriteString("\n")
	if c.Runnable() && c.Annotations[helpOnly] == "" {
		fmt.Fprintf(b, "\n```sh\n%s\n```\n", c.UseLine())
		if c.Example != "" {
			fmt.Fprintf(b, "\n```sh\n%s\n```\n", strings.TrimSpace(c.Example))
		}
		if c.HasAvailableLocalFlags() {
			b.WriteString("\n")
			writeFlags(b, c.LocalFlags())
		}
	}
	next := level + 1
	if next > 3 {
		next = 3
	}
	for _, sub := range docsCommands(c) {
		writeCommand(b, sub, next)
	}
}

func docsDescription(c *cobra.Command) string {
	text := strings.TrimSpace(c.Long)
	if text == "" {
		text = strings.TrimSpace(c.Short)
	}
	return sentence(text)
}

func writeFlags(b *bytes.Buffer, fs *pflag.FlagSet) {
	first := true
	fs.VisitAll(func(f *pflag.Flag) {
		if f.Hidden || f.Deprecated != "" || f.Name == "help" {
			return
		}
		if !first {
			b.WriteString("\n")
		}
		first = false
		name, usage := pflag.UnquoteUsage(f)
		head := "`--" + f.Name
		if name != "" && f.Value.Type() != "bool" {
			head += " " + name
		}
		head += "`"
		if f.Shorthand != "" {
			head = "`-" + f.Shorthand + "`, " + head
		}
		fmt.Fprintf(b, "- %s\n\n  %s", head, mdx.Prose(sentence(usage)))
		if d := flagDefault(f); d != "" {
			fmt.Fprintf(b, " The default is `%s`.", d)
		}
		b.WriteString("\n")
	})
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
