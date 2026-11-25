// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"fmt"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/lint"
)

func main() {
	repo := ".."
	if len(os.Args) > 1 {
		repo = os.Args[1]
	}
	found, err := lint.DocLinks(repo)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	edits, err := lint.HandEdits(repo)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	found = append(found, edits...)
	for _, f := range found {
		fmt.Printf("%s:%d: %s %s\n", f.File, f.Line, f.Rule, f.Text)
	}
	if len(found) > 0 {
		fmt.Fprintf(os.Stderr, "Found %d problems in the docs.\n", len(found))
		os.Exit(1)
	}
}
