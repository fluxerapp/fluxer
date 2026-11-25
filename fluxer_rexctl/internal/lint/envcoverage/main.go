// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"fmt"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/lint"
)

func main() {
	repo := ".."
	if len(os.Args) > 1 {
		repo = os.Args[1]
	}
	found, err := lint.EnvCoverage(repo, instance.EnvNames())
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	for _, f := range found {
		fmt.Printf("%s: %s %s\n", f.File, f.Rule, f.Text)
	}
	if len(found) > 0 {
		fmt.Fprintf(os.Stderr, "Found %d environment variables that the reference doesn't match. Add or remove their entries in internal/instance/envvars.go.\n", len(found))
		os.Exit(1)
	}
}
