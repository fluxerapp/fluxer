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
	found, err := lint.Copy(repo)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	for _, f := range found {
		fmt.Println(f.String())
	}
	if len(found) > 0 {
		fmt.Fprintf(os.Stderr, "Found %d wording problems in user-facing text.\n", len(found))
		os.Exit(1)
	}
}
