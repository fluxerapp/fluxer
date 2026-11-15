// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"fmt"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/lint"
)

func main() {
	root := "."
	if len(os.Args) > 1 {
		root = os.Args[1]
	}
	found, err := lint.Comments(root)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	for _, f := range found {
		fmt.Println(f.String())
	}
	if len(found) > 0 {
		fmt.Fprintf(os.Stderr, "Found %d comments in Go files. Only the SPDX header and compiler directives are allowed.\n", len(found))
		os.Exit(1)
	}
}
