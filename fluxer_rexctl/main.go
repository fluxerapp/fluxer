// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/cmd"
)

var (
	version = "dev"
	commit  = ""
)

func main() {
	os.Exit(cmd.Execute(version, commit))
}
