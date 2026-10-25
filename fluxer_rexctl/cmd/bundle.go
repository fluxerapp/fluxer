// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newBundleCmd(app *App) *cobra.Command {
	var out string
	c := &cobra.Command{
		Use:    "bundle",
		Short:  "Write the compose files for running Fluxer without rexctl",
		Hidden: true,
		Args:   noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if out == "" {
				return ui.Refused("Pass --out with the folder to write to.", "")
			}
			images, err := stack.EmbeddedImages()
			if err != nil {
				return err
			}
			if len(images.Images) == 0 {
				return ui.Refused("This rexctl has no image set, so it can't pin Fluxer's images.", "Use a released rexctl.")
			}
			if err := stack.Bundle(out, images.Images); err != nil {
				return ui.Wrap(err, fmt.Sprintf("Couldn't write the files to %s.", out), "Check that you can write to that folder.")
			}
			if err := os.WriteFile(filepath.Join(out, ".env.example"), []byte(instance.BundleEnv()), 0o644); err != nil {
				return ui.Wrap(err, fmt.Sprintf("Couldn't write the files to %s.", out), "Check that you can write to that folder.")
			}
			return nil
		},
	}
	c.Flags().StringVar(&out, "out", "", "folder to write")
	return c
}
