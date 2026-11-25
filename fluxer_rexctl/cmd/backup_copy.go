// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newBackupCopyCmd(app *App) *cobra.Command {
	var recipients []string
	c := &cobra.Command{
		Use:   "copy-to <user@host:/folder> [file]",
		Short: "Copy a backup to another machine",
		Long:  "Copy a backup to another machine over SSH. Without a file, rexctl copies your newest backup, including the ones Fluxer makes inside its container. It uses scp, so you need an account on the other machine. With --age-recipient, rexctl encrypts the copy with age first. To decrypt it, run rexctl restore --age-identity with the matching age key file.",
		Example: "rexctl backup copy-to --age-recipient age1... backup@storage.example.org:/srv/fluxer\n" +
			"rexctl backup copy-to backup@storage.example.org:/srv/fluxer chat.example.org-20261114-1612.tar.zst",
		Args: maxArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return ui.Usage("Pass where to copy the backup as user@host:/folder.")
			}
			file := ""
			if len(args) == 2 {
				file = args[1]
			}
			return runBackupCopy(c.Context(), app, args[0], file, recipients)
		},
	}
	c.Flags().StringArrayVar(&recipients, "age-recipient", nil, "encrypt the copy for this age public key, which you can pass more than once")
	return c
}

func remoteTarget(dest string) bool {
	h, path, ok := strings.Cut(dest, ":")
	return ok && h != "" && path != "" && filepath.VolumeName(dest) == "" && !strings.HasPrefix(h, "-") && !strings.ContainsAny(h, `/\ `)
}

func runBackupCopy(ctx context.Context, app *App, dest, file string, keys []string) error {
	if !remoteTarget(dest) {
		return ui.Usage("Pass where to copy the backup as user@host:/folder.")
	}
	recipients, err := ageRecipients(keys)
	if err != nil {
		return err
	}
	scp, err := exec.LookPath("scp")
	if err != nil {
		return ui.Refused("rexctl copies backups with scp, and this machine doesn't have it.", "Install the OpenSSH client, then try again.")
	}
	path := ""
	if file != "" {
		path = resolveBackupPath(app, file)
	} else {
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		fetchOwnBackups(ctx, app, inst)
		list, err := backup.List(inst.BackupDir())
		if err != nil || len(list) == 0 {
			return ui.Refused(fmt.Sprintf("There are no backups in %s.", inst.BackupDir()), "Run rexctl backup to make one, then try again.")
		}
		path = list[0].Path
	}
	err = app.UI.Check(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		if _, err := backup.Verify(path); err != nil {
			return backupFileErr(path, err)
		}
		s.Done("The backup file is whole.")
		return nil
	})
	if err != nil {
		return err
	}
	if len(recipients) > 0 {
		app.UI.Info("Encrypting %s for the age key you passed.", filepath.Base(path))
		encrypted, cleanup, err := encryptBackup(path, recipients)
		if err != nil {
			return ui.Wrap(err, "Couldn't encrypt the backup.", "Check that there's free disk space for a second copy, then try again.")
		}
		defer cleanup()
		path = encrypted
	}
	size := int64(0)
	if fi, err := os.Stat(path); err == nil {
		size = fi.Size()
	}
	app.UI.Info("Copying %s (%s) to %s.", filepath.Base(path), ui.Bytes(size), dest)
	c := exec.CommandContext(ctx, scp, "-p", "--", path, dest)
	c.Stdin, c.Stdout, c.Stderr = os.Stdin, app.UI.Stderr(), app.UI.Stderr()
	if err := c.Run(); err != nil {
		if ctx.Err() != nil {
			return &ui.ExitError{Code: ui.ExitInterrupted, Msg: "Stopped copying the backup. The copy on the other machine may be incomplete.", Err: ctx.Err()}
		}
		return ui.Wrap(err, fmt.Sprintf("Couldn't copy the backup to %s.", dest), "Check that you can sign in there with ssh and write to that folder, then try again.")
	}
	app.UI.OK("Copied %s to %s.", filepath.Base(path), dest)
	return nil
}
