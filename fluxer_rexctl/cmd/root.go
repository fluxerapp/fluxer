// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"runtime"
	"strings"
	"syscall"

	"charm.land/fang/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/spf13/cobra"
	"golang.org/x/term"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func exitCodesHelp() string {
	codes := [][2]string{
		{"0", "ok"},
		{"1", "failed"},
		{"2", "usage"},
		{"3", "refused, or doctor found a problem"},
		{"130", "interrupted"},
	}
	room := 0
	if w, _, err := term.GetSize(int(os.Stdout.Fd())); err == nil {
		room = min(w, 120) - 10
	}
	indent := strings.Repeat(" ", 8)
	var b strings.Builder
	b.WriteString("Exit codes:")
	for _, c := range codes {
		desc := c[1]
		if room >= 12 {
			desc = strings.ReplaceAll(ansi.Wordwrap(desc, room, ""), "\n", "\n"+indent)
		}
		fmt.Fprintf(&b, "\n  %-6s%s", c[0], desc)
	}
	return b.String()
}

func Execute(version, commit string) int {
	if runtime.GOOS == "windows" {
		release.RemoveOldBinary()
	}
	app := &App{Version: version, Commit: commit, UI: ui.New(ui.Options{})}
	root := newRoot(app)
	root.SetOut(ui.NewHelpWriter(os.Stdout, groupRun, groupManage, groupOther))
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM, syscall.SIGHUP)
	defer stop()
	defer ui.QuietTerminal()()
	err := fang.Execute(ctx, root,
		fang.WithVersion(version),
		fang.WithCommit(commit),
		fang.WithColorSchemeFunc(ui.FangColorScheme),
		fang.WithErrorHandler(app.handleError),
	)
	return app.exitCode(ctx, err)
}

func newRoot(app *App) *cobra.Command {
	root := &cobra.Command{
		Use:           "rexctl",
		Short:         "Install and run your Fluxer instance",
		Long:          "Install and run your Fluxer instance.\n\n" + exitCodesHelp(),
		Args:          noArgs,
		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRunE: func(c *cobra.Command, _ []string) error {
			app.init(c)
			if c.Flags().Changed("dir") && strings.TrimSpace(app.Flags.Dir) == "" {
				return ui.Usage("Pass a folder to --dir.")
			}
			return nil
		},
		RunE: func(c *cobra.Command, _ []string) error {
			return runHome(c.Context(), app, c)
		},
	}
	root.SetFlagErrorFunc(func(c *cobra.Command, err error) error {
		return &ui.ExitError{
			Code: ui.ExitUsage,
			Msg:  usageSentence(err),
			Fix:  "Run " + c.CommandPath() + " --help to see every flag.",
		}
	})
	pf := root.PersistentFlags()
	pf.BoolP("help", "h", false, "show help for a command")
	root.Flags().BoolP("version", "v", false, "show the rexctl version")
	pf.StringVar(&app.Flags.Dir, "dir", "", "folder with your instance")
	pf.BoolVarP(&app.Flags.Yes, "yes", "y", false, "answer yes to every question and never prompt")
	pf.BoolVarP(&app.Flags.Quiet, "quiet", "q", false, "print only errors and results")
	pf.BoolVar(&app.Flags.Verbose, "verbose", false, "print every step and command")

	root.AddCommand(
		newInstallCmd(app),
		newSetupCmd(app),
		newStatusCmd(app),
		newDoctorCmd(app),
		newLogsCmd(app),
		newStartCmd(app),
		newStopCmd(app),
		newRestartCmd(app),
		newUpgradeCmd(app),
		newAutoUpgradeCmd(app),
		newRollbackCmd(app),
		newBackupCmd(app),
		newRestoreCmd(app),
		newUninstallCmd(app),
		newTakeoverCmd(app),
		newEjectCmd(app),
		newUseCmd(app),
		newProxyCmd(app),
		newUsersCmd(app),
		newAdminsCmd(app),
		newInvitesCmd(app),
		newRegistrationCmd(app),
		newEmailCmd(app),
		newBrandingCmd(app),
		newSearchCmd(app),
		newConfigCmd(app),
		newVersionCmd(app),
		newDocsCmd(app),
		newBundleCmd(app),
	)
	groupCommands(root)
	refuseUnknownSubcommands(root)
	return root
}

func refuseUnknownSubcommands(c *cobra.Command) {
	for _, sub := range c.Commands() {
		refuseUnknownSubcommands(sub)
	}
	if c.Runnable() || !c.HasSubCommands() {
		return
	}
	c.Args = noArgs
	c.RunE = func(c *cobra.Command, _ []string) error { return c.Help() }
	if c.Annotations == nil {
		c.Annotations = map[string]string{}
	}
	c.Annotations[helpOnly] = "true"
}

const helpOnly = "rexctl-help-only"

const (
	groupRun    = "Install and run"
	groupManage = "Manage your instance"
	groupOther  = "Other commands"
)

var commandGroups = map[string][]string{
	"run":    {"install", "setup", "status", "doctor", "logs", "start", "stop", "restart", "upgrade", "auto-upgrade", "rollback", "backup", "restore", "proxy", "takeover", "eject", "use", "uninstall"},
	"manage": {"users", "admins", "invites", "registration", "email", "branding", "search", "config"},
	"other":  {"version", "docs"},
}

func groupCommands(root *cobra.Command) {
	root.AddGroup(
		&cobra.Group{ID: "run", Title: groupRun},
		&cobra.Group{ID: "manage", Title: groupManage},
		&cobra.Group{ID: "other", Title: groupOther},
	)
	for id, names := range commandGroups {
		for _, name := range names {
			if c, _, err := root.Find([]string{name}); err == nil && c != root {
				c.GroupID = id
			}
		}
	}
	root.SetHelpCommandGroupID("other")
	root.SetCompletionCommandGroupID("other")
	root.InitDefaultHelpCmd()
	root.InitDefaultCompletionCmd()
	for _, c := range root.Commands() {
		switch c.Name() {
		case "help":
			c.Short = "Show help for a command"
		case "completion":
			c.Short = "Print a shell completion script"
			c.Long = "Print a script that completes rexctl commands in your shell."
			for _, sub := range c.Commands() {
				sub.Short = "Print the completion script for " + sub.Name()
			}
		}
	}
}

func (a *App) handleError(_ io.Writer, _ fang.Styles, err error) {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		a.UI.ReportError(ee)
		return
	}
	if errors.Is(err, context.Canceled) {
		return
	}
	if isUsageError(err) {
		a.UI.ReportError(&ui.ExitError{Code: ui.ExitUsage, Msg: usageSentence(err), Fix: "Run rexctl --help to see every command."})
		return
	}
	a.UI.ReportError(err)
}

func (a *App) exitCode(ctx context.Context, err error) int {
	if err == nil {
		return a.exit
	}
	if ctx.Err() != nil || errors.Is(err, context.Canceled) {
		return ui.ExitInterrupted
	}
	if isUsageError(err) {
		return ui.ExitUsage
	}
	return ui.CodeOf(err)
}

func isUsageError(err error) bool {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		return ee.Code == ui.ExitUsage
	}
	s := err.Error()
	for _, p := range []string{"unknown command", "unknown flag", "unknown shorthand", "invalid argument", "accepts ", "requires ", "flag needs", "required flag"} {
		if strings.HasPrefix(s, p) {
			return true
		}
	}
	return false
}

func usageSentence(err error) string {
	s, _, _ := strings.Cut(err.Error(), "\n")
	s = strings.TrimSpace(s)
	if !strings.HasSuffix(s, ".") {
		s += "."
	}
	return capitalise(s)
}

func capitalise(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}
