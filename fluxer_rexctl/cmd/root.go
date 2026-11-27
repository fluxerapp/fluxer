// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"runtime"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func Execute(version, commit string) int {
	if runtime.GOOS == "windows" {
		release.RemoveOldBinary()
	}
	os.Args = append(os.Args[:1:1], takeSudoEnv(os.Args[1:], ranWithSudo(), os.Setenv)...)
	ui.PrepareConsole(os.Stdout, os.Stderr)
	app := &App{Version: version, Commit: commit, UI: ui.New(ui.Options{}), errorFile: os.Getenv(childErrorEnv)}
	_ = os.Unsetenv(childErrorEnv)
	root := newRoot(app)
	root.Version = versionString(version, commit)
	ctx, stop := ui.SignalContext()
	defer stop()
	err := root.ExecuteContext(ctx)
	if err != nil {
		app.handleError(err)
	}
	code := app.exitCode(ctx, err)
	app.UI.End(code)
	return code
}

func versionString(version, commit string) string {
	if len(commit) >= 7 {
		return version + " (" + commit[:7] + ")"
	}
	return version
}

type displayFlags struct {
	noColor     bool
	plain       bool
	accessible  bool
	noRex       bool
	noAnimation bool
	progress    string
}

func (d displayFlags) apply(u *ui.UI) error {
	progress := ui.ProgressFormat(d.progress)
	switch progress {
	case ui.ProgressAuto, ui.ProgressPlain, ui.ProgressJSON:
	default:
		return &ui.ExitError{
			Code: ui.ExitUsage,
			Msg:  fmt.Sprintf("--progress can't be %q.", d.progress),
			Fix:  "Pass --progress auto, --progress plain or --progress json.",
		}
	}
	u.SetDisplay(ui.Display{
		NoColor:     d.noColor,
		Plain:       d.plain,
		Accessible:  d.accessible,
		NoRex:       d.noRex,
		NoAnimation: d.noAnimation,
		Progress:    progress,
	})
	return nil
}

func (d *displayFlags) scan(c *cobra.Command, args []string, jsonOut *bool) {
	bools := map[string]*bool{
		"no-color":     &d.noColor,
		"plain":        &d.plain,
		"accessible":   &d.accessible,
		"no-rex":       &d.noRex,
		"no-animation": &d.noAnimation,
	}
	if c.Flags().Lookup("json") != nil {
		bools["json"] = jsonOut
	}
	for i, a := range args {
		if a == "--" {
			return
		}
		name, val, hasVal := strings.Cut(strings.TrimPrefix(a, "--"), "=")
		if !strings.HasPrefix(a, "--") {
			continue
		}
		if p, ok := bools[name]; ok && (!hasVal || val == "true") {
			*p = true
		}
		if name != "progress" {
			continue
		}
		switch {
		case hasVal:
			d.progress = val
		case i+1 < len(args):
			d.progress = args[i+1]
		}
	}
}

func newRoot(app *App) *cobra.Command {
	var display displayFlags
	root := &cobra.Command{
		Use:           "rexctl",
		Short:         "Install and run your Fluxer instance",
		Long:          "Install and run your Fluxer instance.",
		Args:          noArgs,
		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRunE: func(c *cobra.Command, _ []string) error {
			derr := display.apply(app.UI)
			app.init(c)
			app.UI.Begin(c.CommandPath())
			if derr != nil {
				return derr
			}
			if c.Flags().Changed("dir") && strings.TrimSpace(app.Flags.Dir) == "" {
				return ui.Usage("Pass a folder to --dir.")
			}
			return nil
		},
		RunE: func(c *cobra.Command, _ []string) error {
			return runHome(c.Context(), app, c)
		},
	}
	root.SetHelpFunc(func(c *cobra.Command, _ []string) {
		_ = display.apply(app.UI)
		app.UI.Help(c)
	})
	root.SetFlagErrorFunc(func(c *cobra.Command, err error) error {
		display.scan(c, os.Args[1:], &app.Flags.JSON)
		_ = display.apply(app.UI)
		app.init(c)
		app.UI.Begin(c.CommandPath())
		return &ui.ExitError{
			Code: ui.ExitUsage,
			Msg:  flagErrorText(err),
			Fix:  "Run " + c.CommandPath() + " --help to see every flag.",
		}
	})
	pf := root.PersistentFlags()
	pf.BoolP("help", "h", false, "show help for a command")
	root.Flags().BoolP("version", "v", false, "show the rexctl version")
	pf.StringVar(&app.Flags.Dir, "dir", "", "folder with your instance")
	pf.BoolVarP(&app.Flags.Yes, "yes", "y", false, "answer yes to every question and never prompt")
	pf.BoolVarP(&app.Flags.Quiet, "quiet", "q", false, "print only errors and results")
	pf.BoolVar(&app.Flags.Verbose, "verbose", false, "print every step and command, and the full error when one fails")
	pf.BoolVar(&display.noColor, "no-color", false, "print without colour")
	pf.BoolVar(&display.plain, "plain", false, "print plain lines with no animation or prompts")
	pf.BoolVar(&display.accessible, "accessible", false, "print for screen readers and ask questions as numbered lists")
	pf.BoolVar(&display.noRex, "no-rex", false, "hide Rex")
	pf.BoolVar(&display.noAnimation, "no-animation", false, "keep the layout but stop anything moving")
	pf.StringVar(&display.progress, "progress", string(ui.ProgressAuto), "how to show progress on stderr, auto, plain or json")

	root.AddCommand(
		newInstallCmd(app),
		newSetupCmd(app),
		newStatusCmd(app),
		newDoctorCmd(app),
		newLogsCmd(app),
		newStartCmd(app),
		newStopCmd(app),
		newDownCmd(app),
		newRestartCmd(app),
		newUpgradeCmd(app),
		newMigrateCmd(app),
		newAutoUpgradeCmd(app),
		newBootCmd(app),
		newRollbackCmd(app),
		newBackupCmd(app),
		newRestoreCmd(app),
		newUninstallCmd(app),
		newDetachCmd(app),
		newUseCmd(app),
		newProxyCmd(app),
		newUsersCmd(app),
		newAdminsCmd(app),
		newInvitesCmd(app),
		newVoiceCmd(app),
		newRegistrationCmd(app),
		newEmailCmd(app),
		newBrandingCmd(app),
		newSearchCmd(app),
		newConfigCmd(app),
		newPostgresCmd(app),
		newVersionCmd(app),
		newDocsCmd(app),
	)
	groupCommands(root)
	refuseUnknownSubcommands(root)
	reportErrors(app, root)
	return root
}

func reportErrors(app *App, c *cobra.Command) {
	for _, sub := range c.Commands() {
		reportErrors(app, sub)
	}
	run := c.RunE
	if run == nil {
		return
	}
	c.RunE = func(cmd *cobra.Command, args []string) error {
		err := run(cmd, args)
		if err == nil || errors.Is(err, errDeclined) {
			return nil
		}
		app.handleError(err)
		app.SetExit(app.exitCode(cmd.Context(), err))
		return nil
	}
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
	groupEveryday = "Everyday"
	groupSetUp    = "Set up"
	groupManage   = "Manage your instance"
	groupAdvanced = "Advanced"
	groupOther    = "Other commands"
)

var commandGroups = map[string][]string{
	"everyday": {"status", "logs", "start", "stop", "restart", "upgrade", "backup", "restore"},
	"setup":    {"install", "setup", "proxy", "boot", "auto-upgrade", "use"},
	"manage":   {"users", "admins", "invites", "voice", "registration", "email", "branding", "search", "config"},
	"advanced": {"doctor", "rollback", "migrate", "postgres", "down", "detach", "uninstall"},
	"other":    {"version", "docs"},
}

func groupCommands(root *cobra.Command) {
	root.AddGroup(
		&cobra.Group{ID: "everyday", Title: groupEveryday},
		&cobra.Group{ID: "setup", Title: groupSetUp},
		&cobra.Group{ID: "manage", Title: groupManage},
		&cobra.Group{ID: "advanced", Title: groupAdvanced},
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
			c.Args = noArgs
			for _, sub := range c.Commands() {
				sub.Short = "Print the completion script for " + sub.Name()
				sub.Args = noArgs
			}
		}
	}
}

const childErrorEnv = "REXCTL_ERROR_FILE"

type childError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
	Fix     string `json:"fix"`
	err     error
}

func (e *childError) Error() string { return e.err.Error() }

func (e *childError) Unwrap() error { return e.err }

func (a *App) handOver(err error) {
	if a.errorFile == "" || err == nil || errors.Is(err, context.Canceled) {
		return
	}
	c := childError{Code: ui.CodeOf(err), Message: ui.Sentence(err.Error())}
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		c.Message, c.Fix = ee.Msg, ee.Fix
		if c.Message == "" && ee.Err != nil {
			c.Message = ui.Sentence(ee.Err.Error())
		}
	}
	if c.Message == "" {
		return
	}
	if data, jerr := json.Marshal(c); jerr == nil {
		_ = os.WriteFile(a.errorFile, data, 0o600)
	}
}

func childFailure(err error, report string) error {
	if err == nil || report == "" {
		return err
	}
	data, rerr := os.ReadFile(report)
	c := &childError{err: err}
	if rerr != nil || json.Unmarshal(data, c) != nil || c.Message == "" {
		return err
	}
	return c
}

func (a *App) handleError(err error) {
	a.handOver(err)
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		a.UI.ReportError(ee)
		return
	}
	if errors.Is(err, context.Canceled) {
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
	return ui.CodeOf(err)
}
