// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/children"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/updates"
)

const (
	usersUsage        = "usage: fluxerd users create <username> [--email e] [--admin] | reset-link <user>"
	userFlagDeleted   = uint64(1) << 34
	userPageSize      = 100
	userSearchPages   = 50
	usernameSignInErr = "USERNAME_SIGN_IN_ONLY"
)

type adminUser struct {
	ID       string   `json:"id"`
	Username string   `json:"username"`
	Email    *string  `json:"email"`
	Flags    string   `json:"flags"`
	ACLs     []string `json:"acls"`
}

func (u adminUser) deleted() bool {
	flags, _ := strconv.ParseUint(u.Flags, 10, 64)
	return flags&userFlagDeleted != 0
}

type resetLink struct {
	URL       string    `json:"url"`
	ExpiresAt time.Time `json:"expires_at"`
}

type adminCaller interface {
	call(ctx context.Context, method, path string, in, out any) error
}

func cmdUsers(args []string) error {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, usersUsage)
		return exitError(2)
	}
	switch args[0] {
	case "create":
		return cmdUsersCreate(args[1:])
	case "reset-link":
		return cmdUsersResetLink(args[1:])
	}
	fmt.Fprintln(os.Stderr, usersUsage)
	return exitError(2)
}

func splitArgs(fs *flag.FlagSet, args []string) ([]string, error) {
	var positional []string
	for {
		if err := fs.Parse(args); err != nil {
			return nil, err
		}
		args = fs.Args()
		if len(args) == 0 {
			return positional, nil
		}
		positional = append(positional, args[0])
		args = args[1:]
	}
}

func usersOperator(cf *configFlags, instead string) (operator, error) {
	cfg, err := cf.load()
	if err != nil {
		return operator{}, err
	}
	if voiceOnly(cfg) {
		return operator{}, errors.New("run this on the Fluxer machine, not on a voice server")
	}
	op := operatorClient(cfg.Layout.OperatorSocket())
	op.place = cf.runtime
	if updates.DetectMethod(cfg.Env[spec.InstallMethodEnv], "", "") == updates.MethodRexctl {
		op.instead = children.RexctlCommand(cfg.Env, instead)
	}
	return op, nil
}

func cmdUsersCreate(args []string) error {
	fs := flag.NewFlagSet("users create", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	email := fs.String("email", "", "email address for the account")
	admin := fs.Bool("admin", false, "make the account an admin")
	rest, err := splitArgs(fs, args)
	if err != nil {
		return exitError(2)
	}
	if len(rest) != 1 || strings.TrimSpace(rest[0]) == "" {
		fmt.Fprintln(os.Stderr, "usage: fluxerd users create <username> [--email e] [--admin]")
		return exitError(2)
	}
	instead := []string{"users", "create", strings.TrimSpace(rest[0])}
	if strings.TrimSpace(*email) != "" {
		instead = append(instead, "--email", strings.TrimSpace(*email))
	}
	if *admin {
		instead = append(instead, "--admin")
	}
	op, err := usersOperator(cf, strings.Join(instead, " "))
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	return createUser(ctx, op, os.Stdout, os.Stderr, strings.TrimSpace(rest[0]), strings.TrimSpace(*email), *admin)
}

func createUser(ctx context.Context, op adminCaller, stdout, stderr io.Writer, username, email string, admin bool) error {
	body := map[string]any{"username": username}
	if email != "" {
		body["email"] = email
	}
	if admin {
		body["admin"] = true
	}
	var created struct {
		User          adminUser `json:"user"`
		PasswordReset resetLink `json:"password_reset"`
	}
	err := op.call(ctx, http.MethodPost, "/admin/users", body, &created)
	var opErr *operatorError
	if email != "" && errors.As(err, &opErr) && opErr.Code == usernameSignInErr {
		return errors.New("people on this instance sign in with a username and don't have an email address on their account. Run the command again without --email")
	}
	if err != nil {
		return err
	}
	if admin {
		fmt.Fprintf(stderr, "Created %s as an admin.\n", created.User.Username)
	} else {
		fmt.Fprintf(stderr, "Created %s.\n", created.User.Username)
	}
	if created.PasswordReset.URL != "" {
		fmt.Fprintf(stderr, "Send them this link to set a password. It works once, until %s.\n", linkExpiry(created.PasswordReset.ExpiresAt))
		fmt.Fprintln(stdout, created.PasswordReset.URL)
	}
	return nil
}

func cmdUsersResetLink(args []string) error {
	fs := flag.NewFlagSet("users reset-link", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	rest, err := splitArgs(fs, args)
	if err != nil {
		return exitError(2)
	}
	if len(rest) != 1 {
		fmt.Fprintln(os.Stderr, "usage: fluxerd users reset-link <username, email address or user ID>")
		return exitError(2)
	}
	op, err := usersOperator(cf, "users reset-link "+strings.TrimSpace(rest[0]))
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	return makeResetLink(ctx, op, os.Stdout, os.Stderr, rest[0])
}

func makeResetLink(ctx context.Context, op adminCaller, stdout, stderr io.Writer, ref string) error {
	u, err := resolveUser(ctx, op, ref)
	if err != nil {
		return err
	}
	var link resetLink
	err = op.call(ctx, http.MethodPost, "/admin/users/"+url.PathEscape(u.ID)+"/password-reset-link", nil, &link)
	var opErr *operatorError
	if errors.As(err, &opErr) && opErr.Code == usernameSignInErr {
		return errors.New("people on this instance reset their password by email from the sign-in page. If email has stopped working, turn off email delivery in the admin panel, then try again")
	}
	if err != nil {
		return err
	}
	fmt.Fprintf(stderr, "Made a link for %s to set a new password. It works once, until %s.\n", u.Username, linkExpiry(link.ExpiresAt))
	fmt.Fprintln(stdout, link.URL)
	return nil
}

func linkExpiry(t time.Time) string {
	return t.Local().Format("15:04 MST on 2 January")
}

func resolveUser(ctx context.Context, op adminCaller, ref string) (adminUser, error) {
	ref = strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(ref), "@"))
	if ref == "" {
		return adminUser{}, errors.New("pass a username, email address or user ID")
	}
	notFound := fmt.Errorf("no account matches %q", ref)
	if _, err := strconv.ParseUint(ref, 10, 64); err == nil {
		var resp struct {
			Users []adminUser `json:"users"`
		}
		err := op.call(ctx, http.MethodGet, "/admin/users/"+url.PathEscape(ref), nil, &resp)
		var opErr *operatorError
		switch {
		case err == nil && len(resp.Users) > 0 && !resp.Users[0].deleted():
			return resp.Users[0], nil
		case err != nil && !(errors.As(err, &opErr) && opErr.StatusCode == http.StatusNotFound):
			return adminUser{}, err
		}
	}
	query := url.Values{"limit": {strconv.Itoa(userPageSize)}}
	byName := false
	switch {
	case strings.Contains(ref, "@"):
		query.Set("email", ref)
	case strings.Contains(ref, "#"):
		query.Set("resolve", ref)
	default:
		query.Set("q", ref)
		byName = true
	}
	var matches []adminUser
	for page := 0; page < userSearchPages; page++ {
		query.Set("offset", strconv.Itoa(page*userPageSize))
		var resp struct {
			Users []adminUser `json:"users"`
		}
		if err := op.call(ctx, http.MethodGet, "/admin/users?"+query.Encode(), nil, &resp); err != nil {
			return adminUser{}, err
		}
		for _, u := range resp.Users {
			if u.deleted() || (byName && !strings.EqualFold(u.Username, ref)) {
				continue
			}
			matches = append(matches, u)
		}
		if !byName || len(resp.Users) < userPageSize {
			break
		}
	}
	switch len(matches) {
	case 0:
		return adminUser{}, notFound
	case 1:
		return matches[0], nil
	}
	ids := make([]string, len(matches))
	for i, m := range matches {
		ids[i] = m.ID
	}
	return adminUser{}, fmt.Errorf("%d accounts match %q. Pass one of these user IDs instead: %s", len(matches), ref, strings.Join(ids, ", "))
}
