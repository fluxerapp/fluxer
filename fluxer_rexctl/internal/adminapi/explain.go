// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func Explain(err error) error {
	if err == nil {
		return nil
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	var relay *RelayError
	var api *APIError
	var notFound *UserNotFoundError
	var ambiguous *AmbiguousUserError
	var invite *InviteNotFoundError
	var pending *PendingNotFoundError
	var unknown *UnknownKeyError
	var invalid *InvalidValueError
	var banBlocks *BanBlocksDeletionError
	switch {
	case errors.Is(err, ErrSchemaMismatch):
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "This rexctl and the Fluxer running here don't match.", Fix: "Run rexctl upgrade.", Err: err}
	case errors.Is(err, ErrNoFullReindex):
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "The Fluxer release running here can't rebuild search in one go.", Fix: `Run rexctl upgrade, or use the "Reindex" buttons on the admin panel's search index page and "Refresh Channel Messages" on each community's page there.`, Err: err}
	case errors.Is(err, ErrSearchOff):
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Search is turned off on this instance.", Fix: "Run rexctl doctor to see what's wrong.", Err: err}
	case errors.Is(err, ErrNoRelay):
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "The Fluxer release running here can't take commands from rexctl.", Fix: "Run rexctl upgrade.", Err: err}
	case errors.As(err, &relay):
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Couldn't connect to Fluxer on this machine.", Fix: "Run rexctl status to check that it's running.", Err: err}
	case errors.As(err, &notFound):
		return ui.Failed(fmt.Sprintf("There's no account called %s.", notFound.Ref), "Run rexctl users list --search to find it.")
	case errors.As(err, &ambiguous):
		ids := make([]string, 0, len(ambiguous.Matches))
		for _, m := range ambiguous.Matches {
			ids = append(ids, m.ID)
		}
		return ui.Failed(fmt.Sprintf("%d accounts are called %s.", len(ambiguous.Matches), ambiguous.Ref), fmt.Sprintf("Pass a user ID. Theirs are %s.", ui.JoinAnd(ids)))
	case errors.As(err, &banBlocks):
		return ui.Refused(fmt.Sprintf("%s is banned for a reason that doesn't allow closing the account.", banBlocks.Username), fmt.Sprintf("Run rexctl users unban %s, then delete the account.", banBlocks.Username))
	case errors.As(err, &invite):
		return ui.Failed(fmt.Sprintf("There's no invite with the ID %s.", invite.ID), "Run rexctl invites list to see your invites.")
	case errors.As(err, &pending):
		return ui.Failed(fmt.Sprintf("Nobody waiting for approval has the ID %s.", pending.ID), "Run rexctl registration pending to see who is waiting.")
	case errors.As(err, &unknown):
		fix := "Run rexctl config list to see every setting."
		if len(unknown.Similar) > 0 && len(unknown.Similar) <= 3 {
			fix = "Did you mean " + ui.JoinOr(unknown.Similar) + "?"
		}
		return &ui.ExitError{Code: ui.ExitUsage, Msg: fmt.Sprintf("There's no setting called %s.", unknown.Key), Fix: fix}
	case errors.As(err, &invalid):
		return ui.Usage(fmt.Sprintf("%s needs %s.", invalid.Key.Key, invalid.Want))
	case errors.As(err, &api):
		return explainAPI(api)
	}
	return ui.Wrap(err, "Something went wrong talking to Fluxer.", "Run rexctl logs api to see why.")
}

func sentence(s string) string {
	s = strings.TrimSpace(s)
	if s == "" {
		return s
	}
	if !strings.HasSuffix(s, ".") && !strings.HasSuffix(s, "?") && !strings.HasSuffix(s, "!") {
		s += "."
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func explainAPI(e *APIError) error {
	switch {
	case e.Status == http.StatusNotFound && e.Code == "":
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "The Fluxer running here can't do this yet.", Fix: "Run rexctl upgrade.", Err: e}
	case e.Status == http.StatusUnauthorized || e.Status == http.StatusForbidden:
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "Fluxer turned down the request.", Fix: "Run rexctl upgrade, then try again.", Err: e}
	case e.Status == http.StatusTooManyRequests:
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Fluxer is limiting how fast it takes these requests.", Fix: "Wait a minute, then try again.", Err: e}
	case e.Status >= 500:
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Fluxer ran into an error.", Fix: "Run rexctl logs api to see why.", Err: e}
	}
	msg := sentence(e.Error())
	if msg == "" {
		msg = "Fluxer didn't accept that."
	}
	return &ui.ExitError{Code: ui.ExitFailed, Msg: msg, Fix: fieldFix(e.Fields), Err: e}
}

const SSOPrivateFix = "If the provider is on your own network, run rexctl config set sso.allow_private_addresses true, then try again."

var ssoURLFields = []string{"issuer", "authorization_url", "token_url", "user_info_url", "jwks_url"}

func fieldFix(fields []FieldError) string {
	for _, f := range fields {
		if f.Code == "URL_NOT_PUBLICLY_ROUTABLE" && slices.Contains(ssoURLFields, f.Path) {
			return SSOPrivateFix
		}
	}
	return ""
}
