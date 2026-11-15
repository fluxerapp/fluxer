// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func init() {
	Register(Check{ID: "attachment-signatures", Phases: Installed, Run: runAttachmentSignatures, Fix: fixAttachmentSignatures})
}

func runAttachmentSignatures(ctx context.Context, e *Env) Result {
	st := e.state()
	if st == nil || !settings.SignsAttachments(e.value(settings.SignatureModeVar)) {
		return skip("")
	}
	if e.value(settings.SignatureSecretsVar) != "" {
		return pass("Attachment links are signed.")
	}
	msg := "Signed attachments are on and Fluxer has no key to sign them with. The media proxy won't start until it has one."
	if _, chosen := st.Settings[settings.SignatureSecretsVar]; chosen {
		return fail(msg, "Run rexctl config unset "+settings.SignatureSecretsVar+" and rexctl makes a key.")
	}
	return fixable(fail(msg, "Run rexctl doctor --fix to make a key."))
}

func fixAttachmentSignatures(ctx context.Context, e *Env) error {
	if e.state() == nil {
		return errNoProject
	}
	key, err := secrets.Base64(32)
	if err != nil {
		return err
	}
	e.stageSecret(settings.SignatureSecretsVar, key)
	return nil
}
