// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type VoiceServers interface {
	Voice
	Prepare(srv *instance.VoiceServer, pending []instance.VoiceServer) error
	Internal(srv *instance.VoiceServer) string
	Write(srv *instance.VoiceServer) error
	Start(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error
	Restart(ctx context.Context, srv *instance.VoiceServer) error
	Stale(srv *instance.VoiceServer, image string) bool
	State(ctx context.Context, srv *instance.VoiceServer) string
	Logs(ctx context.Context, w io.Writer, srv *instance.VoiceServer, o LogOptions) error
	Images() bool
	InUse(ctx context.Context) ([]string, []host.Port)
	IPv6Blocked(ctx context.Context, bindIP string) (string, string, error)
	CanChange() error
}

func VoiceOf(r Runtime) VoiceServers {
	return r.Voice().(VoiceServers)
}

func (c *compose) Voice() Voice  { return composeVoice{c} }
func (r *nativeRT) Voice() Voice { return nativeVoice{r} }

func localVoice(inst *instance.Instance) []instance.VoiceServer {
	return slices.Clone(inst.State.Voice)
}

func voiceServer(inst *instance.Instance, id string) (*instance.VoiceServer, error) {
	if srv := inst.State.VoiceServer(id); srv != nil {
		return srv, nil
	}
	return nil, ui.Refused(fmt.Sprintf("This machine doesn't run a voice server called %s.", id), "Run rexctl voice list to see the servers.")
}

func voiceStartFailed(srv *instance.VoiceServer, err error) error {
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	return ui.Wrap(err, fmt.Sprintf("The voice server %s didn't start.", srv.ID), fmt.Sprintf("Run rexctl voice logs %s to see why.", srv.ID))
}
