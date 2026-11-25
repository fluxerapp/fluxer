// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"io"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type containerVoice struct{ r *containerRT }

func (r *containerRT) Voice() Voice { return containerVoice{r} }

func extraVoiceInContainer() error {
	return unsupported("This instance runs in one container, which has its own voice server and no room for extra ones next to it.", "Run rexctl voice add with a voice server on another machine instead.")
}

func (v containerVoice) Local(ctx context.Context) ([]instance.VoiceServer, error) {
	return localVoice(v.r.inst), nil
}

func (containerVoice) Add(ctx context.Context, s *ui.Step, srv instance.VoiceServer) error {
	return extraVoiceInContainer()
}

func (containerVoice) Remove(ctx context.Context, s *ui.Step, id string) error {
	return extraVoiceInContainer()
}

func (containerVoice) StartAll(ctx context.Context, s *ui.Step) error { return nil }
func (containerVoice) StopAll(ctx context.Context, s *ui.Step) error  { return nil }

func (containerVoice) Prepare(srv *instance.VoiceServer, pending []instance.VoiceServer) error {
	return extraVoiceInContainer()
}

func (containerVoice) Internal(srv *instance.VoiceServer) string { return "" }

func (containerVoice) Write(srv *instance.VoiceServer) error { return extraVoiceInContainer() }

func (containerVoice) Start(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	return extraVoiceInContainer()
}

func (containerVoice) Restart(ctx context.Context, srv *instance.VoiceServer) error {
	return extraVoiceInContainer()
}

func (containerVoice) Stale(srv *instance.VoiceServer, image string) bool { return false }

func (containerVoice) State(ctx context.Context, srv *instance.VoiceServer) string {
	return "not running"
}

func (containerVoice) Logs(ctx context.Context, w io.Writer, srv *instance.VoiceServer, o LogOptions) error {
	return extraVoiceInContainer()
}

func (containerVoice) Images() bool { return false }

func (containerVoice) InUse(ctx context.Context) ([]string, []host.Port) { return nil, nil }

func (containerVoice) IPv6Blocked(ctx context.Context, bindIP string) (string, string, error) {
	return "", "", nil
}

func (containerVoice) CanChange() error { return extraVoiceInContainer() }
