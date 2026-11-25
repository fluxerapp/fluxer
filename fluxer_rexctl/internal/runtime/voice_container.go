// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"net"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	VoiceContainer         = "fluxer-voice"
	VoicePrevious          = VoiceContainer + previousSuffix
	VoiceVolume            = "fluxer-voice-data"
	VoiceStop              = 30 * time.Second
	voiceServersDir        = "voice"
	voiceServersTarget     = "/etc/fluxer/voice"
	labelRole              = "app.fluxer.role"
	roleVoice              = "voice"
	capNetBindService      = "NET_BIND_SERVICE"
	securityNoNewPrivilege = "no-new-privileges"
)

var voiceCaps = []string{"CHOWN", "DAC_OVERRIDE", "FOWNER", capNetBindService}

type voiceCtr struct {
	inst    *instance.Instance
	connect Connect
	eng     *engine.Engine
}

func newVoiceContainer(inst *instance.Instance, u *ui.UI, connect Connect) *nativeRT {
	r := newNative(inst, u)
	r.ctr = &voiceCtr{inst: inst, connect: connect}
	r.n.Dial, r.n.Host = r.ctr.dial, r.ctr.hostState
	return r
}

func VoiceStack(inst *instance.Instance, connect Connect) *native.Stack {
	if inst.State.Runtime == instance.RuntimeContainer {
		return newVoiceContainer(inst, nil, connect).n
	}
	return native.New(inst)
}

func VoiceEngine(ctx context.Context, inst *instance.Instance) (*engine.Engine, error) {
	pin := inst.State.Engine
	e, err := engine.Connect(ctx, engine.ConnectOptions{Host: pin.Host, ID: pin.ID, Prefer: engine.Kind(pin.Kind), AllowOld: true})
	if err != nil {
		return nil, EngineError(engine.Kind(pin.Kind), err)
	}
	if pin.Kind != "" && e.Kind != engine.Kind(pin.Kind) {
		return nil, ui.Refused(fmt.Sprintf("rexctl found %s on this machine, not %s.", e.Kind.Name(), engine.Kind(pin.Kind).Name()), "Start "+engine.Kind(pin.Kind).Name()+", then run the same command again.")
	}
	return e, nil
}

func (c *voiceCtr) engine(ctx context.Context) (*engine.Engine, error) {
	if c.eng != nil {
		return c.eng, nil
	}
	var e *engine.Engine
	var err error
	if c.connect != nil {
		e, err = c.connect(ctx)
	} else {
		e, err = VoiceEngine(ctx, c.inst)
	}
	if err != nil {
		return nil, err
	}
	c.eng = e
	return e, nil
}

func (c *voiceCtr) dial(ctx context.Context, target string) (net.Conn, error) {
	e, err := c.engine(ctx)
	if err != nil {
		return nil, err
	}
	return e.DialSingle(ctx, VoiceContainer, "fluxerd", "dial", target)
}

func (c *voiceCtr) hostState(ctx context.Context) (string, int) {
	e, err := c.engine(ctx)
	if err != nil {
		return "", 0
	}
	got, ok, err := e.Inspect(ctx, VoiceContainer)
	if err != nil || !ok {
		return "inactive", 0
	}
	return unitState(got), got.RestartCount
}

func VoiceContainerSpec(inst *instance.Instance, version string, env map[string]string) engine.SingleSpec {
	var vars []string
	for _, k := range slices.Sorted(maps.Keys(env)) {
		vars = append(vars, k+"="+env[k])
	}
	s := engine.SingleSpec{
		Name:         VoiceContainer,
		Image:        ImageRef(version),
		Cmd:          []string{"run", "--voice-only"},
		Env:          vars,
		User:         "0:0",
		Binds:        []string{inst.Path(voiceServersDir) + ":" + voiceServersTarget + ":ro,z"},
		Volume:       VoiceVolume,
		VolumeTarget: ContainerData,
		HostNetwork:  true,
		CapDrop:      []string{"ALL"},
		CapAdd:       voiceCaps,
		SecurityOpt:  []string{securityNoNewPrivilege},
		Labels:       map[string]string{LabelManagedBy: ManagedByRexctl, LabelVersion: version, labelRole: roleVoice},
		StopTimeout:  int(VoiceStop / time.Second),
		Restart:      restartPolicy(inst),
	}
	s.Labels[labelSpec] = specHash(s)
	return s
}

func VoiceContainerCurrent(have engine.Single, want engine.SingleSpec) bool {
	return have.Running() && have.Labels[labelSpec] == want.Labels[labelSpec]
}

type WentBack struct{ Err error }

func (w *WentBack) Error() string { return w.Err.Error() }

func (w *WentBack) Unwrap() error { return w.Err }

func EnsureVoiceContainer(ctx context.Context, e containerEngine, spec engine.SingleSpec, seed, wait func(context.Context) error) error {
	keep := context.WithoutCancel(ctx)
	have, ok, err := e.Inspect(ctx, spec.Name)
	if err != nil {
		return ContainerError(err, "Couldn't read the state of the voice container.")
	}
	if ok && VoiceContainerCurrent(have, spec) {
		return wait(ctx)
	}
	if err := e.RemoveSingle(keep, VoicePrevious); err != nil {
		return ContainerError(err, "Couldn't remove the voice container from before.")
	}
	if ok {
		if err := e.StopSingle(keep, spec.Name, VoiceStop); err != nil {
			return ContainerError(err, "Couldn't stop the voice container.")
		}
		if err := e.RenameSingle(keep, spec.Name, VoicePrevious); err != nil {
			return ContainerError(err, "Couldn't keep the voice container for going back.")
		}
	}
	err = startVoiceContainer(ctx, e, spec, seed, wait)
	if err == nil {
		if err := e.RemoveSingle(keep, VoicePrevious); err != nil {
			return ContainerError(err, "Couldn't remove the voice container from before.")
		}
		return nil
	}
	_ = e.StopSingle(keep, spec.Name, VoiceStop)
	_ = e.RemoveSingle(keep, spec.Name)
	if !ok {
		return err
	}
	back := e.RenameSingle(keep, VoicePrevious, spec.Name)
	if back == nil {
		back = e.StartSingle(keep, spec.Name)
	}
	if back == nil {
		back = wait(keep)
	}
	if back != nil {
		return errors.Join(err, back)
	}
	return &WentBack{Err: err}
}

func startVoiceContainer(ctx context.Context, e containerEngine, spec engine.SingleSpec, seed, wait func(context.Context) error) error {
	if err := e.CreateSingle(ctx, spec); err != nil {
		return ContainerError(err, "Couldn't create the voice container.")
	}
	if seed != nil {
		if err := seed(ctx); err != nil {
			return err
		}
	}
	if err := e.StartSingle(ctx, spec.Name); err != nil {
		return ContainerError(err, "Couldn't start the voice container.")
	}
	return wait(ctx)
}

func RemoveVoiceContainer(ctx context.Context, e containerEngine, data bool) error {
	keep := context.WithoutCancel(ctx)
	for _, name := range []string{VoiceContainer, VoicePrevious} {
		if err := e.StopSingle(keep, name, VoiceStop); err != nil {
			return ContainerError(err, "Couldn't stop the voice container.")
		}
		if err := e.RemoveSingle(keep, name); err != nil {
			return ContainerError(err, "Couldn't remove the voice container.")
		}
	}
	if !data {
		return nil
	}
	if err := e.RemoveVolume(keep, VoiceVolume); err != nil {
		return ContainerError(err, "Couldn't remove the volume "+VoiceVolume+".")
	}
	return nil
}
