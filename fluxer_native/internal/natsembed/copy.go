// SPDX-License-Identifier: AGPL-3.0-or-later

package natsembed

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"strings"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/nats.go/jetstream"
)

type Copied struct {
	Streams  int
	Messages int
}

func CopyStreams(ctx context.Context, src, dst *nats.Conn) (Copied, error) {
	from, err := jetstream.New(src)
	if err != nil {
		return Copied{}, err
	}
	to, err := jetstream.New(dst)
	if err != nil {
		return Copied{}, err
	}
	lister := from.StreamNames(ctx)
	var names []string
	for name := range lister.Name() {
		names = append(names, name)
	}
	if err := lister.Err(); err != nil {
		return Copied{}, fmt.Errorf("nats: listing streams: %w", err)
	}
	var c Copied
	for _, name := range names {
		n, err := copyStream(ctx, from, to, name)
		if err != nil {
			return c, fmt.Errorf("nats: stream %s: %w", name, err)
		}
		c.Streams++
		c.Messages += n
	}
	return c, nil
}

func copyStream(ctx context.Context, from, to jetstream.JetStream, name string) (int, error) {
	s, err := from.Stream(ctx, name)
	if err != nil {
		return 0, err
	}
	info := s.CachedInfo()
	cfg := info.Config
	cfg.Metadata = maps.Clone(cfg.Metadata)
	maps.DeleteFunc(cfg.Metadata, func(k, _ string) bool { return strings.HasPrefix(k, "_nats.") })
	if _, err := to.CreateStream(ctx, cfg); err != nil {
		return 0, err
	}
	n := 0
	for seq := info.State.FirstSeq; info.State.Msgs > 0 && seq <= info.State.LastSeq; {
		m, err := s.GetMsg(ctx, seq, jetstream.WithGetMsgSubject(">"))
		if errors.Is(err, jetstream.ErrMsgNotFound) {
			break
		}
		if err != nil {
			return n, err
		}
		h := maps.Clone(m.Header)
		h.Del(nats.MsgIdHdr)
		ack, err := to.PublishMsg(ctx, &nats.Msg{Subject: m.Subject, Header: h, Data: m.Data}, jetstream.WithExpectStream(name))
		if err != nil {
			return n, err
		}
		if ack.Duplicate {
			return n, fmt.Errorf("message %d was taken for a duplicate", m.Sequence)
		}
		seq = m.Sequence + 1
		n++
	}
	return n, nil
}
