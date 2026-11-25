// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/url"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

const (
	postgresReachTimeout = 10 * time.Second
	psqlConnectionFailed = 2
)

var ErrNoImage = errors.New("runtime: the image isn't on this machine")

type PostgresUnreachable struct {
	Addr    string
	Reason  string
	Because string
}

func (e *PostgresUnreachable) Error() string {
	return "runtime: " + e.Addr + " is unreachable: " + e.Reason
}

func (e *PostgresUnreachable) Sentence(who string) string {
	return who + " can't connect to your PostgreSQL database at " + e.Addr + ", because " + e.Because + "."
}

func unreachable(t postgresTarget, reason string) error {
	r := strings.ToLower(reason)
	because := ""
	switch {
	case strings.Contains(r, "could not translate host name"), strings.Contains(r, "no such host"), strings.Contains(r, "name resolution"):
		because = "the name " + t.host + " doesn't resolve"
	case strings.Contains(r, "password authentication failed"), strings.Contains(r, "no pg_hba.conf entry"):
		because = "the database turns away connections from it"
	case strings.Contains(r, "refused"):
		because = "nothing listens at that address"
	case strings.Contains(r, "timeout"), strings.Contains(r, "timed out"), strings.Contains(r, "no route"), strings.Contains(r, "unreachable"):
		because = "the connection times out"
	}
	if because == "" {
		return fmt.Errorf("runtime: connecting to %s: %s", t.addr(), reason)
	}
	return &PostgresUnreachable{Addr: t.addr(), Reason: reason, Because: because}
}

type postgresTarget struct {
	host, port, user, password, database, sslmode string
}

func parsePostgresURL(raw string) (postgresTarget, bool) {
	u, err := url.Parse(raw)
	if err != nil || (u.Scheme != "postgres" && u.Scheme != "postgresql") || u.Hostname() == "" || strings.Contains(u.Host, ",") {
		return postgresTarget{}, false
	}
	t := postgresTarget{host: u.Hostname(), port: u.Port(), user: u.User.Username(), database: strings.TrimPrefix(u.Path, "/")}
	if t.port == "" {
		t.port = "5432"
	}
	t.password, _ = u.User.Password()
	switch mode := u.Query().Get("sslmode"); mode {
	case "verify-ca", "verify-full", "no-verify":
		t.sslmode = "require"
	default:
		t.sslmode = mode
	}
	return t, true
}

func (t postgresTarget) addr() string { return net.JoinHostPort(t.host, t.port) }

func (t postgresTarget) env() []string {
	env := []string{"PGHOST=" + t.host, "PGPORT=" + t.port, "PGCONNECT_TIMEOUT=" + fmt.Sprint(int(postgresReachTimeout.Seconds()))}
	for k, v := range map[string]string{"PGUSER": t.user, "PGPASSWORD": t.password, "PGDATABASE": t.database, "PGSSLMODE": t.sslmode} {
		if v != "" {
			env = append(env, k+"="+v)
		}
	}
	return env
}

type postgresProbe interface {
	imageSource
	RunOnce(ctx context.Context, s engine.SingleSpec, stdin io.Reader, stdout, stderr io.Writer) error
}

func CheckPostgresFromContainer(ctx context.Context, e postgresProbe, version, raw string) error {
	t, ok := parsePostgresURL(raw)
	if !ok {
		return nil
	}
	ref := ImageRef(version)
	_, here, err := e.LocalImage(ctx, ref)
	switch {
	case err != nil:
		return err
	case !here:
		return fmt.Errorf("%w: %s", ErrNoImage, ref)
	}
	if err := checkDigest(ctx, e, version); err != nil {
		return err
	}
	spec := engine.SingleSpec{
		Image:      ref,
		Entrypoint: []string{"psql"},
		Cmd:        []string{"-w", "-X", "-A", "-t", "-q", "-c", "select 1"},
		Env:        t.env(),
		Labels:     map[string]string{LabelManagedBy: ManagedByRexctl},
	}
	err = e.RunOnce(ctx, spec, nil, io.Discard, io.Discard)
	var exit *engine.ExitError
	if errors.As(err, &exit) && exit.Code == psqlConnectionFailed {
		return unreachable(t, strings.TrimSpace(exit.Stderr))
	}
	return err
}

func CheckPostgresFromHost(ctx context.Context, raw string) error {
	t, ok := parsePostgresURL(raw)
	if !ok {
		return nil
	}
	d := net.Dialer{Timeout: postgresReachTimeout}
	c, err := d.DialContext(ctx, "tcp", t.addr())
	if ctx.Err() != nil {
		return ctx.Err()
	}
	if err != nil {
		return unreachable(t, err.Error())
	}
	return c.Close()
}
