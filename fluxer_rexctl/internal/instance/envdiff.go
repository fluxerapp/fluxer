// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type ChangeKind int

const (
	ChangeSetting ChangeKind = iota
	ChangeSecret
	ChangeAdmin
)

type EnvChange struct {
	Var    string
	Key    string
	Value  string
	Kind   ChangeKind
	Secret bool
}

func (c EnvChange) Command() string {
	if c.Secret {
		return "rexctl config set " + c.Key + " -"
	}
	return "rexctl config set " + c.Key + " " + shellQuote(c.Value)
}

func shellQuote(v string) string {
	if v == "" {
		return "''"
	}
	safe := true
	for _, r := range v {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
		case strings.ContainsRune("-_.,:/@+=%", r):
		default:
			safe = false
		}
	}
	if safe {
		return v
	}
	return "'" + strings.ReplaceAll(v, "'", `'\''`) + "'"
}

func compareEnv(set *stack.Set, custom []byte, s *State, env map[string]string) ([]EnvChange, []string, error) {
	eff, err := effectiveFor(set, s)
	if err != nil {
		return nil, nil, err
	}
	read, err := settings.Vars(set, custom)
	if err != nil {
		return nil, nil, err
	}
	defaults := composeDefaults(setFiles(set, custom))
	named := map[string]settings.Key{}
	for _, k := range settings.Named() {
		if len(k.Vars) > 0 {
			named[k.Vars[0]] = k
		}
	}
	proxies := map[string]string{"HTTP_PROXY": settings.HTTPProxy, "HTTPS_PROXY": settings.HTTPSProxy, "NO_PROXY": settings.NoProxy}
	admin := twinsToAdmin(maps.Clone(env), true)
	var out []EnvChange
	var unused []string
	seen := map[string]bool{}
	for _, k := range slices.Sorted(maps.Keys(env)) {
		v := env[k]
		if strings.HasPrefix(k, "COMPOSE_") {
			continue
		}
		cur, ok := eff[k]
		if v == cur || (!ok && (v == "" || equalsDefault(defaults[k], v, eff))) {
			continue
		}
		c := EnvChange{Var: k, Key: k, Value: v}
		switch {
		case isGeneratedSecret(k):
			c.Secret = k != secrets.VAPIDPublicKey
			if _, chosen := s.Settings[k]; !chosen {
				c.Kind = ChangeSecret
			}
		case settings.Twins[k] != "":
			c.Kind, c.Key = ChangeAdmin, settings.Twins[k]
			if translated, ok := admin[c.Key]; ok {
				c.Value = translated
			}
			c.Secret = settings.SecretName(k)
		case proxies[strings.ToUpper(k)] != "":
			c.Key = proxies[strings.ToUpper(k)]
			c.Secret = hasUserinfo(v)
		case named[k].Name != "":
			c.Key, c.Secret = named[k].Name, named[k].Secret
		case slices.Contains(read, k):
			c.Secret = settings.SecretName(k) || hasUserinfo(v)
		default:
			unused = append(unused, k)
			continue
		}
		if seen[c.Key] {
			continue
		}
		seen[c.Key] = true
		out = append(out, c)
	}
	return out, unused, nil
}

func hasUserinfo(v string) bool {
	scheme, rest, ok := strings.Cut(v, "://")
	if !ok || scheme == "" {
		return false
	}
	host, _, _ := strings.Cut(rest, "/")
	return strings.Contains(host, "@")
}

func applyChanges(s *State, changes []EnvChange) {
	for _, c := range changes {
		switch c.Kind {
		case ChangeSecret:
			s.Secrets[c.Key] = c.Value
		case ChangeAdmin:
			if s.AdminImport == nil {
				s.AdminImport = &AdminImport{Values: map[string]string{}}
			}
			if s.AdminImport.Values == nil {
				s.AdminImport.Values = map[string]string{}
			}
			s.AdminImport.Values[c.Key] = c.Value
			s.AdminImport.SentAt = ""
		default:
			s.Settings[c.Key] = c.Value
		}
	}
}
