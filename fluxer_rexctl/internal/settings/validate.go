// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"errors"
	"fmt"
	"net"
	"net/url"
	"runtime"
	"strconv"
	"strings"

	"github.com/distribution/reference"
)

func oneOf(options ...string) func(string) error {
	return func(v string) error {
		for _, o := range options {
			if v == o {
				return nil
			}
		}
		return fmt.Errorf("Use one of %s.", strings.Join(options, ", "))
	}
}

func validText(v string) error {
	if strings.ContainsAny(v, "\r\n\x00") {
		return errors.New("The value has to fit on one line.")
	}
	return nil
}

func trustedProxyList(v string) []string {
	return strings.FieldsFunc(v, func(r rune) bool { return r == ',' || r == ' ' || r == '\t' })
}

func validTrustedProxies(v string) error {
	for _, entry := range trustedProxyList(v) {
		if entry == "private_ranges" || net.ParseIP(entry) != nil {
			continue
		}
		if _, _, err := net.ParseCIDR(entry); err == nil {
			continue
		}
		return fmt.Errorf("%s is not an IP address or range. Use addresses or ranges such as 10.0.0.0/8, or private_ranges.", entry)
	}
	return nil
}

func validBool(v string) error {
	if _, err := strconv.ParseBool(v); err != nil {
		return errors.New("Use true or false.")
	}
	return nil
}

func validPort(v string) error {
	n, err := strconv.Atoi(v)
	if err != nil || n < 1 || n > 65535 {
		return errors.New("Use a port number from 1 to 65535.")
	}
	return nil
}

func validIP(v string) error {
	if v == "" || net.ParseIP(v) != nil {
		return nil
	}
	return errors.New("Use an IP address, for example 203.0.113.7.")
}

func validHostname(v string) error {
	bad := errors.New("Use a domain name such as chat.example.com, without https:// or a port.")
	if v == "" || len(v) > 253 || strings.HasSuffix(v, ".") {
		return bad
	}
	for _, label := range strings.Split(v, ".") {
		if label == "" || len(label) > 63 || strings.HasPrefix(label, "-") || strings.HasSuffix(label, "-") {
			return bad
		}
		for _, r := range label {
			if !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9' || r == '-') {
				return bad
			}
		}
	}
	return nil
}

func validBind(v string) error {
	bad := errors.New("Use an address and port such as 127.0.0.1:8080, or only a port.")
	if validPort(v) == nil {
		return nil
	}
	host, port, err := net.SplitHostPort(v)
	if err != nil || validPort(port) != nil {
		return bad
	}
	if host != "" && net.ParseIP(host) == nil {
		return bad
	}
	return nil
}

func validURL(v string) error {
	u, err := url.Parse(v)
	if err != nil || (u.Scheme != "http" && u.Scheme != "https") || u.Host == "" {
		return errors.New("Use a full address that starts with http:// or https://.")
	}
	return nil
}

func validImage(v string) error {
	if _, err := reference.ParseNormalizedNamed(v); err != nil {
		return errors.New("Use an image reference such as postgres:16-alpine.")
	}
	return nil
}

func validRegistry(v string) error {
	bad := errors.New("Use a registry host such as registry.example.com or localhost:5000, without https://.")
	if v == "" || strings.Contains(v, "://") {
		return bad
	}
	named, err := reference.ParseNormalizedNamed(v + "/fluxer")
	if err != nil || reference.Domain(named) != strings.SplitN(v, "/", 2)[0] {
		return bad
	}
	return nil
}

func EngineExample() string {
	if runtime.GOOS == "windows" {
		return "npipe:////./pipe/docker_engine"
	}
	return "unix:///var/run/docker.sock"
}

func validEndpoint(v string) error {
	bad := errors.New("Use an engine address such as " + EngineExample() + ".")
	u, err := url.Parse(v)
	if err != nil {
		return bad
	}
	switch u.Scheme {
	case "unix", "npipe":
		if u.Path != "" {
			return nil
		}
	case "tcp", "ssh":
		if u.Host != "" {
			return nil
		}
	}
	return bad
}
