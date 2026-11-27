// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"encoding/base64"
	"errors"
	"fmt"
	"net"
	"net/netip"
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

func TrustedProxyList(v string) []string {
	return strings.FieldsFunc(v, func(r rune) bool { return r == ',' || r == ' ' || r == '\t' })
}

func validSignatureSecrets(v string) error {
	for _, entry := range strings.Split(v, ",") {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		key, err := base64.StdEncoding.DecodeString(entry)
		if err != nil || base64.StdEncoding.EncodeToString(key) != entry || len(key) < 32 {
			return errors.New("Give each key as standard base64 that decodes to at least 32 bytes, and separate the keys with commas.")
		}
	}
	return nil
}

func validTrustedProxies(v string) error {
	for _, entry := range TrustedProxyList(v) {
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

func ValidPort(v string) error {
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

func DomainIP(v string) (netip.Addr, bool) {
	bare := v
	if strings.HasPrefix(v, "[") && strings.HasSuffix(v, "]") {
		bare = v[1 : len(v)-1]
	}
	ip, err := netip.ParseAddr(bare)
	if err != nil || ip.Zone() != "" || ip.Is4() && bare != v {
		return netip.Addr{}, false
	}
	return ip.Unmap(), true
}

func NormalDomain(v string) string {
	ip, ok := DomainIP(v)
	switch {
	case !ok:
		return v
	case ip.Is4():
		return ip.String()
	}
	return "[" + ip.String() + "]"
}

func BareHost(v string) string {
	if ip, ok := DomainIP(v); ok {
		return ip.String()
	}
	return v
}

func MailDomain(v string) string {
	ip, ok := DomainIP(v)
	switch {
	case !ok:
		return v
	case ip.Is4():
		return "[" + ip.String() + "]"
	}
	return "[IPv6:" + ip.String() + "]"
}

func LocalOnlyIP(ip netip.Addr) bool {
	return ip.IsLoopback() || ip.IsUnspecified() || ip.IsMulticast() || ip.IsLinkLocalUnicast()
}

func validHostname(v string) error {
	if ip, ok := DomainIP(v); ok {
		if LocalOnlyIP(ip) {
			return errors.New("Use an IP address other devices can open, not a loopback or link-local one.")
		}
		return nil
	}
	bad := errors.New("Use a domain name such as chat.example.com or an IP address, without https:// or a port.")
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

const DefaultBind = "127.0.0.1:8080"

var errBind = errors.New("Use an address and port such as " + DefaultBind + ", or only a port.")

func NormaliseBind(v string) (string, error) {
	v = strings.TrimSpace(v)
	if ValidPort(v) == nil {
		n, _ := strconv.Atoi(v)
		return net.JoinHostPort("127.0.0.1", strconv.Itoa(n)), nil
	}
	host, port, err := net.SplitHostPort(v)
	if err != nil || ValidPort(port) != nil {
		return "", errBind
	}
	n, _ := strconv.Atoi(port)
	if host == "" {
		return net.JoinHostPort("127.0.0.1", strconv.Itoa(n)), nil
	}
	ip := net.ParseIP(host)
	if ip == nil {
		return "", errBind
	}
	return net.JoinHostPort(ip.String(), strconv.Itoa(n)), nil
}

func BindIsLoopback(v string) bool {
	norm, err := NormaliseBind(v)
	if err != nil {
		return false
	}
	host, _, err := net.SplitHostPort(norm)
	if err != nil {
		return false
	}
	ip := net.ParseIP(host)
	return ip != nil && ip.IsLoopback()
}

func validBind(v string) error {
	_, err := NormaliseBind(v)
	return err
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

func engineExample() string {
	if runtime.GOOS == "windows" {
		return "npipe:////./pipe/docker_engine"
	}
	return "unix:///var/run/docker.sock"
}

func validEndpoint(v string) error {
	bad := errors.New("Use an engine address such as " + engineExample() + ".")
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
