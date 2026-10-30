// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"bufio"
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/netip"
	"net/url"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

var cgnatRange = netip.MustParsePrefix("100.64.0.0/10")

var cloudflareRanges = []string{
	"173.245.48.0/20", "103.21.244.0/22", "103.22.200.0/22", "103.31.4.0/22", "141.101.64.0/18",
	"108.162.192.0/18", "190.93.240.0/20", "188.114.96.0/20", "197.234.240.0/22", "198.41.128.0/17",
	"162.158.0.0/15", "104.16.0.0/13", "104.24.0.0/14", "172.64.0.0/13", "131.0.72.0/22",
	"2400:cb00::/32", "2606:4700::/32", "2803:f800::/32", "2405:b500::/32", "2405:8100::/32",
	"2a06:98c0::/29", "2c0f:f248::/32",
}

var unusableIPv6 = []netip.Prefix{
	netip.MustParsePrefix("fc00::/7"),
	netip.MustParsePrefix("2002::/16"),
	netip.MustParsePrefix("2001::/32"),
	netip.MustParsePrefix("2001:db8::/32"),
	netip.MustParsePrefix("3fff::/20"),
}

func IsCloudflare(addr string) bool {
	ip, err := netip.ParseAddr(addr)
	if err != nil {
		return false
	}
	for _, r := range cloudflareRanges {
		if netip.MustParsePrefix(r).Contains(ip) {
			return true
		}
	}
	return false
}

func IsCGNAT(addr string) bool {
	ip, err := netip.ParseAddr(addr)
	return err == nil && cgnatRange.Contains(ip.Unmap())
}

func InterfaceAddresses() []string {
	var out []string
	addrs, err := net.InterfaceAddrs()
	if err != nil {
		return nil
	}
	for _, a := range addrs {
		if p, err := netip.ParsePrefix(a.String()); err == nil {
			out = append(out, p.Addr().Unmap().String())
		}
	}
	return out
}

func DefaultRouteIPv4() string {
	conn, err := net.Dial("udp4", "198.51.100.1:9")
	if err != nil {
		return ""
	}
	defer conn.Close()
	if a, ok := conn.LocalAddr().(*net.UDPAddr); ok {
		return a.IP.String()
	}
	return ""
}

func DefaultRouteIPv6() string {
	conn, err := net.Dial("udp6", "[2001:db8::1]:9")
	if err != nil {
		return ""
	}
	defer conn.Close()
	if a, ok := conn.LocalAddr().(*net.UDPAddr); ok {
		return a.IP.String()
	}
	return ""
}

func TemporaryIPv6() []string {
	data, err := os.ReadFile("/proc/net/if_inet6")
	if err != nil {
		return nil
	}
	return parseIfInet6(string(data))
}

func parseIfInet6(data string) []string {
	var out []string
	for _, line := range strings.Split(data, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 5 || len(fields[0]) != 32 {
			continue
		}
		flags, err := strconv.ParseUint(fields[4], 16, 32)
		if err != nil || flags&0x01 == 0 {
			continue
		}
		raw, err := hex.DecodeString(fields[0])
		if err != nil {
			continue
		}
		if ip, ok := netip.AddrFromSlice(raw); ok {
			out = append(out, ip.String())
		}
	}
	return out
}

func IsIPv4(addr string) bool {
	ip, err := netip.ParseAddr(addr)
	return err == nil && ip.Is4()
}

func IsIPv6(addr string) bool {
	ip, err := netip.ParseAddr(addr)
	return err == nil && ip.Is6() && !ip.Is4In6()
}

func GlobalIPv6(addr string) bool {
	ip, err := netip.ParseAddr(addr)
	if err != nil || !ip.Is6() || ip.Is4In6() || ip.Zone() != "" || !ip.IsGlobalUnicast() {
		return false
	}
	for _, p := range unusableIPv6 {
		if p.Contains(ip) {
			return false
		}
	}
	return true
}

func UsableIPv6(addr string, local, temporary []string) bool {
	if !GlobalIPv6(addr) {
		return false
	}
	ip := netip.MustParseAddr(addr).String()
	return slices.Contains(local, ip) && !slices.Contains(temporary, ip)
}

func StableIPv6(observed string, local, temporary []string) string {
	if UsableIPv6(observed, local, temporary) {
		return netip.MustParseAddr(observed).String()
	}
	seen, err := netip.ParseAddr(observed)
	if err != nil || !GlobalIPv6(observed) {
		return ""
	}
	net64, _ := seen.Prefix(64)
	for _, a := range local {
		if ip, err := netip.ParseAddr(a); err == nil && net64.Contains(ip) && UsableIPv6(a, local, temporary) {
			return ip.String()
		}
	}
	return ""
}

func MachineIPv6(observed string) string {
	local, temporary := InterfaceAddresses(), TemporaryIPv6()
	if observed != "" {
		return StableIPv6(observed, local, temporary)
	}
	return StableIPv6(DefaultRouteIPv6(), local, temporary)
}

const (
	FamilyNone = ""
	FamilyIPv4 = "ipv4"
	FamilyIPv6 = "ipv6"
	FamilyDual = "dual"
)

func FamilyOf(ipv4, ipv6 string) string {
	switch {
	case ipv4 != "" && ipv6 != "":
		return FamilyDual
	case ipv4 != "":
		return FamilyIPv4
	case ipv6 != "":
		return FamilyIPv6
	}
	return FamilyNone
}

func NodeIPs(ipv4, ipv6 string) string {
	var parts []string
	for _, ip := range []string{ipv4, ipv6} {
		if ip != "" {
			parts = append(parts, ip)
		}
	}
	return strings.Join(parts, ",")
}

type Reachability struct {
	PublicIPv4 string
	PublicIPv6 string
	BehindNAT  bool
	CGNAT      bool
}

func (r Reachability) Family() string { return FamilyOf(r.PublicIPv4, r.PublicIPv6) }

func Assess(publicIPv4, publicIPv6 string, local []string, routeIP string) Reachability {
	r := Reachability{PublicIPv4: publicIPv4, PublicIPv6: publicIPv6}
	r.BehindNAT = publicIPv4 != "" && !slices.Contains(local, publicIPv4)
	r.CGNAT = publicIPv4 != "" && IsCGNAT(routeIP)
	return r
}

type DNSAnswer struct {
	A    []string
	AAAA []string
}

type Resolver func(ctx context.Context, name string) (DNSAnswer, error)

var dohServers = []string{
	"https://1.1.1.1/dns-query",
	"https://8.8.8.8/resolve",
	"https://[2606:4700:4700::1111]/dns-query",
	"https://[2001:4860:4860::8888]/resolve",
}

func PublicLookup(hc *http.Client) Resolver {
	return func(ctx context.Context, name string) (DNSAnswer, error) {
		var last error
		for _, server := range dohServers {
			a, errA := doh(ctx, hc, server, name, "A")
			aaaa, errAAAA := doh(ctx, hc, server, name, "AAAA")
			if errA == nil && errAAAA == nil {
				return DNSAnswer{A: a, AAAA: aaaa}, nil
			}
			last = errors.Join(errA, errAAAA)
		}
		addrs, err := net.DefaultResolver.LookupHost(ctx, name)
		if err != nil {
			return DNSAnswer{}, errors.Join(last, err)
		}
		var out DNSAnswer
		for _, a := range addrs {
			if ip, err := netip.ParseAddr(a); err == nil && ip.Is4() {
				out.A = append(out.A, a)
			} else if err == nil {
				out.AAAA = append(out.AAAA, a)
			}
		}
		return out, nil
	}
}

func doh(ctx context.Context, hc *http.Client, server, name, kind string) ([]string, error) {
	u, _ := url.Parse(server)
	q := u.Query()
	q.Set("name", name)
	q.Set("type", kind)
	u.RawQuery = q.Encode()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/dns-json")
	resp, err := hc.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("voice: %s answered %d", server, resp.StatusCode)
	}
	var body struct {
		Status int `json:"Status"`
		Answer []struct {
			Type int    `json:"type"`
			Data string `json:"data"`
		} `json:"Answer"`
	}
	if err := json.NewDecoder(io.LimitReader(resp.Body, 1<<16)).Decode(&body); err != nil {
		return nil, err
	}
	want := map[string]int{"A": 1, "AAAA": 28}[kind]
	var out []string
	for _, a := range body.Answer {
		if a.Type == want {
			out = append(out, a.Data)
		}
	}
	return out, nil
}

type HostnameCheck int

const (
	HostnameMissing HostnameCheck = iota
	HostnameOK
	HostnameElsewhere
	HostnameIPv6Elsewhere
	HostnameProxied
	HostnameIPv4Front
	HostnameNoA
	HostnameNoAAAA
)

func (h HostnameCheck) Usable() bool {
	return h == HostnameOK || h == HostnameIPv4Front || h == HostnameNoA || h == HostnameNoAAAA
}

func CheckHostname(answer DNSAnswer, publicIPv4, publicIPv6 string, ownIPv6 []string) HostnameCheck {
	all := slices.Concat(answer.A, answer.AAAA)
	if len(all) > 0 && !slices.ContainsFunc(all, func(a string) bool { return !IsCloudflare(a) }) {
		return HostnameProxied
	}
	own := func(a string) bool {
		ip, err := netip.ParseAddr(a)
		if err != nil {
			return false
		}
		return slices.ContainsFunc(append([]string{publicIPv6}, ownIPv6...), func(o string) bool {
			p, err := netip.ParseAddr(o)
			return err == nil && p == ip
		})
	}
	if publicIPv4 == "" && publicIPv6 != "" {
		if len(answer.AAAA) == 0 {
			return HostnameMissing
		}
		if !all6(answer.AAAA, own) {
			return HostnameElsewhere
		}
		if len(answer.A) > 0 {
			return HostnameIPv4Front
		}
		return HostnameOK
	}
	if len(answer.A) == 0 {
		if publicIPv6 != "" && len(answer.AAAA) > 0 && all6(answer.AAAA, own) {
			return HostnameNoA
		}
		return HostnameMissing
	}
	for _, a := range answer.A {
		if a != publicIPv4 {
			return HostnameElsewhere
		}
	}
	if !all6(answer.AAAA, own) {
		return HostnameIPv6Elsewhere
	}
	if publicIPv6 != "" && len(answer.AAAA) == 0 {
		return HostnameNoAAAA
	}
	return HostnameOK
}

func all6(records []string, own func(string) bool) bool {
	for _, a := range records {
		if !own(a) {
			return false
		}
	}
	return true
}

func HTTPBase(endpoint string) string {
	u, err := url.Parse(endpoint)
	if err != nil {
		return endpoint
	}
	switch u.Scheme {
	case "wss":
		u.Scheme = "https"
	case "ws":
		u.Scheme = "http"
	}
	return strings.TrimRight(u.String(), "/")
}

const DefaultHTTPBind = "127.0.0.1"

func LocalBase(bind string, httpPort int) string {
	return "http://" + net.JoinHostPort(dialHost(bind), strconv.Itoa(httpPort))
}

func HTTPAddress(bind string, httpPort int) string {
	if bind == "" {
		bind = DefaultHTTPBind
	}
	return net.JoinHostPort(bind, strconv.Itoa(httpPort))
}

func HTTPBindValue(bind string) string {
	if bind == "" {
		return DefaultHTTPBind
	}
	if a, err := netip.ParseAddr(bind); err == nil && a.Is6() {
		return "[" + a.String() + "]"
	}
	return bind
}

func NormalHTTPBind(bind string) (string, bool) {
	a, err := netip.ParseAddr(strings.TrimSpace(bind))
	if err != nil || a.Zone() != "" {
		return "", false
	}
	a = a.Unmap()
	if a.String() == DefaultHTTPBind {
		return "", true
	}
	return a.String(), true
}

func HTTPBindPrivate(bind string) bool {
	if bind == "" {
		return true
	}
	a, err := netip.ParseAddr(bind)
	return err == nil && a.IsLoopback()
}

func HTTPBindOnMachine(bind string, local []string) bool {
	a, err := netip.ParseAddr(bind)
	if err != nil {
		return false
	}
	if a.IsUnspecified() || a.IsLoopback() {
		return true
	}
	return slices.Contains(local, a.String())
}

func dialHost(bind string) string {
	a, err := netip.ParseAddr(bind)
	if err != nil || a.IsUnspecified() {
		return DefaultHTTPBind
	}
	return a.String()
}

func InstanceEdge(endpoint, mode, httpsPort, bind string) (string, string) {
	base := HTTPBase(endpoint)
	switch mode {
	case settings.EdgeProxy, settings.EdgeTunnel:
		addr := "127.0.0.1:8080"
		if mode == settings.EdgeTunnel {
			addr = "127.0.0.1:80"
		}
		if v := usable(bind); v != "" {
			addr = loopback(v)
		}
		if u, err := url.Parse(base); err == nil {
			u.Scheme = "http"
			base = u.String()
		}
		return base, addr
	}
	port := "443"
	if v := usable(httpsPort); v != "" {
		port = v
	}
	return base, "127.0.0.1:" + port
}

func usable(v string) string {
	v = strings.TrimSpace(v)
	if strings.Contains(v, "$") {
		return ""
	}
	return v
}

func loopback(v string) string {
	h, port, err := net.SplitHostPort(v)
	if err != nil {
		h, port = "", strings.TrimPrefix(v, ":")
	}
	if h == "" || h == "0.0.0.0" || h == "::" {
		h = "127.0.0.1"
	}
	return net.JoinHostPort(h, port)
}

func probeClient(base, dialAddr string, insecure bool) (*http.Client, *http.Transport, error) {
	u, err := url.Parse(base)
	if err != nil {
		return nil, nil, err
	}
	dialer := &net.Dialer{Timeout: 5 * time.Second}
	tr := &http.Transport{
		DialContext: func(ctx context.Context, network, addr string) (net.Conn, error) {
			if dialAddr != "" {
				addr = dialAddr
			}
			return dialer.DialContext(ctx, network, addr)
		},
		TLSClientConfig: &tls.Config{ServerName: u.Hostname(), InsecureSkipVerify: insecure},
	}
	hc := &http.Client{Transport: tr, Timeout: 10 * time.Second, CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse }}
	return hc, tr, nil
}

func ProbeThroughEdge(ctx context.Context, base, dialAddr string, insecure bool) error {
	hc, tr, err := probeClient(base, dialAddr, insecure)
	if err != nil {
		return err
	}
	defer tr.CloseIdleConnections()
	target := strings.TrimRight(base, "/") + "/"
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return err
	}
	resp, err := hc.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(io.LimitReader(resp.Body, 64))
	if resp.StatusCode != http.StatusOK || strings.TrimSpace(string(body)) != "OK" {
		return fmt.Errorf("voice: %s answered %d", target, resp.StatusCode)
	}
	return nil
}

func CertificateExpiry(ctx context.Context, endpoint, edgeAddr string) (time.Time, error) {
	u, err := url.Parse(endpoint)
	if err != nil {
		return time.Time{}, err
	}
	d := &tls.Dialer{NetDialer: &net.Dialer{Timeout: 5 * time.Second}, Config: &tls.Config{ServerName: u.Hostname(), InsecureSkipVerify: true}}
	conn, err := d.DialContext(ctx, "tcp", edgeAddr)
	if err != nil {
		return time.Time{}, err
	}
	defer conn.Close()
	state := conn.(*tls.Conn).ConnectionState()
	if len(state.PeerCertificates) == 0 {
		return time.Time{}, errors.New("voice: no certificate")
	}
	leaf := state.PeerCertificates[0]
	pool := x509.NewCertPool()
	for _, c := range state.PeerCertificates[1:] {
		pool.AddCert(c)
	}
	if _, err := leaf.Verify(x509.VerifyOptions{DNSName: u.Hostname(), Intermediates: pool}); err != nil {
		return leaf.NotAfter, err
	}
	return leaf.NotAfter, nil
}

func ReadSysctl(name string) string {
	data, err := os.ReadFile("/proc/sys/" + strings.ReplaceAll(name, ".", "/"))
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(data))
}

func LogLines(r io.Reader, match func(string) bool) []string {
	var out []string
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 0, 64*1024), 1<<20)
	for sc.Scan() {
		if line := sc.Text(); match(line) {
			out = append(out, line)
		}
	}
	return out
}

type RoomLoad struct {
	Rooms        int
	Participants int
}

func ListRooms(ctx context.Context, base, dialAddr, key, secret string, insecure bool) (RoomLoad, error) {
	hc, tr, err := probeClient(base, dialAddr, insecure)
	if err != nil {
		return RoomLoad{}, err
	}
	defer tr.CloseIdleConnections()
	now := time.Now()
	token := SignHS256(secret, map[string]any{
		"iss":   key,
		"nbf":   now.Add(-10 * time.Second).Unix(),
		"exp":   now.Add(time.Minute).Unix(),
		"video": map[string]any{"roomList": true},
	})
	target := strings.TrimRight(base, "/") + "/twirp/livekit.RoomService/ListRooms"
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, target, strings.NewReader("{}"))
	if err != nil {
		return RoomLoad{}, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+token)
	resp, err := hc.Do(req)
	if err != nil {
		return RoomLoad{}, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return RoomLoad{}, fmt.Errorf("voice: the room service answered %d", resp.StatusCode)
	}
	var body struct {
		Rooms []struct {
			Name            string `json:"name"`
			NumParticipants int    `json:"num_participants"`
			CamelCase       int    `json:"numParticipants"`
		} `json:"rooms"`
	}
	if err := json.NewDecoder(io.LimitReader(resp.Body, 4<<20)).Decode(&body); err != nil {
		return RoomLoad{}, err
	}
	var load RoomLoad
	for _, r := range body.Rooms {
		if strings.HasPrefix(r.Name, TestRoomPrefix) {
			continue
		}
		load.Rooms++
		load.Participants += max(r.NumParticipants, r.CamelCase)
	}
	return load, nil
}
