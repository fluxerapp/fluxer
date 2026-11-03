// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"crypto/rand"
	"encoding/binary"
	"errors"
	"fmt"
	"net"
	"time"
)

var ErrNoPublicIP = errors.New("host: no STUN server answered")

var stunServers = []string{
	"stun.cloudflare.com:3478",
	"stun.l.google.com:19302",
	"stun1.l.google.com:19302",
}

const (
	stunMagic          = 0x2112A442
	stunBindingRequest = 0x0001
	stunBindingSuccess = 0x0101
	stunMappedAddress  = 0x0001
	stunXORMapped      = 0x0020
)

func PublicIP(ctx context.Context) (string, error) {
	var last error = ErrNoPublicIP
	for _, network := range []string{"udp4", "udp6"} {
		for _, server := range stunServers {
			ip, err := stunQuery(ctx, network, server)
			if err == nil {
				return ip.String(), nil
			}
			last = err
			if ctx.Err() != nil {
				return "", ctx.Err()
			}
		}
	}
	return "", fmt.Errorf("%w: %v", ErrNoPublicIP, last)
}

func PublicIPv4(ctx context.Context) (string, error) {
	return publicIPOver(ctx, "udp4")
}

func PublicIPv6(ctx context.Context) (string, error) {
	return publicIPOver(ctx, "udp6")
}

func publicIPOver(ctx context.Context, network string) (string, error) {
	var last error = ErrNoPublicIP
	for _, server := range stunServers {
		ip, err := stunQuery(ctx, network, server)
		if err == nil {
			return ip.String(), nil
		}
		last = err
		if ctx.Err() != nil {
			return "", ctx.Err()
		}
	}
	return "", fmt.Errorf("%w: %v", ErrNoPublicIP, last)
}

func stunQuery(ctx context.Context, network, server string) (net.IP, error) {
	ctx, cancel := context.WithTimeout(ctx, 3*time.Second)
	defer cancel()
	var d net.Dialer
	conn, err := d.DialContext(ctx, network, server)
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	if deadline, ok := ctx.Deadline(); ok {
		_ = conn.SetDeadline(deadline)
	}
	req := make([]byte, 20)
	binary.BigEndian.PutUint16(req[0:], stunBindingRequest)
	binary.BigEndian.PutUint32(req[4:], stunMagic)
	if _, err := rand.Read(req[8:20]); err != nil {
		return nil, err
	}
	buf := make([]byte, 1500)
	for attempt := 0; attempt < 2; attempt++ {
		if _, err := conn.Write(req); err != nil {
			return nil, err
		}
		n, err := conn.Read(buf)
		if err != nil {
			continue
		}
		ip, err := parseSTUN(buf[:n], req[8:20])
		if err == nil {
			return ip, nil
		}
	}
	return nil, fmt.Errorf("host: no answer from %s", server)
}

func parseSTUN(msg, txid []byte) (net.IP, error) {
	if len(msg) < 20 || binary.BigEndian.Uint16(msg[0:]) != stunBindingSuccess {
		return nil, errors.New("host: not a STUN binding response")
	}
	if binary.BigEndian.Uint32(msg[4:]) != stunMagic || string(msg[8:20]) != string(txid) {
		return nil, errors.New("host: STUN response for another request")
	}
	length := int(binary.BigEndian.Uint16(msg[2:]))
	if 20+length > len(msg) {
		return nil, errors.New("host: short STUN response")
	}
	attrs := msg[20 : 20+length]
	var mapped net.IP
	for len(attrs) >= 4 {
		typ := binary.BigEndian.Uint16(attrs[0:])
		alen := int(binary.BigEndian.Uint16(attrs[2:]))
		if 4+alen > len(attrs) {
			break
		}
		val := attrs[4 : 4+alen]
		switch typ {
		case stunXORMapped:
			if ip := stunAddress(val, true, txid); ip != nil {
				return ip, nil
			}
		case stunMappedAddress:
			mapped = stunAddress(val, false, txid)
		}
		attrs = attrs[4+(alen+3)&^3:]
	}
	if mapped != nil {
		return mapped, nil
	}
	return nil, errors.New("host: STUN response has no address")
}

func stunAddress(val []byte, xor bool, txid []byte) net.IP {
	if len(val) < 8 {
		return nil
	}
	var size int
	switch val[1] {
	case 0x01:
		size = net.IPv4len
	case 0x02:
		size = net.IPv6len
	default:
		return nil
	}
	if len(val) < 4+size {
		return nil
	}
	ip := make(net.IP, size)
	copy(ip, val[4:4+size])
	if xor {
		key := make([]byte, 16)
		binary.BigEndian.PutUint32(key, stunMagic)
		copy(key[4:], txid)
		for i := range ip {
			ip[i] ^= key[i]
		}
	}
	return ip
}

func PortInUse(proto string, port int) (bool, string, error) {
	if proto != "tcp" && proto != "udp" {
		return false, "", fmt.Errorf("host: unknown protocol %q", proto)
	}
	if port <= 0 || port > 65535 {
		return false, "", fmt.Errorf("host: port %d is out of range", port)
	}
	return portInUse(proto, port)
}

func bindTest(proto string, port int) (bool, error) {
	addr := fmt.Sprintf(":%d", port)
	var err error
	if proto == "tcp" {
		var l net.Listener
		l, err = net.Listen("tcp", addr)
		if err == nil {
			_ = l.Close()
		}
	} else {
		var c net.PacketConn
		c, err = net.ListenPacket("udp", addr)
		if err == nil {
			_ = c.Close()
		}
	}
	if err == nil {
		return false, nil
	}
	if addrInUse(err) {
		return true, nil
	}
	return false, err
}
