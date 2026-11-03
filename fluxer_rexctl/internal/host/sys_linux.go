// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
)

func Memory() (uint64, error) {
	data, err := os.ReadFile("/proc/meminfo")
	if err != nil {
		return 0, err
	}
	return parseMeminfo(data)
}

func parseMeminfo(data []byte) (uint64, error) {
	s := bufio.NewScanner(bytes.NewReader(data))
	for s.Scan() {
		fields := strings.Fields(s.Text())
		if len(fields) >= 2 && fields[0] == "MemTotal:" {
			kb, err := strconv.ParseUint(fields[1], 10, 64)
			if err != nil {
				return 0, err
			}
			return kb * 1024, nil
		}
	}
	return 0, errors.New("host: no MemTotal in /proc/meminfo")
}

func FreeDisk(path string) (uint64, error) {
	var st syscall.Statfs_t
	if err := syscall.Statfs(existingParent(path), &st); err != nil {
		return 0, err
	}
	return st.Bavail * uint64(st.Bsize), nil
}

func DiskID(path string) (string, error) {
	var st syscall.Stat_t
	if err := syscall.Stat(existingParent(path), &st); err != nil {
		return "", err
	}
	return strconv.FormatUint(st.Dev, 10), nil
}

func addrInUse(err error) bool { return errors.Is(err, syscall.EADDRINUSE) }

func portInUse(proto string, port int) (bool, string, error) {
	inodes, err := listeningInodes(proto, port)
	if err != nil {
		used, berr := bindTest(proto, port)
		return used, "", berr
	}
	if len(inodes) == 0 {
		return false, "", nil
	}
	return true, socketOwner(inodes), nil
}

func listeningInodes(proto string, port int) (map[string]bool, error) {
	inodes := map[string]bool{}
	read := 0
	for _, name := range []string{proto, proto + "6"} {
		data, err := os.ReadFile(filepath.Join("/proc/net", name))
		if err != nil {
			continue
		}
		read++
		for _, ino := range parseProcNet(data, proto, port) {
			inodes[ino] = true
		}
	}
	if read == 0 {
		return nil, errors.New("host: /proc/net is not readable")
	}
	return inodes, nil
}

func parseProcNet(data []byte, proto string, port int) []string {
	want := fmt.Sprintf(":%04X", port)
	var out []string
	s := bufio.NewScanner(bytes.NewReader(data))
	first := true
	for s.Scan() {
		if first {
			first = false
			continue
		}
		f := strings.Fields(s.Text())
		if len(f) < 10 || !strings.HasSuffix(f[1], want) {
			continue
		}
		if proto == "tcp" && f[3] != "0A" {
			continue
		}
		if proto == "udp" && f[3] != "07" {
			continue
		}
		out = append(out, f[9])
	}
	return out
}

func socketOwner(inodes map[string]bool) string {
	procs, err := os.ReadDir("/proc")
	if err != nil {
		return ""
	}
	for _, p := range procs {
		if _, err := strconv.Atoi(p.Name()); err != nil {
			continue
		}
		fdDir := filepath.Join("/proc", p.Name(), "fd")
		fds, err := os.ReadDir(fdDir)
		if err != nil {
			continue
		}
		for _, fd := range fds {
			link, err := os.Readlink(filepath.Join(fdDir, fd.Name()))
			if err != nil || !strings.HasPrefix(link, "socket:[") {
				continue
			}
			if inodes[strings.TrimSuffix(strings.TrimPrefix(link, "socket:["), "]")] {
				comm, err := os.ReadFile(filepath.Join("/proc", p.Name(), "comm"))
				if err != nil {
					return ""
				}
				return strings.TrimSpace(string(comm))
			}
		}
	}
	return ""
}
