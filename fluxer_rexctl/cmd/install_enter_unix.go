// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package cmd

import (
	"bytes"
	"io"
	"os"
	"sync"

	"golang.org/x/sys/unix"
	"golang.org/x/term"
)

func watchEnter(r io.Reader) (<-chan struct{}, func()) {
	f, ok := r.(*os.File)
	if !ok || !term.IsTerminal(int(f.Fd())) {
		return nil, func() {}
	}
	fd := int(f.Fd())
	pressed := make(chan struct{})
	stop := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		buf := make([]byte, 256)
		fds := []unix.PollFd{{Fd: int32(fd), Events: unix.POLLIN}}
		for {
			select {
			case <-stop:
				return
			default:
			}
			n, err := unix.Poll(fds, 100)
			if err != nil && err != unix.EINTR {
				return
			}
			if n == 0 {
				continue
			}
			if fds[0].Revents&unix.POLLIN == 0 {
				if fds[0].Revents&(unix.POLLHUP|unix.POLLERR|unix.POLLNVAL) != 0 {
					return
				}
				continue
			}
			read, err := unix.Read(fd, buf)
			if err != nil || read == 0 {
				return
			}
			if bytes.ContainsAny(buf[:read], "\r\n") {
				close(pressed)
				return
			}
		}
	}()
	var once sync.Once
	return pressed, func() {
		once.Do(func() {
			close(stop)
			wg.Wait()
		})
	}
}
