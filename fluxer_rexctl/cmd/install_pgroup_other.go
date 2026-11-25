// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package cmd

import "os/exec"

func ownProcessGroup(*exec.Cmd) {}
