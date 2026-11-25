// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import "os/exec"

func asService(*exec.Cmd, bool) error { return nil }

func ownGroup(*exec.Cmd) {}
