// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build windows

package instance

import "os"

func (i *Instance) OwnerUID() (int, bool) { return 0, false }

func ownerOf(os.FileInfo) (int, bool) { return 0, false }

func (i *Instance) giveToOwner(string) error { return nil }
