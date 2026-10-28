// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"io/fs"
	"os"
)

const CustomTemplate = `# This is the file to edit when you want to change how Fluxer runs.
# rexctl adds it after its own Compose files and keeps it on upgrades.
# Run rexctl config edit to change it. rexctl checks the file and applies it.
# Change settings with rexctl config set, not here.
# To use an example, remove the # at the start of its lines.
#
# services:
#   edge:
#     labels:
#       com.example.team: chat
#     image: caddy:2.11-alpine
#     ports:
#       - "8443:8443"
#   api:
#     volumes:
#       - ./extra:/extra:ro
`

func (i *Instance) WriteCustomTemplate() error {
	f, err := os.OpenFile(i.Path(CustomCompose), os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o644)
	if errors.Is(err, fs.ErrExist) {
		return nil
	}
	if err != nil {
		return err
	}
	if _, err := f.WriteString(CustomTemplate); err != nil {
		f.Close()
		return err
	}
	return f.Close()
}
