// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

var unsupportedVars = map[string]string{
	"FLUXER_NATS_URL":           "A native instance runs its own NATS server and can't use another one.",
	"FLUXER_NATS_JETSTREAM_URL": "A native instance runs its own NATS server and can't use another one.",
	"FLUXER_NATS_GOMEMLIMIT":    "NATS runs inside fluxerd on a native instance.",
	"FLUXER_LOG_MAX_SIZE":       "A native instance logs to the systemd journal.",
	"FLUXER_LOG_MAX_FILE":       "A native instance logs to the systemd journal.",
	"FLUXER_BACKUP_ON_UPGRADE":  "rexctl backs up before every upgrade and keeps Fluxer's own upgrade backup off.",
	"FLUXER_INSTALL_METHOD":     "rexctl sets this so Fluxer shows rexctl's own upgrade command.",
	"FLUXER_UPGRADE_COMMAND":    "rexctl sets this to its own upgrade command.",
}

var Pinned = map[string]string{
	settings.RustProcess:    settings.RustSingle,
	settings.KVMode:         settings.KVPostgres,
	settings.StorageBackend: settings.StorageFilesystem,
	settings.LiveKitEnabled: "true",
}

func Unsupported(key string) (string, bool) {
	if why, ok := unsupportedVars[key]; ok {
		return why, true
	}
	if strings.HasPrefix(key, workerPrefix) {
		return "The worker runs inside the API process on a native instance.", true
	}
	return "", false
}
