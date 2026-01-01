/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integrations

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// WriteLivekitFileFromEnv writes LiveKit configuration from environment variables
func WriteLivekitFileFromEnv(path string, env map[string]string) (bool, error) {
	voiceEnabled := strings.ToLower(strings.TrimSpace(env["VOICE_ENABLED"])) == "true"
	if !voiceEnabled {
		return false, nil
	}

	apiKey := strings.TrimSpace(env["LIVEKIT_API_KEY"])
	apiSecret := strings.TrimSpace(env["LIVEKIT_API_SECRET"])
	webhookURL := strings.TrimSpace(env["LIVEKIT_WEBHOOK_URL"])

	if apiKey == "" || apiSecret == "" || webhookURL == "" {
		return false, nil
	}

	redisURL := strings.TrimSpace(env["REDIS_URL"])
	redisAddr := strings.TrimPrefix(redisURL, "redis://")
	if redisAddr == "" {
		redisAddr = "redis:6379"
	}

	yaml := fmt.Sprintf(`port: 7880

redis:
  address: "%s"
  db: 0

keys:
  "%s": "%s"

rtc:
  tcp_port: 7881

webhook:
  api_key: "%s"
  urls:
    - "%s"

room:
  auto_create: true
  max_participants: 100
  empty_timeout: 300

development: true
`, redisAddr, apiKey, apiSecret, apiKey, webhookURL)

	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return false, err
	}
	if err := os.WriteFile(path, []byte(yaml), 0o600); err != nil {
		return false, err
	}
	return true, nil
}
