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

package integration

import (
	"io"
	"net/http"
	"strings"
	"testing"
	"time"
)

func TestHealthEndpoint(t *testing.T) {
	client := newTestClient(t)

	timeout := time.After(2 * time.Minute)
	ticker := time.NewTicker(2 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-timeout:
			t.Fatalf("API did not report healthy within the expected time")
		case <-ticker.C:
			resp, err := client.get("/_health")
			if err != nil {
				continue
			}
			body, readErr := io.ReadAll(resp.Body)
			resp.Body.Close()
			if readErr != nil {
				continue
			}
			if resp.StatusCode == http.StatusOK && strings.TrimSpace(string(body)) == "OK" {
				return
			}
		}
	}
}
