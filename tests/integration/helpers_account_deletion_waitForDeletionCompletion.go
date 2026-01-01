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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

// waitForDeletionCompletion waits for deletion to complete with exponential backoff
func waitForDeletionCompletion(t testing.TB, client *testClient, userID string, timeout time.Duration) {
	backoff := 100 * time.Millisecond
	maxBackoff := 5 * time.Second
	start := time.Now()

	for time.Since(start) < timeout {
		time.Sleep(backoff)

		resp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", userID))
		if err != nil {
			t.Logf("Error checking deletion status: %v", err)
			continue
		}

		if resp.StatusCode == http.StatusOK {
			var data userDataExistsResponse
			if err := json.NewDecoder(resp.Body).Decode(&data); err != nil {
				resp.Body.Close()
				t.Logf("Error decoding response: %v", err)
				continue
			}
			resp.Body.Close()

			if data.UserExists && data.HasDeletedFlag {
				t.Log("Deletion completed successfully")
				return
			}
		} else {
			resp.Body.Close()
		}

		backoff = time.Duration(float64(backoff) * 1.5)
		if backoff > maxBackoff {
			backoff = maxBackoff
		}
	}

	t.Fatal("Deletion did not complete within timeout")
}
