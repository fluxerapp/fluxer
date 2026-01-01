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
	"net/http"
	"testing"
)

func assertStatus(t testing.TB, resp *http.Response, expectedStatus ...int) {
	t.Helper()
	accepted := expectedStatus
	if len(accepted) == 0 {
		accepted = []int{http.StatusOK}
	}
	for _, status := range accepted {
		if resp.StatusCode == status {
			return
		}
	}

	body := readResponseBody(resp)
	t.Fatalf("expected status %v, got %d: %s", accepted, resp.StatusCode, body)
}
