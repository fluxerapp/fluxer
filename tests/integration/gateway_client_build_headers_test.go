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
	"os"
	"testing"
)

func buildHeaders(t testing.TB, client *testClient) http.Header {
	t.Helper()
	headers := http.Header{}
	headers.Set("User-Agent", "FluxerIntegrationTests/1.0")
	if origin := os.Getenv("FLUXER_WEBAPP_ORIGIN"); origin != "" {
		headers.Set("Origin", origin)
	}
	if client != nil && client.clientIP != "" {
		headers.Set("X-Forwarded-For", client.clientIP)
	}
	if testToken := os.Getenv("FLUXER_TEST_TOKEN"); testToken != "" {
		headers.Set("X-Test-Token", testToken)
	}
	return headers
}
