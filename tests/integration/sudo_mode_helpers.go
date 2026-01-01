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

func assertSudoModeRequired(t testing.TB, resp *http.Response) {
	t.Helper()
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for sudo mode requirement, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var apiErr errorResponse
	decodeJSONResponse(t, resp, &apiErr)
	if apiErr.Code != "SUDO_MODE_REQUIRED" {
		t.Fatalf("expected SUDO_MODE_REQUIRED code, got %s", apiErr.Code)
	}
}
