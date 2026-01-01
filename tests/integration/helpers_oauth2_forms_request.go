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
	"net/url"
	"strings"
	"testing"
)

// newFormRequest creates a new HTTP request with form-encoded data.
func newFormRequest(t testing.TB, client *testClient, path string, form url.Values) *http.Request {
	t.Helper()
	req, err := http.NewRequest(http.MethodPost, client.baseURL+path, strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatalf("failed to build form request: %v", err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	client.applyCommonHeaders(req)
	return req
}
