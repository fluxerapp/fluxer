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
	"fmt"
	"net/http"
	"strings"
	"testing"
	"time"
)

const registerPasswordMinLength = 8

func registerTestUser(t testing.TB, client *testClient, email, password string, opts ...registerOption) registerResponse {
	t.Helper()

	req := registerRequest{
		Email:       email,
		Username:    fmt.Sprintf("itest%x", time.Now().UnixNano()),
		GlobalName:  "Integration Tester",
		Password:    password,
		DateOfBirth: adultDateOfBirth(),
		Consent:     true,
	}

	for _, opt := range opts {
		opt(&req)
	}

	req.Password = ensureMinPasswordLength(req.Password)

	var resp *http.Response
	var err error
	for attempt := 1; attempt <= 3; attempt++ {
		resp, err = client.postJSON("/auth/register", req)
		if err == nil && resp.StatusCode == http.StatusOK {
			break
		}

		if resp != nil && resp.StatusCode < http.StatusInternalServerError {
			break
		}

		if resp != nil {
			resp.Body.Close()
		}
		time.Sleep(time.Duration(attempt) * time.Second)
	}
	if err != nil {
		t.Fatalf("failed to call register endpoint: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("register returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var parsed registerResponse
	decodeJSONResponse(t, resp, &parsed)
	return parsed
}

func withDateOfBirth(date string) registerOption {
	return func(req *registerRequest) {
		req.DateOfBirth = date
	}
}

func ensureMinPasswordLength(password string) string {
	if len(password) >= registerPasswordMinLength {
		return password
	}
	return password + strings.Repeat("X", registerPasswordMinLength-len(password))
}
