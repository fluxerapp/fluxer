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
	"testing"
)

func TestAuthDesktopHandoffFlow(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	login := loginTestUser(t, client, account.Email, account.Password)

	resp, err := client.postJSON("/auth/handoff/initiate", nil)
	if err != nil {
		t.Fatalf("failed to initiate desktop handoff: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var initResp handoffInitiateResponse
	decodeJSONResponse(t, resp, &initResp)
	if initResp.Code == "" {
		t.Fatalf("expected handoff code")
	}

	if !validateHandoffCodeFormat(initResp.Code) {
		t.Fatalf("expected code in XXXX-XXXX format, got %s", initResp.Code)
	}

	statusURL := fmt.Sprintf("/auth/handoff/%s/status", initResp.Code)
	resp, err = client.get(statusURL)
	if err != nil {
		t.Fatalf("failed to poll pending handoff status: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var status handoffStatusResponse
	decodeJSONResponse(t, resp, &status)
	if status.Status != "pending" {
		t.Fatalf("expected pending status, got %s", status.Status)
	}
	if status.Token != "" || status.UserID != "" {
		t.Fatalf("expected no token while pending")
	}

	resp, err = client.postJSON("/auth/handoff/complete", map[string]string{
		"code":    initResp.Code,
		"token":   login.Token,
		"user_id": login.UserID,
	})
	if err != nil {
		t.Fatalf("failed to complete handoff: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.get(statusURL)
	if err != nil {
		t.Fatalf("failed to poll completed handoff status: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &status)
	if status.Status != "completed" {
		t.Fatalf("expected completed status, got %s", status.Status)
	}
	if status.Token == "" {
		t.Fatalf("expected handoff token to be returned")
	}
	if status.Token == login.Token {
		t.Fatalf("expected handoff token to be a new session token")
	}
	if status.UserID != login.UserID {
		t.Fatalf("expected user id %s, got %s", login.UserID, status.UserID)
	}

	resp, err = client.getWithAuth("/users/@me", login.Token)
	if err != nil {
		t.Fatalf("failed to fetch /users/@me with original session: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var originalSession userPrivateResponse
	decodeJSONResponse(t, resp, &originalSession)
	if originalSession.ID != login.UserID {
		t.Fatalf("expected original session to remain valid for user %s, got %s", login.UserID, originalSession.ID)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", status.Token)
	if err != nil {
		t.Fatalf("failed to fetch /users/@me with handoff session: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var handoffSession userPrivateResponse
	decodeJSONResponse(t, resp, &handoffSession)
	if handoffSession.ID != login.UserID {
		t.Fatalf("expected handoff session to resolve to user %s, got %s", login.UserID, handoffSession.ID)
	}
	resp.Body.Close()

	resp, err = client.get(statusURL)
	if err != nil {
		t.Fatalf("failed to poll status after token retrieval: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &status)
	if status.Status != "expired" {
		t.Fatalf("expected expired status after token retrieval, got %s", status.Status)
	}

	resp, err = client.postJSON("/auth/handoff/initiate", nil)
	if err != nil {
		t.Fatalf("failed to initiate second handoff: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var second handoffInitiateResponse
	decodeJSONResponse(t, resp, &second)

	cancelURL := fmt.Sprintf("/auth/handoff/%s", second.Code)
	resp, err = client.delete(cancelURL, "")
	if err != nil {
		t.Fatalf("failed to cancel handoff: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.get(fmt.Sprintf("/auth/handoff/%s/status", second.Code))
	if err != nil {
		t.Fatalf("failed to poll cancelled handoff status: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &status)
	if status.Status != "expired" {
		t.Fatalf("expected expired status after cancelling, got %s", status.Status)
	}
}
