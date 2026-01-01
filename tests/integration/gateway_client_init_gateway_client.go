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
	"strings"
	"testing"
)

func initGatewayClient(
	t testing.TB,
	client *testClient,
	token string,
	resume *gatewayResumeState,
	skipEnsure bool,
) *gatewayClient {
	t.Helper()
	if token == "" {
		t.Fatalf("gateway client requires auth token")
	}
	if !skipEnsure && !strings.HasPrefix(token, "Bot ") {
		ensureSessionStarted(t, client, token)
	}

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

	gc := &gatewayClient{
		token:         token,
		gatewayURL:    buildGatewayURL(t),
		headers:       headers,
		helloCh:       make(chan gatewayHelloPayload, 1),
		readySignal:   make(chan struct{}, 1),
		resumedSignal: make(chan struct{}, 1),
		dispatchCh:    make(chan gatewayDispatch, 10000),
		errCh:         make(chan error, 2),
	}

	gc.connect(t, resume)
	return gc
}
