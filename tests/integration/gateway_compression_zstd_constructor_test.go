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
	"testing"

	"github.com/klauspost/compress/zstd"
)

func newZstdGatewayClient(t testing.TB, client *testClient, token string) *zstdGatewayClient {
	t.Helper()

	decoder, err := zstd.NewReader(nil)
	if err != nil {
		t.Fatalf("failed to create zstd decoder: %v", err)
	}

	gc := &zstdGatewayClient{
		token:         token,
		gatewayURL:    buildGatewayURL(t),
		headers:       buildHeaders(t, client),
		helloCh:       make(chan gatewayHelloPayload, 1),
		readySignal:   make(chan struct{}),
		resumedSignal: make(chan struct{}),
		dispatchCh:    make(chan gatewayDispatch, 100),
		errCh:         make(chan error, 1),
		heartbeatDone: make(chan struct{}),
		zstdDecoder:   decoder,
	}

	gc.connect(t)
	return gc
}
