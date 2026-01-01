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
	"sync"
	"sync/atomic"
	"time"

	"github.com/gorilla/websocket"
	"github.com/klauspost/compress/zstd"
)

type zstdGatewayClient struct {
	token      string
	gatewayURL string
	headers    http.Header

	conn *websocket.Conn

	helloCh         chan gatewayHelloPayload
	readySignal     chan struct{}
	resumedSignal   chan struct{}
	dispatchCh      chan gatewayDispatch
	errCh           chan error
	pendingMu       sync.Mutex
	pendingDispatch []gatewayDispatch

	heartbeatMu    sync.Mutex
	heartbeatTimer *time.Ticker
	heartbeatDone  chan struct{}

	writeMu sync.Mutex
	wg      sync.WaitGroup

	sequence atomic.Int64
	closed   atomic.Bool

	zstdDecoder *zstd.Decoder
}
