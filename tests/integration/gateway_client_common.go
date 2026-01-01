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
	"github.com/gorilla/websocket"
	"net/http"
	"sync"
	"sync/atomic"
	"time"
)

const (
	gatewayOpDispatch         = 0
	gatewayOpHeartbeat        = 1
	gatewayOpIdentify         = 2
	gatewayOpVoiceStateUpdate = 4
	gatewayOpResume           = 6
	gatewayOpReconnect        = 7
	gatewayOpInvalidSession   = 9
	gatewayOpHello            = 10
	gatewayOpHeartbeatAck     = 11
	gatewayOpGatewayError     = 12
)

type gatewayResumeState struct {
	SessionID string
	Sequence  int64
}

type gatewayClient struct {
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

	sessionMu sync.Mutex
	sessionID string

	sequence atomic.Int64
	closed   atomic.Bool
}

type gatewayHelloPayload struct {
	HeartbeatInterval int `json:"heartbeat_interval"`
}

type gatewayPayload struct {
	Op       int             `json:"op"`
	Data     json.RawMessage `json:"d"`
	Sequence *int            `json:"s"`
	Type     string          `json:"t"`
}

type gatewayDispatch struct {
	Type     string
	Data     json.RawMessage
	Sequence int64
}
