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
	lksdk "github.com/livekit/server-sdk-go/v2"
	"github.com/pion/webrtc/v4"
	"testing"
)

// connectToLiveKit connects to a LiveKit room using the provided server update information
func connectToLiveKit(t testing.TB, endpoint, token, roomName, identity string) *livekitConnection {
	t.Helper()

	hostURL := endpoint

	roomCB := &lksdk.RoomCallback{
		ParticipantCallback: lksdk.ParticipantCallback{
			OnTrackSubscribed: func(track *webrtc.TrackRemote, pub *lksdk.RemoteTrackPublication, rp *lksdk.RemoteParticipant) {
				t.Logf("Track subscribed: participant=%s track=%s", rp.Identity(), pub.SID())
			},
			OnTrackUnsubscribed: func(track *webrtc.TrackRemote, pub *lksdk.RemoteTrackPublication, rp *lksdk.RemoteParticipant) {
				t.Logf("Track unsubscribed: participant=%s track=%s", rp.Identity(), pub.SID())
			},
		},
		OnParticipantConnected: func(rp *lksdk.RemoteParticipant) {
			t.Logf("Participant connected: %s", rp.Identity())
		},
		OnDisconnected: func() {
			t.Logf("LiveKit room disconnected")
		},
		OnReconnecting: func() {
			t.Logf("LiveKit room reconnecting")
		},
		OnReconnected: func() {
			t.Logf("LiveKit room reconnected")
		},
	}

	room, err := lksdk.ConnectToRoomWithToken(hostURL, token, roomCB)
	if err != nil {
		t.Fatalf("failed to connect to LiveKit room: %v", err)
	}

	t.Logf("Connected to LiveKit room: %s as %s", roomName, identity)

	return &livekitConnection{
		room: room,
		t:    t,
	}
}
