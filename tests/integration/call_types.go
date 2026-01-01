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

// Call event types for gateway events

type callCreateEvent struct {
	ChannelID   string             `json:"channel_id"`
	MessageID   string             `json:"message_id"`
	Region      string             `json:"region"`
	Ringing     []string           `json:"ringing"`
	VoiceStates []voiceStateUpdate `json:"voice_states"`
}

type callUpdateEvent struct {
	ChannelID   string             `json:"channel_id"`
	MessageID   string             `json:"message_id"`
	Region      string             `json:"region"`
	Ringing     []string           `json:"ringing"`
	VoiceStates []voiceStateUpdate `json:"voice_states"`
}

type callDeleteEvent struct {
	ChannelID   string `json:"channel_id"`
	Unavailable bool   `json:"unavailable,omitempty"`
}

// Message call object embedded in message responses
type messageCallObject struct {
	Participants   []string `json:"participants"`
	EndedTimestamp *string  `json:"ended_timestamp"`
}

// Full message response with call object
type callMessageResponse struct {
	ID        string             `json:"id"`
	Type      int                `json:"type"`
	ChannelID string             `json:"channel_id"`
	AuthorID  string             `json:"author_id"`
	Content   string             `json:"content"`
	Call      *messageCallObject `json:"call,omitempty"`
}

// Call eligibility response
type callEligibilityResponse struct {
	Ringable bool `json:"ringable"`
	Silent   bool `json:"silent,omitempty"`
}
