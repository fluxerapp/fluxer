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

// Voice state and server update structures based on gateway protocol
type voiceServerUpdate struct {
	Token        string  `json:"token"`
	Endpoint     string  `json:"endpoint"`
	GuildID      *string `json:"guild_id"`
	ConnectionID string  `json:"connection_id"`
}

type voiceStateUpdate struct {
	UserID       string  `json:"user_id"`
	SessionID    string  `json:"session_id"`
	GuildID      *string `json:"guild_id"`
	ChannelID    *string `json:"channel_id"`
	ConnectionID string  `json:"connection_id"`
	SelfMute     bool    `json:"self_mute"`
	SelfDeaf     bool    `json:"self_deaf"`
	SelfVideo    bool    `json:"self_video"`
	SelfStream   bool    `json:"self_stream"`
	Mute         bool    `json:"mute"`
	Deaf         bool    `json:"deaf"`
	Video        bool    `json:"video"`
	Stream       bool    `json:"stream"`
}
