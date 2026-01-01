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

// SendVoiceStateUpdate sends a voice state update to the gateway
// Pass nil for guildID when joining DM/group DM calls
// Pass nil for channelID to disconnect from voice
func (g *gatewayClient) SendVoiceStateUpdate(guildID, channelID, connectionID *string, selfMute, selfDeaf, selfVideo, selfStream bool) {
	payload := map[string]any{
		"op": gatewayOpVoiceStateUpdate,
		"d": map[string]any{
			"guild_id":      guildID,
			"channel_id":    channelID,
			"connection_id": connectionID,
			"self_mute":     selfMute,
			"self_deaf":     selfDeaf,
			"self_video":    selfVideo,
			"self_stream":   selfStream,
		},
	}
	g.writeJSON(payload)
}
