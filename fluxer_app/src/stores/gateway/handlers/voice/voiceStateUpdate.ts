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

import MediaEngineStore, {type VoiceState} from '~/stores/voice/MediaEngineFacade';
import type {GatewayHandlerContext} from '../index';

interface VoiceStateUpdatePayload {
	user_id: string;
	channel_id?: string | null;
	guild_id?: string | null;
	session_id?: string;
	connection_id?: string;
	self_mute?: boolean;
	self_deaf?: boolean;
	self_video?: boolean;
	self_stream?: boolean;
	mute?: boolean;
	deaf?: boolean;
	suppress?: boolean;
}

export function handleVoiceStateUpdate(data: VoiceStateUpdatePayload, _context: GatewayHandlerContext): void {
	const guildId = data.guild_id ?? null;
	const voiceState = data as VoiceState;
	MediaEngineStore.handleGatewayVoiceStateUpdate(guildId, voiceState);
}
