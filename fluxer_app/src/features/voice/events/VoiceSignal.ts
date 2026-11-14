// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {dispatchVoiceMeshSignal, parseVoiceMeshSignalData} from '@app/features/voice/engine/mesh/VoiceMeshSignal';

const logger = new Logger('VoiceSignal');

interface VoiceSignalPayload {
	guild_id?: string;
	channel_id: string;
	from: string;
	user_id: string;
	data: unknown;
}

export function handleVoiceSignal(data: VoiceSignalPayload, _context: GatewayHandlerContext): void {
	const signal = parseVoiceMeshSignalData(data.data);
	if (!signal) {
		logger.warn('Dropping malformed voice signal', {from: data.from});
		return;
	}
	dispatchVoiceMeshSignal({
		guildId: data.guild_id ?? null,
		channelId: data.channel_id,
		from: data.from,
		userId: data.user_id,
		data: signal,
	});
}
