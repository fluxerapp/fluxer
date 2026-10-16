// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import {Logger} from '@app/features/platform/utils/AppLogger';
import EntranceSoundPlaybackEngine from '@app/features/voice/engine/EntranceSoundPlaybackEngine';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {waitForVoiceJoinChimeSequence} from '@app/features/voice/engine/VoiceSelfJoinChime';

const logger = new Logger('EntranceSoundPlay');

interface EntranceSoundPlayPayload {
	user_id: string;
	channel_id: string;
	guild_id?: string | null;
	sound_id: string;
	hash: string;
	url: string;
	duration_ms: number;
	content_type: string;
}

export function handleEntranceSoundPlay(data: EntranceSoundPlayPayload, _context: GatewayHandlerContext): void {
	if (!MediaEngine.connected) return;
	if (MediaEngine.channelId !== data.channel_id) {
		logger.debug('Ignoring entrance sound for a channel we are not in', {
			eventChannelId: data.channel_id,
			localChannelId: MediaEngine.channelId,
		});
		return;
	}
	void waitForVoiceJoinChimeSequence({
		userId: data.user_id,
		channelId: data.channel_id,
	}).then((result) => {
		if (result === 'expired-before-start') return;
		if (!MediaEngine.connected || MediaEngine.channelId !== data.channel_id) return;
		return EntranceSoundPlaybackEngine.play({
			userId: data.user_id,
			hash: data.hash,
			url: data.url,
			durationMs: data.duration_ms,
		});
	});
}
