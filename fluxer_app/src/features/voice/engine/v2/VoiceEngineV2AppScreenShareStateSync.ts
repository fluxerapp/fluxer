// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	getVoiceConnectionContextFromMediaEngine,
	syncLocalVoiceStateWithServer,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';

type VoiceScreenShareStateReason = 'user' | 'server';

export interface VoiceScreenShareStateOptions {
	forceSync?: boolean;
	reason: VoiceScreenShareStateReason;
	sendUpdate?: boolean;
}

export function applyVoiceEngineV2AppScreenShareState(enabled: boolean, options: VoiceScreenShareStateOptions): void {
	assert.equal(typeof enabled, 'boolean', 'screen-share state must be a boolean');
	assert.ok(
		typeof options.forceSync === 'boolean' || options.forceSync === undefined,
		'screen-share state forceSync must be a boolean when present',
	);
	const {forceSync = false, sendUpdate = true} = options;
	const changed = LocalVoiceState.getSelfStream() !== enabled;
	if (changed) {
		LocalVoiceState.updateSelfStream(enabled);
	}
	if (!sendUpdate || (!changed && !forceSync)) return;
	const connection = getVoiceConnectionContextFromMediaEngine();
	if (!connection || connection.disconnecting || !connection.channelId || !connection.connectionId) return;
	syncLocalVoiceStateWithServer({self_stream: enabled});
}
