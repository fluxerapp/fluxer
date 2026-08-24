// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import voiceEngineV2AppConnectionHostAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppConnectionHostAdapter';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceSessionRestore, {VOICE_SESSION_RESTORE_HEARTBEAT_MS} from '@app/features/voice/state/VoiceSessionRestore';

export interface VoiceSessionRestoreSyncHandle {
	save(): void;
	dispose(): void;
}

interface VoiceSessionRestoreSyncState {
	currentUserId: string | null;
	connected: boolean;
	guildId: string | null;
	channelId: string | null;
	selfVideo: boolean;
	selfStream: boolean;
}

function readVoiceSessionRestoreSyncState(): VoiceSessionRestoreSyncState {
	return {
		currentUserId: Authentication?.currentUserId ?? null,
		connected: voiceEngineV2AppConnectionHostAdapter.connected,
		guildId: voiceEngineV2AppConnectionHostAdapter.guildId,
		channelId: voiceEngineV2AppConnectionHostAdapter.channelId,
		selfVideo: LocalVoiceState.getSelfVideo(),
		selfStream: LocalVoiceState.getSelfStream(),
	};
}

function areVoiceSessionRestoreSyncStatesEqual(
	previous: VoiceSessionRestoreSyncState,
	current: VoiceSessionRestoreSyncState,
): boolean {
	return (
		current.currentUserId === previous.currentUserId &&
		current.connected === previous.connected &&
		current.guildId === previous.guildId &&
		current.channelId === previous.channelId &&
		current.selfVideo === previous.selfVideo &&
		current.selfStream === previous.selfStream
	);
}

export function saveCurrentVoiceSessionRestoreSnapshot(): void {
	const currentUserId = Authentication?.currentUserId ?? null;
	const connected = voiceEngineV2AppConnectionHostAdapter.connected;
	const guildId = voiceEngineV2AppConnectionHostAdapter.guildId;
	const channelId = voiceEngineV2AppConnectionHostAdapter.channelId;
	if (!connected || !currentUserId || !channelId) {
		return;
	}
	VoiceSessionRestore.saveSnapshot({
		userId: currentUserId,
		guildId,
		channelId,
		selfVideo: LocalVoiceState.getSelfVideo(),
		selfStream: LocalVoiceState.getSelfStream(),
	});
}

export function createVoiceSessionRestoreSync(): VoiceSessionRestoreSyncHandle {
	let previousState: VoiceSessionRestoreSyncState | null = null;
	const syncVoiceSessionRestore = () => {
		const currentState = readVoiceSessionRestoreSyncState();
		if (previousState !== null && areVoiceSessionRestoreSyncStatesEqual(previousState, currentState)) {
			return;
		}
		previousState = currentState;
		saveCurrentVoiceSessionRestoreSnapshot();
	};
	const saveCurrentSnapshot = () => saveCurrentVoiceSessionRestoreSnapshot();
	const unsubscribeConnectionHost = voiceEngineV2AppConnectionHostAdapter.subscribe(syncVoiceSessionRestore);
	const unsubscribeLocalVoiceState = LocalVoiceState.subscribe(syncVoiceSessionRestore);
	const heartbeatIntervalId = window.setInterval(saveCurrentSnapshot, VOICE_SESSION_RESTORE_HEARTBEAT_MS);
	window.addEventListener('pagehide', saveCurrentSnapshot);
	window.addEventListener('beforeunload', saveCurrentSnapshot);
	return {
		save: saveCurrentVoiceSessionRestoreSnapshot,
		dispose: () => {
			window.clearInterval(heartbeatIntervalId);
			window.removeEventListener('pagehide', saveCurrentSnapshot);
			window.removeEventListener('beforeunload', saveCurrentSnapshot);
			unsubscribeConnectionHost();
			unsubscribeLocalVoiceState();
		},
	};
}
