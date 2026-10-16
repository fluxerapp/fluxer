// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import VoiceSessionRestore, {type VoiceSessionRestoreSnapshot} from '@app/features/voice/state/VoiceSessionRestore';
import {
	VoiceAccountExitReason,
	type VoiceAccountExitRequest,
	type VoiceAccountIdentity,
} from '@app/features/voice/VoiceAccountLifecyclePort';

function snapshotToRestoreAfterExit(
	request: VoiceAccountExitRequest,
): Omit<VoiceSessionRestoreSnapshot, 'updatedAt'> | null {
	if (request.reason !== VoiceAccountExitReason.ACCOUNT_SWITCH) {
		return null;
	}
	const snapshot = VoiceSessionRestore.getSnapshotForUser(request.userId);
	if (snapshot === null) {
		return null;
	}
	return {
		userId: snapshot.userId,
		guildId: snapshot.guildId,
		channelId: snapshot.channelId,
		selfVideo: snapshot.selfVideo,
		selfStream: snapshot.selfStream,
	};
}

export async function leaveVoiceChannelForAccountExit(request: VoiceAccountExitRequest): Promise<void> {
	const retained = snapshotToRestoreAfterExit(request);
	await MediaEngine.disconnectFromVoiceChannel('user');
	if (retained !== null) {
		VoiceSessionRestore.saveSnapshot(retained);
	}
}

export async function suspendVoiceForAccountRestriction(_account: VoiceAccountIdentity): Promise<void> {
	await MediaEngine.disconnectFromVoiceChannel('server');
}
