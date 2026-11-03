// SPDX-License-Identifier: AGPL-3.0-or-later

import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {ME} from '@fluxer/constants/src/AppConstants';

export interface OpenAndWatchStreamParams {
	streamKey: string;
	guildId: string | null;
	channelId: string | null;
	userId: string;
	connectionId: string;
}

export interface OpenAndWatchStreamActions {
	startWatching: () => void;
	markPending: () => void;
}

function selectStreamChannel(guildId: string | null, channelId: string | null): void {
	if (!channelId) return;
	NavigationCommands.selectChannel(guildId ?? ME, channelId);
}

function isConnectedToStreamChannel(guildId: string | null, channelId: string | null): boolean {
	if (!channelId) return false;
	return MediaEngine.channelId === channelId && MediaEngine.guildId === (guildId ?? null);
}

export function applyStreamWatchFocus(params: {
	participantIdentity: string;
	guildId: string | null;
	channelId: string | null;
}): void {
	VoiceCallLayoutCommands.setLayoutMode('focus');
	VoiceCallLayoutCommands.setPinnedParticipant(params.participantIdentity, VoiceTrackSource.ScreenShare);
	selectStreamChannel(params.guildId, params.channelId);
}

export function openAndWatchStream(params: OpenAndWatchStreamParams, actions: OpenAndWatchStreamActions): void {
	const {streamKey, guildId, channelId, userId, connectionId} = params;
	if (!streamKey || !userId || !connectionId) return;
	const participantIdentity = buildVoiceParticipantIdentity(userId, connectionId);
	actions.startWatching();
	if (isConnectedToStreamChannel(guildId, channelId)) {
		applyStreamWatchFocus({participantIdentity, guildId, channelId});
		return;
	}
	actions.markPending();
}
