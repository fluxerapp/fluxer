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

import type {LocalTrackPublication, RemoteAudioTrack, Room} from 'livekit-client';
import {Track} from 'livekit-client';
import {Logger} from '~/lib/Logger';
import KeybindStore from '~/stores/KeybindStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import ParticipantVolumeStore from '~/stores/ParticipantVolumeStore';
import type {VoiceState} from './VoiceStateManager';

const logger = new Logger('VoiceAudioManager');

const isRemoteAudioTrack = (track: unknown): track is RemoteAudioTrack =>
	track != null && typeof track === 'object' && 'kind' in track && (track as {kind: string}).kind === Track.Kind.Audio;

const extractUserId = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return match ? match[1] : null;
};

export function applyLocalAudioPreferencesForUser(userId: string, room: Room | null): void {
	if (!room) {
		logger.warn('[applyLocalAudioPreferencesForUser] No room');
		return;
	}

	const selfDeaf = LocalVoiceStateStore.getSelfDeaf();

	room.remoteParticipants.forEach((p) => {
		if (extractUserId(p.identity) !== userId) return;

		p.audioTrackPublications.forEach((pub) => {
			try {
				const volume = ParticipantVolumeStore.getVolume(userId);
				const locallyMuted = ParticipantVolumeStore.isLocalMuted(userId);

				const track = pub.track;
				if (isRemoteAudioTrack(track)) {
					track.setVolume(volume / 100);
				}

				const shouldDisable = locallyMuted || selfDeaf;
				pub.setEnabled(!shouldDisable);
			} catch (error) {
				logger.warn(`[applyLocalAudioPreferencesForUser] Failed for user ${userId}`, {error});
			}
		});
	});
}

export function applyAllLocalAudioPreferences(room: Room | null): void {
	if (!room) {
		logger.warn('[applyAllLocalAudioPreferences] No room');
		return;
	}

	const selfDeaf = LocalVoiceStateStore.getSelfDeaf();
	ParticipantVolumeStore.applySettingsToRoom(room, selfDeaf);
}

export function applyPushToTalkHold(
	held: boolean,
	room: Room | null,
	getCurrentUserVoiceState: () => VoiceState | null,
	syncVoiceState: (partial: {self_mute?: boolean}) => void,
): void {
	KeybindStore.setPushToTalkHeld(held);
	if (!KeybindStore.isPushToTalkEnabled()) return;

	const serverVoiceState = getCurrentUserVoiceState();
	if (serverVoiceState?.mute) return;

	const userMuted = LocalVoiceStateStore.getHasUserSetMute() && LocalVoiceStateStore.getSelfMute();
	const shouldMute = userMuted || !held;

	applyLocalMuteState(shouldMute, room, syncVoiceState);
}

export function handlePushToTalkModeChange(
	room: Room | null,
	getCurrentUserVoiceState: () => VoiceState | null,
	syncVoiceState: (partial: {self_mute?: boolean}) => void,
): void {
	const serverVoiceState = getCurrentUserVoiceState();
	if (serverVoiceState?.mute) return;

	if (KeybindStore.isPushToTalkEffective()) {
		KeybindStore.setPushToTalkHeld(false);
		KeybindStore.resetPushToTalkState();
		if (!LocalVoiceStateStore.getHasUserSetMute()) {
			applyLocalMuteState(true, room, syncVoiceState);
		}
	} else if (!LocalVoiceStateStore.getHasUserSetMute()) {
		applyLocalMuteState(false, room, syncVoiceState);
	}
}

export function getMuteReason(voiceState: VoiceState | null): 'guild' | 'push_to_talk' | 'self' | null {
	const isGuildMuted = voiceState?.mute ?? false;
	if (isGuildMuted) return 'guild';

	const selfMuted = voiceState?.self_mute ?? LocalVoiceStateStore.getSelfMute();
	if (KeybindStore.isPushToTalkEffective() && KeybindStore.isPushToTalkMuted(selfMuted)) return 'push_to_talk';
	if (selfMuted) return 'self';
	return null;
}

export function applyLocalMuteState(
	muted: boolean,
	room: Room | null,
	syncVoiceState: (partial: {self_mute?: boolean}) => void,
): void {
	const targetMute = LocalVoiceStateStore.getSelfDeaf() ? true : muted;
	const currentMute = LocalVoiceStateStore.getSelfMute();

	if (currentMute === targetMute) {
		return;
	}

	LocalVoiceStateStore.updateSelfMute(targetMute);

	if (room?.localParticipant) {
		room.localParticipant.audioTrackPublications.forEach((publication: LocalTrackPublication) => {
			const track = publication.track;
			if (!track) return;
			const operation = targetMute ? track.mute() : track.unmute();
			operation.catch((error) =>
				logger.error(targetMute ? 'Failed to mute local track' : 'Failed to unmute local track', {error}),
			);
		});
	}

	syncVoiceState({self_mute: targetMute});
}
