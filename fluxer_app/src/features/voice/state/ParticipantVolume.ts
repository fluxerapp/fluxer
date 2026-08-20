// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {isScreenShareAudioPublicationLike, VoiceTrackKind} from '@app/features/voice/engine/VoiceTrackSource';
import {getRemoteVoicePlaybackBoost} from '@app/features/voice/state/RemoteVoicePlaybackBoost';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	clampVoiceTrackTotalGain,
	clampVoiceVolumePercent,
	composeVoiceVolumeGain,
	recalibrateStoredVoiceVolumes,
} from '@app/features/voice/utils/VoiceVolumeUtils';
import type {RemoteAudioTrack, RemoteParticipant, Room} from 'livekit-client';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('ParticipantVolume');
const isRemoteAudioTrack = (track: unknown): track is RemoteAudioTrack =>
	track != null &&
	typeof track === 'object' &&
	'kind' in track &&
	(track as {kind?: unknown}).kind === VoiceTrackKind.Audio;
const idUser = (identity: string): string | null => {
	const m = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return m ? m[1] : null;
};
const idConnection = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)_(.+)$/);
	return match ? match[2] : null;
};

interface VoiceConnectionContextAccess {
	guildId?: string | null;
	channelId?: string | null;
	connectionId?: string | null;
}

function getVoiceConnectionContext(): VoiceConnectionContextAccess {
	try {
		const mediaEngine =
			(
				window as typeof window & {
					_mediaEngineFacade?: VoiceConnectionContextAccess;
					_mediaEngine?: VoiceConnectionContextAccess;
				}
			)._mediaEngineFacade ??
			(window as typeof window & {_mediaEngine?: VoiceConnectionContextAccess})._mediaEngine ??
			null;
		return {
			guildId: mediaEngine?.guildId ?? null,
			channelId: mediaEngine?.channelId ?? null,
			connectionId: mediaEngine?.connectionId ?? null,
		};
	} catch (error) {
		logger.error('Failed to read voice connection context from media engine', {error});
		return {guildId: null, channelId: null, connectionId: null};
	}
}

class ParticipantVolume {
	volumes: Record<string, number> = {};
	localMutes: Record<string, boolean> = {};
	outputVolumeRecalibratedV1 = false;
	connectionVolumesByLocalConnectionId: Record<string, Record<string, number>> = {};
	private listeners = new Set<() => void>();

	constructor() {
		makeAutoObservable<this, 'listeners' | 'notifyListeners'>(
			this,
			{
				listeners: false,
				getVolume: false,
				isLocalMuted: false,
				getConnectionVolume: false,
				notifyListeners: false,
			},
			{autoBind: true},
		);
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'ParticipantVolume', ['volumes', 'localMutes', 'outputVolumeRecalibratedV1']);
		this.applyOutputVolumeRecalibration();
	}

	private applyOutputVolumeRecalibration(): void {
		if (this.outputVolumeRecalibratedV1) {
			return;
		}
		this.volumes = recalibrateStoredVoiceVolumes(this.volumes);
		this.outputVolumeRecalibratedV1 = true;
		this.notifyListeners();
	}

	setVolume(userId: string, volume: number): void {
		const clamped = clampVoiceVolumePercent(volume);
		this.volumes = {
			...this.volumes,
			[userId]: clamped,
		};
		this.notifyListeners();
		logger.debug(`Set volume for ${userId}: ${clamped}`);
	}

	setLocalMute(userId: string, muted: boolean): void {
		this.localMutes = {
			...this.localMutes,
			[userId]: muted,
		};
		this.notifyListeners();
		logger.debug(`Set local mute for ${userId}: ${muted}`);
	}

	setConnectionVolume(connectionId: string, volume: number): void {
		const localConnectionId = getVoiceConnectionContext().connectionId;
		if (!localConnectionId || !connectionId) {
			return;
		}
		const clamped = clampVoiceVolumePercent(volume);
		const existingBucket = this.connectionVolumesByLocalConnectionId[localConnectionId] ?? {};
		this.connectionVolumesByLocalConnectionId = {
			...this.connectionVolumesByLocalConnectionId,
			[localConnectionId]: {
				...existingBucket,
				[connectionId]: clamped,
			},
		};
		this.notifyListeners();
		logger.debug(`Set connection volume for ${connectionId} from ${localConnectionId}: ${clamped}`);
	}

	getVolume(userId: string): number {
		return clampVoiceVolumePercent(this.volumes[userId] ?? 100);
	}

	getConnectionVolume(connectionId: string | null): number {
		if (!connectionId) {
			return 100;
		}
		const localConnectionId = getVoiceConnectionContext().connectionId;
		if (!localConnectionId) {
			return 100;
		}
		const bucket = this.connectionVolumesByLocalConnectionId[localConnectionId];
		return clampVoiceVolumePercent(bucket?.[connectionId] ?? 100);
	}

	isLocalMuted(userId: string): boolean {
		return this.localMutes[userId] ?? false;
	}

	resetUserSettings(userId: string): void {
		const newVolumes = {...this.volumes};
		const newLocalMutes = {...this.localMutes};
		delete newVolumes[userId];
		delete newLocalMutes[userId];
		this.volumes = newVolumes;
		this.localMutes = newLocalMutes;
		this.notifyListeners();
		logger.debug(`Reset settings for ${userId}`);
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private notifyListeners(): void {
		for (const listener of [...this.listeners]) {
			listener();
		}
	}

	applySettingsToRoom(room: Room | null): void {
		if (!room) return;
		room.remoteParticipants.forEach((participant) => {
			this.applySettingsToParticipant(participant);
		});
	}

	applySettingsToParticipant(participant: RemoteParticipant): void {
		const userId = idUser(participant.identity);
		if (!userId) return;
		const connectionId = idConnection(participant.identity);
		const connectionContext = getVoiceConnectionContext();
		const streamKey = connectionId
			? getStreamKey(connectionContext.guildId, connectionContext.channelId, connectionId)
			: null;
		const userVolume = this.getVolume(userId);
		const connectionVolume = this.getConnectionVolume(connectionId);
		const outputVolume = VoiceSettings.getOutputVolume();
		const locallyMuted = this.isLocalMuted(userId);
		const {effectiveDeaf} = getEffectiveAudioState();
		participant.audioTrackPublications.forEach((pub) => {
			const isScreenShareAudio = isScreenShareAudioPublicationLike(pub);
			const streamVolume = streamKey ? StreamAudioPrefs.getVolume(streamKey) : 100;
			const streamMuted = streamKey ? StreamAudioPrefs.isMuted(streamKey) : false;
			const baseVolume = isScreenShareAudio
				? composeVoiceVolumeGain(streamVolume, outputVolume)
				: composeVoiceVolumeGain(userVolume, connectionVolume, outputVolume);
			const playbackBoost = isScreenShareAudio ? 1 : getRemoteVoicePlaybackBoost(participant.identity);
			const nextVolume = clampVoiceTrackTotalGain(baseVolume * playbackBoost);
			const shouldDisable = isScreenShareAudio ? streamMuted || effectiveDeaf : locallyMuted || effectiveDeaf;
			logger.debug('Applying remote audio publication prefs', {
				participantIdentity: participant.identity,
				source: pub.source,
				trackName: pub.trackName,
				trackSid: pub.trackSid,
				isScreenShareAudio,
				streamKey,
				streamVolume,
				streamMuted,
				userVolume,
				connectionVolume,
				outputVolume,
				playbackBoost,
				nextVolume,
				locallyMuted,
				effectiveDeaf,
				shouldDisable,
			});
			const track = pub.track;
			if (isRemoteAudioTrack(track)) {
				try {
					track.setVolume(nextVolume);
				} catch (error) {
					logger.warn(`Failed to set remote track volume for participant ${userId}`, {
						error,
						trackSid: pub.trackSid,
					});
				}
			}
			try {
				if (pub.isDesired) {
					pub.setEnabled(!shouldDisable);
				}
			} catch (error) {
				logger.warn(`Failed to apply enabled state for participant ${userId}`, {
					error,
					trackSid: pub.trackSid,
				});
			}
			if (isScreenShareAudio && streamKey && StreamAudioPrefs.hasEntry(streamKey)) {
				try {
					StreamAudioPrefs.touchStream(streamKey);
				} catch (error) {
					logger.warn(`Failed to touch stream audio prefs for participant ${userId}`, {error, streamKey});
				}
			}
		});
	}
}

export default new ParticipantVolume();
