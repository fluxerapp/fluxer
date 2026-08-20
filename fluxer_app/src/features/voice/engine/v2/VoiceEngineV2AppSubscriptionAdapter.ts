// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {ScreenShareSubscriptionManager} from '@app/features/voice/engine/ScreenShareSubscriptionManager';
import {Store} from '@app/features/voice/engine/Store';
import {VideoSubscriptionManager} from '@app/features/voice/engine/VideoSubscriptionManager';
import type {
	VoiceMediaGraphSubscriptionContext,
	VoiceMediaGraphVideoQuality,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {isScreenShareAudioPublicationLike, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
} from '@fluxer/voice_engine_v2';
import type {RemoteTrackPublication, Room} from 'livekit-client';

const logger = new Logger('VoiceEngineV2AppSubscriptionAdapter');

export type VideoQualityLevel = VoiceMediaGraphVideoQuality;

type VoiceEngineV2AppSubscriptionSource = 'camera' | 'screen';

function normalizeSubscriptionSource(source: string): VoiceEngineV2AppSubscriptionSource | null {
	switch (source) {
		case VoiceTrackSource.Camera:
			return 'camera';
		case VoiceTrackSource.ScreenShare:
		case 'screen':
		case 'screenshare':
			return 'screen';
		default:
			return null;
	}
}

function screenShareContextForQuality(quality: VideoQualityLevel | undefined): VoiceMediaGraphSubscriptionContext {
	return quality === 'high' ? 'focused' : 'carousel';
}

class VoiceEngineV2AppSubscriptionAdapter extends Store {
	private room: Room | null = null;
	private videoManager = new VideoSubscriptionManager();
	private screenShareManager = new ScreenShareSubscriptionManager();

	constructor() {
		super();
		this.videoManager.subscribe(() => this.emitChange());
		this.screenShareManager.subscribe(() => this.emitChange());
		logger.debug('Initialized');
	}

	setRoom(room: Room | null): void {
		if (this.room === room) return;
		if (this.room) {
			this.cleanup();
		}
		this.update(() => {
			this.room = room;
		});
		this.videoManager.setRoom(room);
		this.screenShareManager.setRoom(room);
		if (room) {
			logger.info('Room set', {participantCount: room.remoteParticipants.size});
		} else {
			logger.info('Room cleared');
		}
	}

	async setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void> {
		const participant = this.room?.remoteParticipants.get(options.participantIdentity);
		if (!participant) return;
		participant.audioTrackPublications.forEach((publication) => {
			if (isScreenShareAudioPublicationLike(publication)) {
				logger.debug('Skipping screen share audio publication in participant volume update', {
					participantIdentity: options.participantIdentity,
					trackSid: publication.trackSid,
				});
				return;
			}
			const track = publication.audioTrack;
			if (track && 'setVolume' in track && typeof track.setVolume === 'function') {
				track.setVolume(options.volume);
			}
		});
	}

	async setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void> {
		const source = normalizeSubscriptionSource(options.source);
		if (!source) {
			logger.debug('Ignoring unsupported v2 remote track subscription source', {
				participantIdentity: options.participantIdentity,
				source: options.source,
				subscribed: options.subscribed,
			});
			return;
		}
		if (source === 'camera') {
			if (options.subscribed) {
				this.subscribeToVideo(options.participantIdentity, null, options.quality ?? 'low');
				if (options.enabled !== undefined) {
					this.setVideoEnabled(options.participantIdentity, options.enabled);
				}
				if (options.quality !== undefined) {
					this.setVideoQuality(options.participantIdentity, options.quality);
				}
			} else {
				this.unsubscribeFromVideo(options.participantIdentity);
			}
			return;
		}
		if (options.subscribed) {
			this.subscribeToScreenShare(
				options.participantIdentity,
				null,
				options.enabled === false ? 'hidden' : screenShareContextForQuality(options.quality),
			);
			if (options.enabled === false) {
				this.setScreenShareContext(options.participantIdentity, 'hidden');
			} else if (options.quality !== undefined) {
				this.setScreenShareContext(options.participantIdentity, screenShareContextForQuality(options.quality));
			}
		} else {
			this.unsubscribeFromScreenShare(options.participantIdentity);
		}
	}

	cleanup(): void {
		logger.debug('Cleaning up all subscriptions');
		this.videoManager.cleanup();
		this.screenShareManager.cleanup();
		this.update(() => {
			this.room = null;
		});
		this.videoManager.setRoom(null);
		this.screenShareManager.setRoom(null);
		logger.info('All subscriptions cleaned up');
	}

	subscribeToVideo(
		participantIdentity: string,
		element: HTMLElement | null,
		initialQuality: VideoQualityLevel = 'low',
	): void {
		this.videoManager.subscribeToParticipant(participantIdentity, element, initialQuality);
	}

	unsubscribeFromVideo(participantIdentity: string): void {
		this.videoManager.unsubscribeFromParticipant(participantIdentity);
	}

	setVideoEnabled(participantIdentity: string, enabled: boolean): void {
		this.videoManager.setEnabled(participantIdentity, enabled);
	}

	setVideoQuality(participantIdentity: string, quality: VideoQualityLevel): void {
		this.videoManager.setQuality(participantIdentity, quality);
	}

	subscribeToScreenShare(
		participantIdentity: string,
		element: HTMLElement | null,
		context: VoiceMediaGraphSubscriptionContext = 'carousel',
	): void {
		this.screenShareManager.subscribeToParticipant(participantIdentity, element, context);
	}

	unsubscribeFromScreenShare(participantIdentity: string): void {
		this.screenShareManager.unsubscribeFromParticipant(participantIdentity);
	}

	setScreenShareContext(participantIdentity: string, context: VoiceMediaGraphSubscriptionContext): void {
		this.screenShareManager.setContext(participantIdentity, context);
	}

	isVideoSubscribed(participantIdentity: string): boolean {
		return this.videoManager.isSubscribed(participantIdentity);
	}

	isScreenShareSubscribed(participantIdentity: string): boolean {
		return this.screenShareManager.isSubscribed(participantIdentity);
	}

	reattachScreenShareAfterPublish(participantIdentity: string, publication?: RemoteTrackPublication): void {
		this.screenShareManager.reattachAfterPublish(participantIdentity, publication);
	}

	reconcileSubscriptions(): void {
		const commands = voiceMediaGraphStore.takeSubscriptionCommands({type: 'subscription.reconcile'});
		for (const command of commands) {
			this.videoManager.applyReconciledCommand(command);
			this.screenShareManager.applyReconciledCommand(command);
		}
	}

	reattachVideoAfterPublish(participantIdentity: string): void {
		this.videoManager.reattachAfterPublish(participantIdentity);
	}

	getVideoQuality(participantIdentity: string): VideoQualityLevel | null {
		return this.videoManager.getQuality(participantIdentity);
	}

	getScreenShareContext(participantIdentity: string): VoiceMediaGraphSubscriptionContext | null {
		return this.screenShareManager.getContext(participantIdentity);
	}
}

const voiceEngineV2AppSubscriptionAdapter = new VoiceEngineV2AppSubscriptionAdapter();

export default voiceEngineV2AppSubscriptionAdapter;
