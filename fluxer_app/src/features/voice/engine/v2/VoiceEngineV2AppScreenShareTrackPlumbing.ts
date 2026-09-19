// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	applyScreenShareContentHint,
	enforceScreenShareSenderParameters,
	getScreenShareBackupSenders,
	logger,
	resolveActiveScreenShareTarget,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {prepareHighFidelityScreenShareAudioTrack} from '@app/features/voice/utils/AudioPublishOptions';
import type {ScreenShareContentSource} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	type LocalAudioTrack,
	type LocalParticipant,
	type LocalVideoTrack,
	ParticipantEvent,
	type Room,
	RoomEvent,
	type Track as SdkTrack,
	Track,
	TrackEvent,
	type VideoCodec,
} from 'livekit-client';

function isVideoCodecValue(value: unknown): value is VideoCodec {
	return value === 'av1' || value === 'h265' || value === 'h264' || value === 'vp9' || value === 'vp8';
}

export interface VoiceEngineV2AppScreenShareTrackPlumbingHost {
	getActiveContentSource(): ScreenShareContentSource;
}

export class VoiceEngineV2AppScreenShareTrackPlumbing {
	private readonly host: VoiceEngineV2AppScreenShareTrackPlumbingHost;
	private keepAliveElement: HTMLVideoElement | null = null;
	private keepAliveTrack: LocalVideoTrack | null = null;
	private keyFrameRequestDisposer: (() => void) | null = null;
	private senderParameterDisposer: (() => void) | null = null;

	constructor(host: VoiceEngineV2AppScreenShareTrackPlumbingHost) {
		assert.ok(host, 'track plumbing host is required');
		assert.equal(typeof host.getActiveContentSource, 'function', 'host must expose getActiveContentSource');
		this.host = host;
	}

	private getOrCreateKeepAliveElement(): HTMLVideoElement | null {
		if (typeof document === 'undefined' || !document.body) return null;
		if (this.keepAliveElement?.isConnected) return this.keepAliveElement;
		const element = document.createElement('video');
		element.autoplay = true;
		element.muted = true;
		element.playsInline = true;
		element.setAttribute('aria-hidden', 'true');
		element.setAttribute('data-flx', 'voice.screen-share-keepalive');
		Object.assign(element.style, {
			position: 'fixed',
			left: '0',
			top: '0',
			width: '2px',
			height: '2px',
			opacity: '0.001',
			visibility: 'hidden',
			pointerEvents: 'none',
			zIndex: '0',
		});
		document.body.appendChild(element);
		this.keepAliveElement = element;
		return element;
	}

	ensureKeepAliveSink(participant: LocalParticipant, preferredTrack?: LocalVideoTrack): void {
		const screenShareTrack =
			preferredTrack ??
			(participant.getTrackPublication(Track.Source.ScreenShare)?.videoTrack as LocalVideoTrack | undefined);
		if (!screenShareTrack || screenShareTrack.mediaStreamTrack.readyState === 'ended') {
			this.clearKeepAliveSink();
			return;
		}
		const element = this.getOrCreateKeepAliveElement();
		if (!element) return;
		if (this.keepAliveTrack && this.keepAliveTrack !== screenShareTrack) {
			try {
				this.keepAliveTrack.detach(element);
			} catch (error) {
				logger.debug('Failed to detach stale screen-share keepalive sink', {error});
			}
		}
		this.keepAliveTrack = screenShareTrack;
		try {
			screenShareTrack.attach(element);
			element.pause();
		} catch (error) {
			logger.warn('Failed to attach screen-share keepalive sink', {error});
		}
	}

	clearKeepAliveSink(): void {
		const element = this.keepAliveElement;
		const track = this.keepAliveTrack;
		this.keepAliveElement = null;
		this.keepAliveTrack = null;
		if (track && element) {
			try {
				track.detach(element);
			} catch (error) {
				logger.debug('Failed to detach screen-share keepalive sink', {error});
			}
		}
		if (element) {
			element.pause();
			element.srcObject = null;
			element.remove();
		}
	}

	applyContentHint(
		participant: LocalParticipant,
		contentSource: ScreenShareContentSource = this.host.getActiveContentSource(),
		preferredTrack?: LocalVideoTrack,
	): void {
		const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
		const track = (preferredTrack ?? publication?.videoTrack) as LocalVideoTrack | undefined;
		if (!track) {
			return;
		}
		const {contentHint} = resolveActiveScreenShareTarget(contentSource);
		applyScreenShareContentHint(track.mediaStreamTrack, contentHint);
		for (const backup of getScreenShareBackupSenders(track)) {
			applyScreenShareContentHint(backup.sender.track ?? undefined, contentHint);
		}
	}

	applyAudioContentHint(participant: LocalParticipant): void {
		const publication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		const track =
			publication?.audioTrack?.mediaStreamTrack ??
			(publication?.track as LocalAudioTrack | undefined)?.mediaStreamTrack;
		prepareHighFidelityScreenShareAudioTrack(track);
	}

	async enforceSenderParameters(
		participant: LocalParticipant,
		publishedCodec: VideoCodec | undefined,
		preferredTrack?: LocalVideoTrack,
	): Promise<boolean> {
		const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
		const track = preferredTrack ?? (publication?.videoTrack as LocalVideoTrack | undefined);
		if (!track) {
			logger.warn('No screen share track found for sender parameter enforcement');
			return false;
		}
		const applied = await this.enforceTrackSenderParameters(track, publishedCodec);
		this.bindSenderParameterReapply(participant, track, publishedCodec);
		return applied;
	}

	async enforceTrackSenderParameters(track: LocalVideoTrack, publishedCodec: VideoCodec | undefined): Promise<boolean> {
		const target = ActiveScreenShareSource.getTarget();
		if (target === null) return false;
		let applied = track.sender
			? (await enforceScreenShareSenderParameters(track.sender, target, {publishedCodec})).applied
			: false;
		for (const backup of getScreenShareBackupSenders(track)) {
			const codecOverride = isVideoCodecValue(backup.codec) ? backup.codec : undefined;
			const backupApplied = await enforceScreenShareSenderParameters(backup.sender, target, {
				codecOverride,
				publishedCodec,
			});
			applied = applied && backupApplied.applied;
		}
		return applied;
	}

	private async enforceBackupSenderParameters(
		sender: RTCRtpSender,
		codecOverride: VideoCodec | undefined,
		publishedCodec: VideoCodec | undefined,
	): Promise<void> {
		applyScreenShareContentHint(
			sender.track ?? undefined,
			resolveActiveScreenShareTarget(this.host.getActiveContentSource()).contentHint,
		);
		const target = ActiveScreenShareSource.getTarget();
		if (target === null) return;
		await enforceScreenShareSenderParameters(sender, target, {codecOverride, publishedCodec});
	}

	cleanupSenderParameterReapply(): void {
		this.senderParameterDisposer?.();
		this.senderParameterDisposer = null;
	}

	bindSenderParameterReapply(
		participant: LocalParticipant,
		track: LocalVideoTrack,
		publishedCodec: VideoCodec | undefined,
	): void {
		this.cleanupSenderParameterReapply();
		const reapply = (): void => {
			void this.enforceTrackSenderParameters(track, publishedCodec).catch((error) => {
				logger.warn('Failed to reapply screen share sender parameters after track restart', {error});
			});
		};
		const onLocalSenderCreated = (sender: RTCRtpSender, senderTrack: SdkTrack, codec?: VideoCodec): void => {
			if (senderTrack !== track || sender === track.sender) return;
			void this.enforceBackupSenderParameters(sender, codec, publishedCodec).catch((error) => {
				logger.warn('Failed to apply screen share sender parameters to a backup sender', {error, codec});
			});
		};
		track.on(TrackEvent.Restarted, reapply);
		participant.on(ParticipantEvent.LocalSenderCreated, onLocalSenderCreated);
		this.senderParameterDisposer = () => {
			track.off(TrackEvent.Restarted, reapply);
			participant.off(ParticipantEvent.LocalSenderCreated, onLocalSenderCreated);
		};
	}

	cleanupKeyFrameRequests(): void {
		this.keyFrameRequestDisposer?.();
		this.keyFrameRequestDisposer = null;
	}

	bindKeyFrameRequests(room: Room | null, participant: LocalParticipant, preferredTrack?: LocalVideoTrack): void {
		this.cleanupKeyFrameRequests();
		if (!room || VoiceSettings.getStreamingMode() !== 'screenshare') return;
		const requestKeyFrame = (): void => {
			const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
			const track = preferredTrack ?? (publication?.videoTrack as LocalVideoTrack | undefined);
			const sender = track?.sender as
				| (RTCRtpSender & {
						generateKeyFrame?: () => Promise<void>;
				  })
				| undefined;
			if (typeof sender?.generateKeyFrame !== 'function') return;
			void sender.generateKeyFrame().catch((error) => {
				logger.debug('Failed to generate screen-share keyframe on participant join', {error});
			});
		};
		room.on(RoomEvent.ParticipantConnected, requestKeyFrame);
		this.keyFrameRequestDisposer = () => {
			room.off(RoomEvent.ParticipantConnected, requestKeyFrame);
		};
	}
}
