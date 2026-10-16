// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {AudioTrackFeature, type TrackInfo} from '@livekit/protocol';
import {TrackEvent} from '../events.ts';
import type {LoggerOptions} from '../types.ts';
import {isAudioTrack, isVideoTrack} from '../utils.ts';
import type LocalAudioTrack from './LocalAudioTrack.ts';
import type LocalTrack from './LocalTrack.ts';
import type LocalVideoTrack from './LocalVideoTrack.ts';
import type {TrackPublishOptions} from './options.ts';
import type {Track} from './Track.ts';
import {TrackPublication} from './TrackPublication.ts';

export default class LocalTrackPublication extends TrackPublication {
	override track?: LocalTrack = undefined;

	options?: TrackPublishOptions;

	get isUpstreamPaused() {
		return this.track?.isUpstreamPaused;
	}

	constructor(kind: Track.Kind, ti: TrackInfo, track?: LocalTrack, loggerOptions?: LoggerOptions) {
		super(kind, ti.sid, ti.name, loggerOptions);

		this.updateInfo(ti);
		this.setTrack(track);
	}

	override setTrack(track?: Track) {
		if (this.track) {
			this.track.off(TrackEvent.Ended, this.handleTrackEnded);
			this.track.off(TrackEvent.CpuConstrained, this.handleCpuConstrained);
		}

		super.setTrack(track);

		if (track) {
			track.on(TrackEvent.Ended, this.handleTrackEnded);
			track.on(TrackEvent.CpuConstrained, this.handleCpuConstrained);
		}
	}

	override get isMuted(): boolean {
		if (this.track) {
			return this.track.isMuted;
		}
		return super.isMuted;
	}

	override get audioTrack(): LocalAudioTrack | undefined {
		return super.audioTrack as LocalAudioTrack | undefined;
	}

	override get videoTrack(): LocalVideoTrack | undefined {
		return super.videoTrack as LocalVideoTrack | undefined;
	}

	get isLocal() {
		return true;
	}

	async mute() {
		return this.track?.mute();
	}

	async unmute() {
		return this.track?.unmute();
	}

	async pauseUpstream() {
		await this.track?.pauseUpstream();
	}

	async resumeUpstream() {
		await this.track?.resumeUpstream();
	}

	getTrackFeatures() {
		if (isAudioTrack(this.track)) {
			const settings = this.track!.getSourceTrackSettings();
			const features: Set<AudioTrackFeature> = new Set();
			if (settings.autoGainControl) {
				features.add(AudioTrackFeature.TF_AUTO_GAIN_CONTROL);
			}
			if (settings.echoCancellation) {
				features.add(AudioTrackFeature.TF_ECHO_CANCELLATION);
			}
			if (settings.noiseSuppression) {
				features.add(AudioTrackFeature.TF_NOISE_SUPPRESSION);
			}
			if (this.options?.forceStereo ?? (settings.channelCount !== undefined && settings.channelCount > 1)) {
				features.add(AudioTrackFeature.TF_STEREO);
			}
			if (!this.options?.dtx) {
				features.add(AudioTrackFeature.TF_NO_DTX);
			}
			if (this.track.enhancedNoiseCancellation) {
				features.add(AudioTrackFeature.TF_ENHANCED_NOISE_CANCELLATION);
			}
			return Array.from(features.values());
		} else return [];
	}

	handleTrackEnded = () => {
		this.emit(TrackEvent.Ended);
	};

	private handleCpuConstrained = () => {
		if (this.track && isVideoTrack(this.track)) {
			this.emit(TrackEvent.CpuConstrained, this.track);
		}
	};
}
