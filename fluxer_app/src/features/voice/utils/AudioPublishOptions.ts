// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {TrackPublishOptions} from 'livekit-client';

export const OPUS_MAX_AUDIO_BITRATE_BPS = 510000;
export const VOICE_CHANNEL_MIN_AUDIO_BITRATE_BPS = 8000;
export const STEREO_VOICE_MIN_AUDIO_BITRATE_BPS = 128000;

export function normaliseAudioBitrateBps(value: number | null | undefined): number | undefined {
	if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return undefined;
	const rounded = Math.round(value);
	const bitsPerSecond = rounded >= 8 && rounded <= 512 ? rounded * 1000 : rounded;
	return Math.min(Math.max(bitsPerSecond, VOICE_CHANNEL_MIN_AUDIO_BITRATE_BPS), OPUS_MAX_AUDIO_BITRATE_BPS);
}

export function buildMicrophonePublishOptions(
	channelBitrate: number | null | undefined,
	processingMode: VoiceProcessingMode,
	stereoCapture = false,
): TrackPublishOptions | undefined {
	const maxBitrate = normaliseAudioBitrateBps(channelBitrate);
	if (!maxBitrate) return undefined;
	const studioMode = processingMode === 'studio';
	const bitrateAllowsStereo = maxBitrate >= STEREO_VOICE_MIN_AUDIO_BITRATE_BPS;
	return {
		audioPreset: {
			maxBitrate,
			priority: 'high',
		},
		red: true,
		...(studioMode
			? {
					dtx: false,
					forceStereo: bitrateAllowsStereo,
				}
			: stereoCapture && !bitrateAllowsStereo
				? {forceStereo: false}
				: {}),
	};
}

export const SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS: TrackPublishOptions = {
	audioPreset: {
		maxBitrate: OPUS_MAX_AUDIO_BITRATE_BPS,
		priority: 'high',
	},
	dtx: false,
	forceStereo: true,
	red: true,
	source: VoiceTrackSource.ScreenShareAudio as TrackPublishOptions['source'],
	stream: 'screen_share',
};

export function prepareHighFidelityScreenShareAudioTrack(track: MediaStreamTrack | undefined): void {
	if (!track) return;
	try {
		(
			track as MediaStreamTrack & {
				contentHint?: string;
			}
		).contentHint = 'music';
	} catch {}
	if (typeof track.applyConstraints === 'function') {
		void track
			.applyConstraints({
				channelCount: 2,
				sampleRate: 48000,
				echoCancellation: false,
				noiseSuppression: false,
				autoGainControl: false,
			})
			.catch(() => undefined);
	}
}
