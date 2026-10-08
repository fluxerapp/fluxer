// SPDX-License-Identifier: AGPL-3.0-or-later

export const VoiceTrackSource = Object.freeze({
	Microphone: 'microphone',
	Camera: 'camera',
	ScreenShare: 'screen_share',
	ScreenShareAudio: 'screen_share_audio',
	Unknown: 'unknown',
});

export type VoiceTrackSource = (typeof VoiceTrackSource)[keyof typeof VoiceTrackSource];
export type PinnableVoiceTrackSource = typeof VoiceTrackSource.Camera | typeof VoiceTrackSource.ScreenShare;

export const VoiceTrackKind = Object.freeze({
	Audio: 'audio',
	Video: 'video',
	Unknown: 'unknown',
});

export type VoiceTrackKind = (typeof VoiceTrackKind)[keyof typeof VoiceTrackKind];

export interface VoiceTrackPublicationSourceLike {
	source?: unknown;
	trackName?: unknown;
	name?: unknown;
	track?: {
		source?: unknown;
		mediaStreamTrack?: {
			label?: string;
		} | null;
	} | null;
	audioTrack?: {
		source?: unknown;
		mediaStreamTrack?: {
			label?: string;
		} | null;
	} | null;
}

export const VoiceConnectionQuality = Object.freeze({
	Excellent: 'excellent',
	Good: 'good',
	Poor: 'poor',
	Lost: 'lost',
	Unknown: 'unknown',
});

export type VoiceConnectionQuality = (typeof VoiceConnectionQuality)[keyof typeof VoiceConnectionQuality];

export function asPinnableVoiceTrackSource(source: unknown): PinnableVoiceTrackSource | null {
	const normalized = asVoiceTrackSource(source);
	if (normalized === VoiceTrackSource.Camera || normalized === VoiceTrackSource.ScreenShare) {
		return normalized;
	}
	return null;
}

export function asVoiceTrackSource(source: unknown): VoiceTrackSource {
	switch (source) {
		case VoiceTrackSource.Microphone:
			return VoiceTrackSource.Microphone;
		case VoiceTrackSource.Camera:
			return VoiceTrackSource.Camera;
		case VoiceTrackSource.ScreenShare:
		case 'screenshare':
			return VoiceTrackSource.ScreenShare;
		case VoiceTrackSource.ScreenShareAudio:
		case 'screenshareAudio':
		case 'screenshare_audio':
		case 'screen_audio':
		case 'system_audio':
			return VoiceTrackSource.ScreenShareAudio;
		default:
			return VoiceTrackSource.Unknown;
	}
}

function normalizeTrackLabel(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	const normalized = value
		.trim()
		.toLowerCase()
		.replace(/[^a-z0-9]/g, '');
	return normalized.length > 0 ? normalized : null;
}

function isScreenShareAudioTrackLabel(value: unknown): boolean {
	const label = normalizeTrackLabel(value);
	return label === 'screenaudio' || label === 'screenshareaudio' || label === 'systemaudio';
}

export function isScreenShareAudioPublicationLike(
	publication: VoiceTrackPublicationSourceLike | null | undefined,
): boolean {
	if (!publication) return false;
	if (asVoiceTrackSource(publication.source) === VoiceTrackSource.ScreenShareAudio) return true;
	if (asVoiceTrackSource(publication.track?.source) === VoiceTrackSource.ScreenShareAudio) return true;
	if (asVoiceTrackSource(publication.audioTrack?.source) === VoiceTrackSource.ScreenShareAudio) return true;
	return (
		isScreenShareAudioTrackLabel(publication.trackName) ||
		isScreenShareAudioTrackLabel(publication.name) ||
		isScreenShareAudioTrackLabel(publication.track?.mediaStreamTrack?.label) ||
		isScreenShareAudioTrackLabel(publication.audioTrack?.mediaStreamTrack?.label)
	);
}

export function asVoiceConnectionQuality(value: unknown): VoiceConnectionQuality {
	switch (value) {
		case VoiceConnectionQuality.Excellent:
			return VoiceConnectionQuality.Excellent;
		case VoiceConnectionQuality.Good:
			return VoiceConnectionQuality.Good;
		case VoiceConnectionQuality.Poor:
			return VoiceConnectionQuality.Poor;
		case VoiceConnectionQuality.Lost:
			return VoiceConnectionQuality.Lost;
		default:
			return VoiceConnectionQuality.Unknown;
	}
}
