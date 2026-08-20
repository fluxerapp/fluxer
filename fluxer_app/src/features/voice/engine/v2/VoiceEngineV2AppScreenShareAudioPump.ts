// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ScreenShareAudioPumpDiagnostics {
	active: boolean;
	captureId: string | null;
	sampleRate: number | null;
	channels: number | null;
	usesNativeSink: boolean;
	publishStrategy: 'eager' | 'lazy' | 'none';
	publishedFormatKey: string | null;
	eagerPublish: 'succeeded' | 'failed' | 'skipped' | null;
	eagerPublishError: string | null;
	droppedPushFrames: number;
	pendingPushFrames: number;
}

export function getScreenShareAudioPumpDiagnostics(): ScreenShareAudioPumpDiagnostics {
	return {
		active: false,
		captureId: null,
		sampleRate: null,
		channels: null,
		usesNativeSink: false,
		publishStrategy: 'none',
		publishedFormatKey: null,
		eagerPublish: null,
		eagerPublishError: null,
		droppedPushFrames: 0,
		pendingPushFrames: 0,
	};
}
