// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ScreenShareAudioCaptureDebugInfo {
	captureId?: string | null;
	platform?: string | null;
	sourceId?: string | null;
	sourceKind?: string | null;
	sourceMode?: string | null;
	backend?: string | null;
	reason?: string | null;
	detail?: string | null;
}

export class ScreenShareAudioCaptureError extends Error {
	readonly debugInfo: ScreenShareAudioCaptureDebugInfo;

	constructor(debugInfo: ScreenShareAudioCaptureDebugInfo) {
		super('Screen share audio was requested but its capture route could not start');
		this.name = 'ScreenShareAudioCaptureError';
		this.debugInfo = debugInfo;
	}
}

export function isScreenShareAudioCaptureError(error: unknown): error is ScreenShareAudioCaptureError {
	return error instanceof ScreenShareAudioCaptureError;
}
