// SPDX-License-Identifier: AGPL-3.0-or-later

import {getNativeAudioCaptureDiagnosticState} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {recordBridgeFrame, startBridgeStats} from '@app/features/voice/utils/native_audio_capture_bridge/bridgeStats';
import type {
	NativeAudioBridgeEndedCapture,
	NativeAudioBridgeStats,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
import {describe, expect, it} from 'vitest';

describe('getNativeAudioCaptureDiagnosticState', () => {
	it('keeps the six fields real dumps already carry and appends the retained histories', () => {
		expect(Object.keys(getNativeAudioCaptureDiagnosticState())).toEqual([
			'armedCapture',
			'activeBridge',
			'supersededBridge',
			'lastStartedCapture',
			'lastArmFailure',
			'bridgeStats',
			'endedBridgeCaptures',
			'lifecycleFaults',
		]);
	});

	it('emits the superseded capture next to the live one instead of only the replacement', () => {
		startBridgeStats('generator', 'native-audio:diag-failed', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});
		recordBridgeFrame('native-audio:diag-failed', {timestampUs: 0, durationUs: 10_000, peak: 0.31, rms: 0.08});
		startBridgeStats('generator', 'native-audio:diag-live', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});

		const state = getNativeAudioCaptureDiagnosticState();

		expect((state.bridgeStats as NativeAudioBridgeStats).captureId).toBe('native-audio:diag-live');
		const ended = state.endedBridgeCaptures as Array<NativeAudioBridgeEndedCapture>;
		const retained = ended.find((capture) => capture.captureId === 'native-audio:diag-failed');
		expect(retained?.framesReceived).toBe(1);
		expect(retained?.nonSilentFrameCount).toBe(1);
		expect(retained?.endReason).toBe('superseded');
	});

	it('exposes a native-audio fault history that outlives the lifecycle unbind', () => {
		expect(Array.isArray(getNativeAudioCaptureDiagnosticState().lifecycleFaults)).toBe(true);
	});
});
