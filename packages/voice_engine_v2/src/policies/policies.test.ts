// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import cameraShareEncodingPlanFixtures from '../../fixtures/policies/camera_share_encoding_plan.json';
import hardwareEncoderCapabilityFixtures from '../../fixtures/policies/hardware_encoder_capabilities.json';
import microphoneFailureFixtures from '../../fixtures/policies/microphone_failure_action.json';
import screenShareEncodingPlanFixtures from '../../fixtures/policies/screen_share_encoding_plan.json';
import voiceStatsCoercionFixtures from '../../fixtures/policies/voice_stats_coercion.json';
import voiceStatsSummaryFixtures from '../../fixtures/policies/voice_stats_summary.json';
import voiceTrackClassificationFixtures from '../../fixtures/policies/voice_track_classification.json';
import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '../protocol';
import {
	classifyVoiceEngineV2TrackStats,
	coerceVoiceEngineV2Stats,
	getVoiceEngineV2MicrophoneOperationFailureAction,
	hasVoiceEngineV2NativeNvencEncoder,
	normalizeVoiceEngineV2HardwareEncoderCapabilities,
	planVoiceEngineV2CameraEncodingChange,
	planVoiceEngineV2ScreenEncodingChange,
	summarizeVoiceEngineV2Stats,
	type VoiceEngineV2MicrophoneFailureContext,
	type VoiceEngineV2OperationResultLike,
	type VoiceEngineV2StatsSummary,
	type VoiceEngineV2StatsTrackClassificationInput,
	type VoiceEngineV2StatsTrackRoleSelection,
} from './index';

interface MicrophoneFailureFixture {
	name: string;
	result: VoiceEngineV2OperationResultLike;
	requestedEnabled: boolean;
	context?: VoiceEngineV2MicrophoneFailureContext;
	expected: string;
}

interface HardwareEncoderCapabilityFixture {
	name: string;
	input: unknown;
	codec: string;
	expectedCapabilities: Record<string, unknown>;
	expectedNativeNvenc: boolean;
}

interface ScreenShareEncodingPlanFixture {
	name: string;
	input: {
		published: VoiceEngineV2ScreenOptions | null;
		desired: VoiceEngineV2ScreenOptions | null;
		update: VoiceEngineV2ScreenEncodingOptions;
	};
	expectedAction: string;
	expectedReason: string;
	expectedCodec?: string;
	expectedHardwareEncoding?: boolean;
	expectedZeroCopyRequired?: boolean;
	expectedErrorCode?: string;
}

interface CameraShareEncodingPlanFixture {
	name: string;
	input: {
		published: VoiceEngineV2CameraOptions | null;
		desired: VoiceEngineV2CameraOptions | null;
		update: VoiceEngineV2CameraEncodingOptions;
	};
	expectedAction: string;
	expectedReason: string;
	expectedCodec?: string;
	expectedMirror?: boolean;
	expectedBackgroundMode?: string;
	expectedWidth?: number;
	expectedFrameRate?: number;
	expectedErrorCode?: string;
}

interface VoiceStatsSummaryFixture {
	name: string;
	input: VoiceEngineV2Stats;
	expectedSummary: Partial<VoiceEngineV2StatsSummary>;
}

interface VoiceStatsCoercionFixture {
	name: string;
	input: Record<string, unknown>;
	expected: VoiceEngineV2Stats;
}

interface VoiceTrackClassificationFixture {
	name: string;
	input: VoiceEngineV2StatsTrackClassificationInput;
	expected: VoiceEngineV2StatsTrackRoleSelection;
}

describe('voice engine v2 policies', () => {
	it.each(
		microphoneFailureFixtures as Array<MicrophoneFailureFixture>,
	)('replays microphone failure action fixture: $name', ({result, requestedEnabled, context, expected}) => {
		expect(getVoiceEngineV2MicrophoneOperationFailureAction(result, requestedEnabled, context)).toBe(expected);
	});

	it.each(
		hardwareEncoderCapabilityFixtures as Array<HardwareEncoderCapabilityFixture>,
	)('replays hardware encoder capability fixture: $name', ({
		input,
		codec,
		expectedCapabilities,
		expectedNativeNvenc,
	}) => {
		const capabilities = normalizeVoiceEngineV2HardwareEncoderCapabilities(input);
		expect(capabilities).toMatchObject(expectedCapabilities);
		expect(hasVoiceEngineV2NativeNvencEncoder(capabilities, codec)).toBe(expectedNativeNvenc);
	});

	it.each(
		screenShareEncodingPlanFixtures as Array<ScreenShareEncodingPlanFixture>,
	)('replays screen-share encoding plan fixture: $name', (fixture) => {
		const plan = planVoiceEngineV2ScreenEncodingChange(fixture.input);
		expect(plan.action).toBe(fixture.expectedAction);
		expect(plan.reason).toBe(fixture.expectedReason);
		if (fixture.expectedCodec !== undefined) {
			expect(plan.desired?.codec).toBe(fixture.expectedCodec);
		}
		if (fixture.expectedHardwareEncoding !== undefined) {
			expect(plan.desired?.hardwareEncoding).toBe(fixture.expectedHardwareEncoding);
		}
		if (fixture.expectedZeroCopyRequired !== undefined) {
			expect(plan.desired?.zeroCopyRequired).toBe(fixture.expectedZeroCopyRequired);
		}
		if (fixture.expectedErrorCode !== undefined) {
			expect(plan.error?.code).toBe(fixture.expectedErrorCode);
		}
	});

	it.each(
		cameraShareEncodingPlanFixtures as Array<CameraShareEncodingPlanFixture>,
	)('replays camera-share encoding plan fixture: $name', (fixture) => {
		const plan = planVoiceEngineV2CameraEncodingChange(fixture.input);
		expect(plan.action).toBe(fixture.expectedAction);
		expect(plan.reason).toBe(fixture.expectedReason);
		if (fixture.expectedCodec !== undefined) {
			expect(plan.desired?.codec).toBe(fixture.expectedCodec);
		}
		if (fixture.expectedMirror !== undefined) {
			expect(plan.desired?.mirror).toBe(fixture.expectedMirror);
		}
		if (fixture.expectedBackgroundMode !== undefined) {
			expect(plan.desired?.backgroundMode).toBe(fixture.expectedBackgroundMode);
		}
		if (fixture.expectedWidth !== undefined) {
			expect(plan.desired?.width).toBe(fixture.expectedWidth);
		}
		if (fixture.expectedFrameRate !== undefined) {
			expect(plan.desired?.frameRate).toBe(fixture.expectedFrameRate);
		}
		if (fixture.expectedErrorCode !== undefined) {
			expect(plan.error?.code).toBe(fixture.expectedErrorCode);
		}
	});

	it.each(
		voiceStatsCoercionFixtures as Array<VoiceStatsCoercionFixture>,
	)('replays voice stats coercion fixture: $name', ({input, expected}) => {
		expect(coerceVoiceEngineV2Stats(input)).toMatchObject(expected);
	});

	it.each(voiceStatsSummaryFixtures as Array<VoiceStatsSummaryFixture>)('replays voice stats summary fixture: $name', ({
		input,
		expectedSummary,
	}) => {
		expect(summarizeVoiceEngineV2Stats(input)).toMatchObject(expectedSummary);
	});

	it.each(
		voiceTrackClassificationFixtures as Array<VoiceTrackClassificationFixture>,
	)('replays voice track classification fixture: $name', ({input, expected}) => {
		expect(classifyVoiceEngineV2TrackStats(input)).toEqual(expected);
	});
});
