// SPDX-License-Identifier: AGPL-3.0-or-later

import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2OperationStatus,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2AudioControls,
	VoiceEngineV2Error,
	VoiceEngineV2NativeCaptureOptions,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {describe, expect, it} from 'vitest';

const captureOptions: VoiceEngineV2NativeCaptureOptions = {
	captureId: 'capture-main',
	source: {id: 'display-1', kind: 'screen', title: 'Display 1', width: 1920, height: 1080, displayId: 'display-1'},
	width: 1920,
	height: 1080,
	frameRate: 60,
	includeCursor: true,
	includeAudio: false,
	zeroCopyRequired: true,
};
const captureError: VoiceEngineV2Error = {code: 'implementationError', message: 'Capture stopped unexpectedly'};
const previousError: VoiceEngineV2Error = {code: 'implementationError', message: 'Previous capture failed'};

function replay(events: ReadonlyArray<VoiceEngineV2Event>): VoiceEngineV2Snapshot {
	let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	for (const event of events) {
		snapshot = transitionVoiceEngineV2(snapshot, event).snapshot;
	}
	return snapshot;
}

interface NativeCompletionCase {
	name: string;
	before: Array<VoiceEngineV2Event>;
	event: VoiceEngineV2Event;
	expectedStatus: VoiceEngineV2OperationStatus;
	expectedOperationError: VoiceEngineV2Error | null;
	expectedOperationIds: Record<string, number>;
	expectedFailure: VoiceEngineV2Error | null;
	expectedLastFailure: VoiceEngineV2Error | null;
	sameSnapshot: boolean;
}

const nativeCases: Array<NativeCompletionCase> = [
	{
		name: 'matching completion succeeds',
		before: [],
		event: {type: 'nativeCapture.started', captureId: 'capture-main', operationId: 1},
		expectedStatus: 'succeeded',
		expectedOperationError: null,
		expectedOperationIds: {},
		expectedFailure: null,
		expectedLastFailure: null,
		sameSnapshot: false,
	},
	{
		name: 'matching failure records the operation error',
		before: [],
		event: {type: 'nativeCapture.failed', captureId: 'capture-main', operationId: 1, error: captureError},
		expectedStatus: 'failed',
		expectedOperationError: captureError,
		expectedOperationIds: {},
		expectedFailure: captureError,
		expectedLastFailure: captureError,
		sameSnapshot: false,
	},
	{
		name: 'stale completion leaves the active operation pending',
		before: [],
		event: {type: 'nativeCapture.started', captureId: 'capture-main', operationId: 2},
		expectedStatus: 'queued',
		expectedOperationError: null,
		expectedOperationIds: {'capture-main': 1},
		expectedFailure: null,
		expectedLastFailure: null,
		sameSnapshot: true,
	},
	{
		name: 'stale failure cannot replace current state',
		before: [],
		event: {type: 'nativeCapture.failed', captureId: 'capture-main', operationId: 2, error: captureError},
		expectedStatus: 'queued',
		expectedOperationError: null,
		expectedOperationIds: {'capture-main': 1},
		expectedFailure: null,
		expectedLastFailure: null,
		sameSnapshot: true,
	},
	{
		name: 'unsolicited failure preserves the pending operation',
		before: [],
		event: {type: 'nativeCapture.failed', captureId: 'capture-main', operationId: null, error: captureError},
		expectedStatus: 'queued',
		expectedOperationError: null,
		expectedOperationIds: {'capture-main': 1},
		expectedFailure: captureError,
		expectedLastFailure: captureError,
		sameSnapshot: false,
	},
	{
		name: 'successful completion retains the previous failure history',
		before: [{type: 'nativeCapture.failed', captureId: 'previous-capture', operationId: null, error: previousError}],
		event: {type: 'nativeCapture.started', captureId: 'capture-main', operationId: 1},
		expectedStatus: 'succeeded',
		expectedOperationError: null,
		expectedOperationIds: {},
		expectedFailure: null,
		expectedLastFailure: previousError,
		sameSnapshot: false,
	},
];

interface DeafenCase {
	name: string;
	before: Array<VoiceEngineV2Event>;
	deafened: boolean;
	expectedControls: VoiceEngineV2AudioControls;
	expectedEnabled: boolean;
	expectedSpeaking: boolean | null;
	expectedCommands: Array<VoiceEngineV2Command>;
}

const deafenCases: Array<DeafenCase> = [
	{
		name: 'deafening an unmuted microphone remembers unmute',
		before: [],
		deafened: true,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: false,
			locallyDeafened: true,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: true,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [{type: 'microphone.setEnabled', operationId: 3, enabled: false}],
	},
	{
		name: 'undeafening restores the remembered unmute',
		before: [
			{type: 'localAudio.deafenRequested', deafened: true},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
		],
		deafened: false,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: false,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: true,
		expectedSpeaking: null,
		expectedCommands: [{type: 'microphone.setEnabled', operationId: 4, enabled: true}],
	},
	{
		name: 'deafening a user-muted microphone preserves mute',
		before: [
			{type: 'localAudio.muteRequested', muted: true},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
		],
		deafened: true,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: true,
			locallyDeafened: true,
			mutedByPermission: false,
			hasUserSetMute: true,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [],
	},
	{
		name: 'undeafening does not undo a user mute',
		before: [
			{type: 'localAudio.muteRequested', muted: true},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
			{type: 'localAudio.deafenRequested', deafened: true},
		],
		deafened: false,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: true,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: true,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [],
	},
	{
		name: 'deafening a permission-muted microphone preserves permission mute',
		before: [
			{type: 'permissions.result', operationId: null, result: {name: 'microphone', status: 'denied', canPrompt: false}},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
		],
		deafened: true,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: false,
			locallyDeafened: true,
			mutedByPermission: true,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [],
	},
	{
		name: 'undeafening does not bypass a denied permission',
		before: [
			{type: 'permissions.result', operationId: null, result: {name: 'microphone', status: 'denied', canPrompt: false}},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
			{type: 'localAudio.deafenRequested', deafened: true},
		],
		deafened: false,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: true,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [],
	},
	{
		name: 'permission denial while deafened vetoes remembered unmute',
		before: [
			{type: 'localAudio.deafenRequested', deafened: true},
			{type: 'microphone.setEnabledSucceeded', operationId: 3},
			{type: 'permissions.result', operationId: null, result: {name: 'microphone', status: 'denied', canPrompt: false}},
		],
		deafened: false,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: true,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: true,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: false,
		expectedSpeaking: false,
		expectedCommands: [],
	},
	{
		name: 'undeafening an already unmuted microphone emits no command',
		before: [],
		deafened: false,
		expectedControls: {
			mode: 'voiceActivity',
			locallyMuted: false,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		expectedEnabled: true,
		expectedSpeaking: null,
		expectedCommands: [],
	},
];

describe('voice engine native completion and local deafen transitions', () => {
	it.each(nativeCases)('$name', (row) => {
		const snapshot = replay([...row.before, {type: 'nativeCapture.startRequested', options: captureOptions}]);
		const saved = structuredClone(snapshot);
		const result = transitionVoiceEngineV2(snapshot, row.event);

		expect(result.commands).toEqual([]);
		expect(result.snapshot.operations['1']).toEqual({
			operationId: 1,
			resourceKey: 'nativeCapture',
			commandType: 'nativeCapture.start',
			status: row.expectedStatus,
			error: row.expectedOperationError,
		});
		expect(result.snapshot.nativeCapture.operationIds).toEqual(row.expectedOperationIds);
		expect(result.snapshot.nativeCapture.captures).toEqual({'capture-main': captureOptions});
		expect(result.snapshot.nativeCapture.captures['capture-main']).toBe(captureOptions);
		expect(result.snapshot.nativeCapture.failure).toBe(row.expectedFailure);
		expect(result.snapshot.lastFailure).toBe(row.expectedLastFailure);
		expect(result.snapshot === snapshot).toBe(row.sameSnapshot);
		expect(result.snapshot.nextOperationId).toBe(2);
		expect(snapshot).toEqual(saved);
	});

	it.each(deafenCases)('$name', (row) => {
		const snapshot = replay([
			{type: 'connection.connectRequested', options: {url: 'wss://voice.example', token: 'voice-token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'microphone.publishRequested', options: {deviceId: 'mic-a'}},
			{type: 'microphone.publishSucceeded', operationId: 2},
			...row.before,
		]);
		const saved = structuredClone(snapshot);
		const result = transitionVoiceEngineV2(snapshot, {type: 'localAudio.deafenRequested', deafened: row.deafened});

		expect(result.commands).toEqual(row.expectedCommands);
		expect(result.snapshot.audioControls).toEqual(row.expectedControls);
		expect(result.snapshot.microphone.enabled).toBe(row.expectedEnabled);
		expect(result.snapshot.microphone.localSpeakingOverride).toBe(row.expectedSpeaking);
		expect(result.snapshot.microphone.status).toBe('published');
		expect(result.snapshot.microphone.published).toEqual({deviceId: 'mic-a'});
		expect(result.snapshot.connection.status).toBe('connected');
		expect(snapshot).toEqual(saved);
	});
});
