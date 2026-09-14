// SPDX-License-Identifier: AGPL-3.0-or-later

import deviceHotswapFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/device_hotswap.json';
import localMediaFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/local_media.json';
import microphoneFailureRecoveryFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/microphone_failure_recovery.json';
import permissionsRecoveryFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/permissions_recovery.json';
import pttPtmFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/ptt_ptm.json';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command, VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol';
import {describe, expect, it} from 'vitest';

interface LocalMediaSnapshotExpectation {
	nextOperationId?: number;
	connectionStatus?: string;
	microphoneStatus?: string;
	microphoneEnabled?: boolean;
	localSpeakingOverride?: boolean | null;
	audioMode?: string;
	locallyMuted?: boolean;
	preferredLocallyMuted?: boolean;
	locallyDeafened?: boolean;
	mutedByPermission?: boolean;
	shouldUnmuteOnUndeafen?: boolean;
	publishedDeviceId?: string | null;
	desiredDeviceId?: string | null;
	selectedAudioInputId?: string | null;
	permissionStatus?: string;
	lastFailureCode?: string | null;
}

interface LocalMediaCheckpoint extends LocalMediaSnapshotExpectation {
	afterEvent: number;
}

interface LocalMediaFixture {
	name: string;
	events: Array<VoiceEngineV2Event>;
	expectedCommands: Array<VoiceEngineV2Command>;
	checkpoints?: Array<LocalMediaCheckpoint>;
	expectedSnapshot: LocalMediaSnapshotExpectation;
}

const fixtures = [
	localMediaFixtureJson,
	pttPtmFixtureJson,
	deviceHotswapFixtureJson,
	permissionsRecoveryFixtureJson,
	microphoneFailureRecoveryFixtureJson,
] as Array<LocalMediaFixture>;

function applyExpectation(snapshot: VoiceEngineV2Snapshot, expected: LocalMediaSnapshotExpectation): void {
	if (expected.nextOperationId !== undefined) expect(snapshot.nextOperationId).toBe(expected.nextOperationId);
	if (expected.connectionStatus !== undefined) expect(snapshot.connection.status).toBe(expected.connectionStatus);
	if (expected.microphoneStatus !== undefined) expect(snapshot.microphone.status).toBe(expected.microphoneStatus);
	if (expected.microphoneEnabled !== undefined) expect(snapshot.microphone.enabled).toBe(expected.microphoneEnabled);
	if (expected.localSpeakingOverride !== undefined) {
		expect(snapshot.microphone.localSpeakingOverride).toBe(expected.localSpeakingOverride);
	}
	if (expected.audioMode !== undefined) expect(snapshot.audioControls.mode).toBe(expected.audioMode);
	if (expected.locallyMuted !== undefined) expect(snapshot.audioControls.locallyMuted).toBe(expected.locallyMuted);
	if (expected.preferredLocallyMuted !== undefined) {
		expect(snapshot.audioControls.preferredLocallyMuted).toBe(expected.preferredLocallyMuted);
	}
	if (expected.locallyDeafened !== undefined) {
		expect(snapshot.audioControls.locallyDeafened).toBe(expected.locallyDeafened);
	}
	if (expected.mutedByPermission !== undefined) {
		expect(snapshot.audioControls.mutedByPermission).toBe(expected.mutedByPermission);
	}
	if (expected.shouldUnmuteOnUndeafen !== undefined) {
		expect(snapshot.audioControls.shouldUnmuteOnUndeafen).toBe(expected.shouldUnmuteOnUndeafen);
	}
	if (expected.publishedDeviceId !== undefined) {
		expect(snapshot.microphone.published?.deviceId ?? null).toBe(expected.publishedDeviceId);
	}
	if (expected.desiredDeviceId !== undefined) {
		expect(snapshot.microphone.desired?.deviceId ?? null).toBe(expected.desiredDeviceId);
	}
	if (expected.selectedAudioInputId !== undefined) {
		expect(snapshot.devices.inventory.selectedAudioInputId).toBe(expected.selectedAudioInputId);
	}
	if (expected.permissionStatus !== undefined) {
		expect(snapshot.permissions.results.microphone?.status).toBe(expected.permissionStatus);
	}
	if (expected.lastFailureCode !== undefined) expect(snapshot.lastFailure?.code ?? null).toBe(expected.lastFailureCode);
}

describe('voice engine v2 local media fixture replay', () => {
	it.each(fixtures)('replays local media fixture: $name', (fixture) => {
		let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		const commands: Array<VoiceEngineV2Command> = [];

		fixture.events.forEach((event, index) => {
			const transition = transitionVoiceEngineV2(snapshot, event);
			snapshot = transition.snapshot;
			commands.push(...transition.commands);
			for (const checkpoint of fixture.checkpoints ?? []) {
				if (checkpoint.afterEvent === index + 1) applyExpectation(snapshot, checkpoint);
			}
		});

		expect(commands).toEqual(fixture.expectedCommands);
		applyExpectation(snapshot, fixture.expectedSnapshot);
	});
});
