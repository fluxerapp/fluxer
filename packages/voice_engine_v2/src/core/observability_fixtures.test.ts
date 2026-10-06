// SPDX-License-Identifier: AGPL-3.0-or-later

import capabilitiesLifecycleFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/capabilities_lifecycle.json';
import deviceInventoryFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/device_inventory.json';
import diagnosticsLifecycleFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/diagnostics_lifecycle.json';
import e2eeLifecycleFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/e2ee_lifecycle.json';
import statsLifecycleFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/stats_lifecycle.json';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	selectVoiceEngineV2CapabilitiesProjection,
	selectVoiceEngineV2DeviceProjection,
	selectVoiceEngineV2DiagnosticsProjection,
	selectVoiceEngineV2E2eeProjection,
	selectVoiceEngineV2StatsProjection,
} from '@fluxer/voice_engine_v2/src/core/selectors';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import {describe, expect, it} from 'vitest';

interface VoiceEngineV2EventLogFixture<Expected> {
	name: string;
	events: Array<VoiceEngineV2Event>;
	expectedCommands: Array<Array<VoiceEngineV2Command>>;
	expected: Expected;
}

interface StatsLifecycleExpected {
	nextOperationId: number;
	statsOperationId: number | null;
	statsFailureCode: string | null;
	summary: Record<string, unknown>;
}

interface DiagnosticsLifecycleExpected {
	nextOperationId: number;
	entryIds: Array<string>;
	lastCode: string;
	operationStatuses: Record<string, string>;
}

interface CapabilitiesLifecycleExpected {
	nextOperationId: number;
	hardwareEncoderOperationId: number | null;
	hardwareEncoderFailureCode: string | null;
	hardwareEncoderBackend: string;
	hasZeroCopyNativeInput: boolean;
	hasNativeNvencH264: boolean;
	hasNativeNvencH265: boolean;
}

interface E2eeLifecycleExpected {
	nextOperationId: number;
	status: string;
	keyId: string | null;
	operationId: number | null;
	failureCode: string | null;
}

interface DeviceInventoryExpected {
	nextOperationId: number;
	deviceOperationId: number | null;
	failureCode: string | null;
	selectedAudioInputId: string | null;
	selectedAudioInputLabel: string | null;
	selectedAudioInputRole: string | null;
	audioOutputCount: number;
	cameraCount: number;
	operationStatuses: Record<string, string>;
}

function replayFixture<Expected>(
	fixture: VoiceEngineV2EventLogFixture<Expected>,
	initialSnapshot: VoiceEngineV2Snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities()),
): VoiceEngineV2Snapshot {
	let snapshot = initialSnapshot;
	fixture.events.forEach((event, index) => {
		const transition = transitionVoiceEngineV2(snapshot, event);
		expect(transition.commands).toEqual(fixture.expectedCommands[index]);
		snapshot = transition.snapshot;
	});
	return snapshot;
}

describe('voice engine v2 observability lifecycle fixtures', () => {
	it('replays stats collection into the stats summary projection', () => {
		const fixture = statsLifecycleFixtureJson as VoiceEngineV2EventLogFixture<StatsLifecycleExpected>;
		const snapshot = replayFixture(fixture);
		const statsProjection = selectVoiceEngineV2StatsProjection(snapshot);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(snapshot.statsOperationId).toBe(fixture.expected.statsOperationId);
		expect(snapshot.statsFailure?.code ?? null).toBe(fixture.expected.statsFailureCode);
		expect(statsProjection.summary).toMatchObject(fixture.expected.summary);
	});

	it('replays diagnostics logging into append-only diagnostics projection', () => {
		const fixture = diagnosticsLifecycleFixtureJson as VoiceEngineV2EventLogFixture<DiagnosticsLifecycleExpected>;
		const snapshot = replayFixture(fixture);
		const diagnosticsProjection = selectVoiceEngineV2DiagnosticsProjection(snapshot);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(diagnosticsProjection.entries.map((entry) => entry.id)).toEqual(fixture.expected.entryIds);
		expect(diagnosticsProjection.entries.at(-1)?.code).toBe(fixture.expected.lastCode);
		expect(
			Object.fromEntries(Object.entries(snapshot.operations).map(([id, operation]) => [id, operation.status])),
		).toEqual(fixture.expected.operationStatuses);
	});

	it('replays hardware capability discovery into the capabilities projection', () => {
		const fixture = capabilitiesLifecycleFixtureJson as VoiceEngineV2EventLogFixture<CapabilitiesLifecycleExpected>;
		const snapshot = replayFixture(fixture);
		const capabilitiesProjection = selectVoiceEngineV2CapabilitiesProjection(snapshot);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(snapshot.hardwareEncoder.operationId).toBe(fixture.expected.hardwareEncoderOperationId);
		expect(snapshot.hardwareEncoder.failure?.code ?? null).toBe(fixture.expected.hardwareEncoderFailureCode);
		expect(capabilitiesProjection.hardwareEncoderCapabilities?.backend).toBe(fixture.expected.hardwareEncoderBackend);
		expect(capabilitiesProjection.hasZeroCopyNativeInput).toBe(fixture.expected.hasZeroCopyNativeInput);
		expect(capabilitiesProjection.hasNativeNvencH264).toBe(fixture.expected.hasNativeNvencH264);
		expect(capabilitiesProjection.hasNativeNvencH265).toBe(fixture.expected.hasNativeNvencH265);
	});

	it('rejects stale E2EE completions and projects public E2EE state without reducer bookkeeping', () => {
		const fixture = e2eeLifecycleFixtureJson as VoiceEngineV2EventLogFixture<E2eeLifecycleExpected>;
		const snapshot = replayFixture(fixture);
		const e2eeProjection = selectVoiceEngineV2E2eeProjection(snapshot);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(e2eeProjection.status).toBe(fixture.expected.status);
		expect(e2eeProjection.keyId).toBe(fixture.expected.keyId);
		expect(e2eeProjection.failure?.code ?? null).toBe(fixture.expected.failureCode);
		expect('operationId' in e2eeProjection).toBe(false);
		expect(snapshot.e2ee.operationId).toBe(fixture.expected.operationId);
	});

	it('replays typed device inventory and clears selection operations', () => {
		const fixture = deviceInventoryFixtureJson as VoiceEngineV2EventLogFixture<DeviceInventoryExpected>;
		const snapshot = replayFixture(fixture);
		const deviceProjection = selectVoiceEngineV2DeviceProjection(snapshot);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(snapshot.devices.operationId).toBe(fixture.expected.deviceOperationId);
		expect(snapshot.devices.failure?.code ?? null).toBe(fixture.expected.failureCode);
		expect(deviceProjection.devices.selectedAudioInputId).toBe(fixture.expected.selectedAudioInputId);
		expect(deviceProjection.selectedAudioInput?.label ?? null).toBe(fixture.expected.selectedAudioInputLabel);
		expect(deviceProjection.selectedAudioInput?.role ?? null).toBe(fixture.expected.selectedAudioInputRole);
		expect(deviceProjection.devices.audioOutputs).toHaveLength(fixture.expected.audioOutputCount);
		expect(deviceProjection.devices.cameras).toHaveLength(fixture.expected.cameraCount);
		expect(
			Object.fromEntries(Object.entries(snapshot.operations).map(([id, operation]) => [id, operation.status])),
		).toEqual(fixture.expected.operationStatuses);
	});
});
