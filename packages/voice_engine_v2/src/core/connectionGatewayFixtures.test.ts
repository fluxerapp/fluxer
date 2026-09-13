// SPDX-License-Identifier: AGPL-3.0-or-later

import gatewayConnectionFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/gateway_connection.json';
import reconnectChannelMoveFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/reconnect_channel_move.json';
import resumePreservesMediaFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/resume_preserves_media.json';
import staleConnectionFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/stale_connection.json';
import teardownRendererWindowFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/teardown_renderer_window.json';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2OperationStatus,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import {describe, expect, it} from 'vitest';

interface VoiceEngineV2ConnectionGatewayFixtureStep {
	name: string;
	event: VoiceEngineV2Event;
	commands: Array<VoiceEngineV2Command>;
}

interface VoiceEngineV2ConnectionGatewayFixtureExpected {
	nextOperationId: number;
	connectionStatus: string;
	activeUrl: string | null;
	desiredUrl: string | null;
	gatewayChannelId: string | null;
	lifecycleTearingDown: boolean;
	operationStatuses?: Record<string, VoiceEngineV2OperationStatus>;
}

interface VoiceEngineV2ConnectionGatewayFixture {
	name: string;
	steps: Array<VoiceEngineV2ConnectionGatewayFixtureStep>;
	expected: VoiceEngineV2ConnectionGatewayFixtureExpected;
}

const fixtures = [
	gatewayConnectionFixtureJson,
	reconnectChannelMoveFixtureJson,
	resumePreservesMediaFixtureJson,
	staleConnectionFixtureJson,
	teardownRendererWindowFixtureJson,
] as Array<unknown>;

function initialSnapshot(): VoiceEngineV2Snapshot {
	return createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
}

function replayFixture(fixture: VoiceEngineV2ConnectionGatewayFixture): VoiceEngineV2Snapshot {
	let snapshot = initialSnapshot();
	for (const step of fixture.steps) {
		const transition = transitionVoiceEngineV2(snapshot, step.event);
		expect(transition.commands, `${fixture.name}: ${step.name}`).toEqual(step.commands);
		snapshot = transition.snapshot;
	}
	return snapshot;
}

describe('connection and gateway event-log fixtures', () => {
	it.each(fixtures)('replays %s as pure snapshot transitions', (fixtureJson) => {
		const fixture = fixtureJson as VoiceEngineV2ConnectionGatewayFixture;
		const snapshot = replayFixture(fixture);

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(snapshot.connection.status).toBe(fixture.expected.connectionStatus);
		expect(snapshot.connection.active?.url ?? null).toBe(fixture.expected.activeUrl);
		expect(snapshot.connection.desired?.url ?? null).toBe(fixture.expected.desiredUrl);
		expect(snapshot.gateway.selfVoiceState?.channelId ?? null).toBe(fixture.expected.gatewayChannelId);
		expect(snapshot.lifecycle.tearingDown).toBe(fixture.expected.lifecycleTearingDown);

		for (const [operationId, status] of Object.entries(fixture.expected.operationStatuses ?? {})) {
			expect(snapshot.operations[operationId]?.status, `${fixture.name}: operation ${operationId}`).toBe(status);
		}
	});

	it('does not mutate the previous snapshot when planning a connection command', () => {
		const snapshot = initialSnapshot();
		const transition = transitionVoiceEngineV2(snapshot, {
			type: 'connection.connectRequested',
			options: {url: 'wss://voice.example.test', token: 'token'},
		});

		expect(snapshot.connection.status).toBe('idle');
		expect(snapshot.connection.operationId).toBeNull();
		expect(snapshot.nextOperationId).toBe(1);
		expect(transition.snapshot).not.toBe(snapshot);
		expect(transition.snapshot.connection.status).toBe('connecting');
		expect(transition.commands).toEqual([
			{
				type: 'connection.connect',
				operationId: 1,
				options: {url: 'wss://voice.example.test', token: 'token'},
			},
		]);
	});
});
