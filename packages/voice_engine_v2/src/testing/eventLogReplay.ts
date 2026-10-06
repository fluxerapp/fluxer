// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {selectVoiceEngineV2Model} from '@fluxer/voice_engine_v2/src/core/selectors';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {VoiceEngineV2Capabilities, VoiceEngineV2Model} from '@fluxer/voice_engine_v2/src/protocol/types';

export const VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION = 1;

export interface VoiceEngineV2EventLogFixtureStep {
	name?: string;
	event: VoiceEngineV2Event;
	commands: Array<VoiceEngineV2Command>;
}

export interface VoiceEngineV2EventLogFixtureExpected {
	finalSnapshot: VoiceEngineV2Snapshot;
	finalModel?: VoiceEngineV2Model;
}

export interface VoiceEngineV2EventLogFixture {
	version: typeof VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION;
	name: string;
	description?: string;
	capabilities?: VoiceEngineV2Capabilities;
	initialSnapshot?: VoiceEngineV2Snapshot;
	steps: Array<VoiceEngineV2EventLogFixtureStep>;
	expected: VoiceEngineV2EventLogFixtureExpected;
}

export interface VoiceEngineV2EventLogReplayStepResult {
	index: number;
	name: string | null;
	event: VoiceEngineV2Event;
	previousSnapshot: VoiceEngineV2Snapshot;
	snapshot: VoiceEngineV2Snapshot;
	commands: Array<VoiceEngineV2Command>;
}

export interface VoiceEngineV2EventLogReplayResult {
	fixtureName: string;
	initialSnapshot: VoiceEngineV2Snapshot;
	steps: Array<VoiceEngineV2EventLogReplayStepResult>;
	finalSnapshot: VoiceEngineV2Snapshot;
	finalModel: VoiceEngineV2Model;
	commandBatches: Array<Array<VoiceEngineV2Command>>;
}

export function replayVoiceEngineV2EventLogFixture(
	fixture: VoiceEngineV2EventLogFixture,
): VoiceEngineV2EventLogReplayResult {
	assertVoiceEngineV2EventLogFixture(fixture);

	const initialSnapshot = fixture.initialSnapshot
		? cloneReplayValue(fixture.initialSnapshot)
		: createVoiceEngineV2InitialSnapshot(fixture.capabilities ?? availableVoiceEngineV2Capabilities());
	const initialSnapshotForResult = cloneReplayValue(initialSnapshot);
	let snapshot = initialSnapshot;
	const steps: Array<VoiceEngineV2EventLogReplayStepResult> = [];

	for (const [index, step] of fixture.steps.entries()) {
		const previousSnapshot = cloneReplayValue(snapshot);
		const previousSnapshotBeforeTransition = cloneReplayValue(snapshot);
		deepFreeze(snapshot);

		const transition = transitionVoiceEngineV2(snapshot, step.event);

		assert.deepStrictEqual(
			snapshot,
			previousSnapshotBeforeTransition,
			`${fixture.name} step ${index + 1} mutated its previous snapshot`,
		);
		assert.deepStrictEqual(
			transition.commands,
			step.commands,
			`${fixture.name} step ${index + 1} emitted an unexpected command batch`,
		);

		snapshot = transition.snapshot;
		steps.push({
			index,
			name: step.name ?? null,
			event: cloneReplayValue(step.event),
			previousSnapshot,
			snapshot: cloneReplayValue(snapshot),
			commands: cloneReplayValue(transition.commands),
		});
	}

	assert.deepStrictEqual(snapshot, fixture.expected.finalSnapshot, `${fixture.name} final snapshot drifted`);

	const finalModel = selectVoiceEngineV2Model(snapshot);
	if (fixture.expected.finalModel) {
		assert.deepStrictEqual(finalModel, fixture.expected.finalModel, `${fixture.name} final model drifted`);
	}

	return {
		fixtureName: fixture.name,
		initialSnapshot: initialSnapshotForResult,
		steps,
		finalSnapshot: cloneReplayValue(snapshot),
		finalModel: cloneReplayValue(finalModel),
		commandBatches: steps.map((step) => cloneReplayValue(step.commands)),
	};
}

export function assertVoiceEngineV2EventLogFixture(fixture: VoiceEngineV2EventLogFixture): void {
	assert.equal(
		fixture.version,
		VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION,
		`${fixture.name} uses an unsupported voice engine v2 event-log fixture version`,
	);
	assert.ok(fixture.name.length > 0, 'voice engine v2 event-log fixtures must have a name');
	assert.ok(Array.isArray(fixture.steps), `${fixture.name} must provide replay steps`);

	for (const [index, step] of fixture.steps.entries()) {
		assert.ok(step.event && typeof step.event.type === 'string', `${fixture.name} step ${index + 1} has no event`);
		assert.ok(Array.isArray(step.commands), `${fixture.name} step ${index + 1} must declare commands`);
	}
}

function cloneReplayValue<Value>(value: Value): Value {
	if (Array.isArray(value)) return value.map((entry) => cloneReplayValue(entry)) as Value;
	if (!isFreezable(value)) return value;
	const clone: Record<PropertyKey, unknown> = {};
	for (const property of Reflect.ownKeys(value)) {
		clone[property] = cloneReplayValue((value as Record<PropertyKey, unknown>)[property]);
	}
	return clone as Value;
}

function deepFreeze<Value>(value: Value): Value {
	if (!isFreezable(value) || Object.isFrozen(value)) return value;
	for (const property of Reflect.ownKeys(value)) {
		deepFreeze((value as Record<PropertyKey, unknown>)[property]);
	}
	Object.freeze(value);
	return value;
}

function isFreezable(value: unknown): value is Record<PropertyKey, unknown> {
	return (typeof value === 'object' && value !== null) || typeof value === 'function';
}
