// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceEngineV2SourceLifecycleRemovedEvent,
	VoiceEngineV2SourceLifecycleTransitionedEvent,
} from '../../protocol/events';
import type {VoiceEngineV2DiagnosticEntry} from '../../protocol/types';
import type {SourceFault, SourceLifecycleState} from '../../source_isolation/SourceLifecycleState';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '../state';
import {allocateOperation, appendDiagnostic, queueCommand} from './_helpers';

const SOURCE_LIFECYCLE_DIAGNOSTICS_CODE = 'sourceFailed';

type VoiceEngineV2SourceLifecyclesEvent =
	| VoiceEngineV2SourceLifecycleTransitionedEvent
	| VoiceEngineV2SourceLifecycleRemovedEvent;

function assertNeverLifecycleKind(kind: never): never {
	assert.fail(`unhandled source lifecycle kind: ${JSON.stringify(kind)}`);
}

function rebuildSourceLifecycleState(event: VoiceEngineV2SourceLifecycleTransitionedEvent): SourceLifecycleState {
	assert.ok(event != null, 'rebuildSourceLifecycleState event must not be null');
	assert.equal(typeof event.since, 'bigint', 'sourceLifecycle.transitioned.since must be a bigint');
	assert.ok(event.since >= 0n, 'sourceLifecycle.transitioned.since must be non-negative');
	assert.ok(Number.isInteger(event.attempts), 'sourceLifecycle.transitioned.attempts must be integer');
	assert.ok(event.attempts >= 0, 'sourceLifecycle.transitioned.attempts must be non-negative');
	if (event.kind === 'active') {
		return {kind: 'active', since: event.since};
	}
	if (event.kind === 'reconnecting') {
		assert.ok(event.fault !== null, 'reconnecting transition requires a fault');
		return {kind: 'reconnecting', since: event.since, attempts: event.attempts, lastFault: event.fault};
	}
	if (event.kind === 'failed') {
		assert.ok(event.fault !== null, 'failed transition requires a fault');
		return {kind: 'failed', since: event.since, finalFault: event.fault, totalAttempts: event.attempts};
	}
	return assertNeverLifecycleKind(event.kind);
}

export function transitionSourceLifecycles(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2SourceLifecyclesEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionSourceLifecycles snapshot must not be null');
	assert.ok(event != null, 'transitionSourceLifecycles event must not be null');
	assert.equal(typeof event.sourceId, 'string', 'sourceLifecycle.sourceId must be a string');
	assert.ok(event.sourceId.length > 0, 'sourceLifecycle.sourceId must not be empty');
	if (event.type === 'sourceLifecycle.removed') {
		if (!Object.hasOwn(snapshot.sourceLifecycles, event.sourceId)) return {snapshot, commands: []};
		const sourceLifecycles = {...snapshot.sourceLifecycles};
		delete sourceLifecycles[event.sourceId];
		return {snapshot: {...snapshot, sourceLifecycles}, commands: []};
	}
	const nextState = rebuildSourceLifecycleState(event);
	const base: VoiceEngineV2Snapshot = {
		...snapshot,
		sourceLifecycles: {...snapshot.sourceLifecycles, [event.sourceId]: nextState},
	};
	if (event.kind !== 'failed') return {snapshot: base, commands: []};
	const fault: SourceFault | null = event.fault;
	assert.ok(fault !== null, 'failed transition requires a fault');
	const allocated = allocateOperation(base);
	const diagnosticEntry: VoiceEngineV2DiagnosticEntry = {
		id: `sourceLifecycle.failed.${event.sourceId}.${allocated.operationId}`,
		atMs: event.atMs,
		level: 'error',
		code: SOURCE_LIFECYCLE_DIAGNOSTICS_CODE,
		message: `source ${event.sourceId} failed with fault ${fault}`,
		detail: {sourceId: event.sourceId, fault, attempts: event.attempts},
	};
	return queueCommand(
		{...allocated.snapshot, diagnostics: appendDiagnostic(allocated.snapshot.diagnostics, diagnosticEntry)},
		{type: 'diagnostics.log', operationId: allocated.operationId, entry: diagnosticEntry},
	);
}
