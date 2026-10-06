// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2EventLogEntry} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';

const SIMULATOR_INVARIANT_LOG_MAX = 4096;

export interface VoiceEngineV2SafetyViolation {
	readonly code: string;
	readonly message: string;
	readonly sequence: number;
}

function assertVoiceEngineV2EventLogInvariants(
	entries: ReadonlyArray<VoiceEngineV2EventLogEntry>,
): ReadonlyArray<VoiceEngineV2SafetyViolation> {
	assert.ok(Array.isArray(entries), 'event log invariants require an array');
	assert.ok(entries.length <= SIMULATOR_INVARIANT_LOG_MAX, 'event log exceeds the invariant inspection cap');
	const violations: Array<VoiceEngineV2SafetyViolation> = [];
	let previousSequence = 0;
	let previousAtMs = -Infinity;
	for (const entry of entries) {
		if (entry.sequence !== previousSequence + 1) {
			violations.push({
				code: 'eventLog.sequenceHole',
				message: `expected sequence ${previousSequence + 1} but received ${entry.sequence}`,
				sequence: entry.sequence,
			});
		}
		if (entry.atMs < previousAtMs) {
			violations.push({
				code: 'eventLog.nonMonotonicClock',
				message: `event ${entry.sequence} regressed clock from ${previousAtMs} to ${entry.atMs}`,
				sequence: entry.sequence,
			});
		}
		previousSequence = entry.sequence;
		previousAtMs = entry.atMs;
	}
	return violations;
}

function assertVoiceEngineV2FrameSinkInvariants(
	entries: ReadonlyArray<VoiceEngineV2EventLogEntry>,
): ReadonlyArray<VoiceEngineV2SafetyViolation> {
	assert.ok(Array.isArray(entries), 'frame sink invariants require an array');
	assert.ok(entries.length <= SIMULATOR_INVARIANT_LOG_MAX, 'frame sink log exceeds the invariant cap');
	const violations: Array<VoiceEngineV2SafetyViolation> = [];
	const startedCaptureIds = new Set<string>();
	for (const entry of entries) {
		if (entry.event.type === 'nativeCapture.started') {
			startedCaptureIds.add(entry.event.captureId);
			continue;
		}
		if (entry.event.type === 'nativeFrameSink.attachRequested') {
			const captureId = entry.event.options.captureId;
			if (!startedCaptureIds.has(captureId)) {
				violations.push({
					code: 'frameSink.attachedWithoutCapture',
					message: `frame sink attached for captureId=${captureId} before capture started at sequence ${entry.sequence}`,
					sequence: entry.sequence,
				});
			}
		}
	}
	return violations;
}

function assertVoiceEngineV2ConnectionInvariants(
	entries: ReadonlyArray<VoiceEngineV2EventLogEntry>,
): ReadonlyArray<VoiceEngineV2SafetyViolation> {
	assert.ok(Array.isArray(entries), 'connection invariants require an array');
	const violations: Array<VoiceEngineV2SafetyViolation> = [];
	let connectingActive = false;
	let disconnectRequested = false;
	for (const entry of entries) {
		const eventType = entry.event.type;
		if (eventType === 'connection.connectRequested') {
			connectingActive = true;
			disconnectRequested = false;
		}
		if (eventType === 'connection.disconnectRequested') {
			disconnectRequested = true;
			connectingActive = false;
		}
		if (eventType === 'connection.connectSucceeded' && disconnectRequested && !connectingActive) {
			violations.push({
				code: 'connection.staleConnectSucceeded',
				message: `connection.connectSucceeded at sequence ${entry.sequence} arrived after a disconnect with no intervening connect`,
				sequence: entry.sequence,
			});
		}
	}
	return violations;
}

function assertVoiceEngineV2OperationIdInvariants(
	entries: ReadonlyArray<VoiceEngineV2EventLogEntry>,
): ReadonlyArray<VoiceEngineV2SafetyViolation> {
	assert.ok(Array.isArray(entries), 'operation invariants require an array');
	const violations: Array<VoiceEngineV2SafetyViolation> = [];
	const seen = new Set<number>();
	for (const entry of entries) {
		for (const command of entry.commands) {
			if (seen.has(command.operationId)) {
				violations.push({
					code: 'commands.duplicateOperationId',
					message: `operationId ${command.operationId} reused at sequence ${entry.sequence}`,
					sequence: entry.sequence,
				});
				continue;
			}
			seen.add(command.operationId);
		}
	}
	return violations;
}

export function collectVoiceEngineV2SafetyViolations(
	entries: ReadonlyArray<VoiceEngineV2EventLogEntry>,
): ReadonlyArray<VoiceEngineV2SafetyViolation> {
	assert.ok(Array.isArray(entries), 'safety violation collector requires an array');
	const violations: Array<VoiceEngineV2SafetyViolation> = [];
	for (const v of assertVoiceEngineV2EventLogInvariants(entries)) violations.push(v);
	for (const v of assertVoiceEngineV2FrameSinkInvariants(entries)) violations.push(v);
	for (const v of assertVoiceEngineV2ConnectionInvariants(entries)) violations.push(v);
	for (const v of assertVoiceEngineV2OperationIdInvariants(entries)) violations.push(v);
	return violations;
}
