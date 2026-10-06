// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	appendDiagnostic,
	markOperation,
	queueCommand,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2UtilityPortsEvent = Extract<VoiceEngineV2Event, {type: `timer.${string}` | `diagnostics.${string}`}>;

export function transitionUtilityPorts(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2UtilityPortsEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionUtilityPorts snapshot must not be null');
	assert.ok(event != null, 'transitionUtilityPorts event must not be null');
	assert.equal(typeof event.type, 'string', 'utilityPorts event type must be a string');
	assert.ok(
		event.type.startsWith('timer.') || event.type.startsWith('diagnostics.'),
		'utilityPorts reducer received unrelated event',
	);
	switch (event.type) {
		case 'timer.scheduleRequested': {
			const allocated = allocateOperation(snapshot);
			return queueCommand(allocated.snapshot, {
				type: 'timer.schedule',
				operationId: allocated.operationId,
				options: event.options,
			});
		}
		case 'timer.cancelRequested': {
			const allocated = allocateOperation(snapshot);
			return queueCommand(allocated.snapshot, {
				type: 'timer.cancel',
				operationId: allocated.operationId,
				timerId: event.timerId,
			});
		}
		case 'timer.fired':
			return {snapshot, commands: []};
		case 'diagnostics.logRequested': {
			const allocated = allocateOperation(snapshot);
			return queueCommand(
				{...allocated.snapshot, diagnostics: appendDiagnostic(allocated.snapshot.diagnostics, event.entry)},
				{type: 'diagnostics.log', operationId: allocated.operationId, entry: event.entry},
			);
		}
		case 'diagnostics.logged':
			return {
				snapshot: {
					...(event.operationId != null ? markOperation(snapshot, event.operationId, 'succeeded') : snapshot),
					diagnostics: appendDiagnostic(snapshot.diagnostics, event.entry),
				},
				commands: [],
			};
	}
}
