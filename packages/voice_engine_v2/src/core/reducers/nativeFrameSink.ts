// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {allocateOperation, queueCommand} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {
	nativeZeroCopyRequiredError,
	unavailableZeroCopyTransportError,
} from '@fluxer/voice_engine_v2/src/core/reducers/_zeroCopy';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {VoiceEngineV2NativeFrameSinkOptions} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2NativeFrameSinkEvent = Extract<VoiceEngineV2Event, {type: `nativeFrameSink.${string}`}>;

export function transitionNativeFrameSink(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2NativeFrameSinkEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionNativeFrameSink snapshot must not be null');
	assert.ok(event != null, 'transitionNativeFrameSink event must not be null');
	assert.equal(typeof event.type, 'string', 'nativeFrameSink event type must be a string');
	assert.ok(event.type.startsWith('nativeFrameSink.'), 'nativeFrameSink reducer received unrelated event');
	switch (event.type) {
		case 'nativeFrameSink.attachRequested':
			return onAttachRequested(snapshot, event.options);
		case 'nativeFrameSink.detachRequested':
			return onDetachRequested(snapshot, event.sinkId);
	}
}

function onAttachRequested(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2NativeFrameSinkOptions,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onAttachRequested snapshot must not be null');
	assert.ok(options != null, 'onAttachRequested options must not be null');
	assert.equal(typeof options.sinkId, 'string', 'options.sinkId must be a string');
	if (options.zeroCopyRequired !== true) {
		const error = nativeZeroCopyRequiredError('frameSink');
		return {
			snapshot: {
				...snapshot,
				nativeFrameSink: {...snapshot.nativeFrameSink, failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	if (!snapshot.capabilities.zeroCopyScreenTransport) {
		const error = unavailableZeroCopyTransportError('frameSink');
		return {
			snapshot: {
				...snapshot,
				nativeFrameSink: {...snapshot.nativeFrameSink, failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			nativeFrameSink: {
				...allocated.snapshot.nativeFrameSink,
				sinks: {...allocated.snapshot.nativeFrameSink.sinks, [options.sinkId]: options},
				operationIds: {
					...allocated.snapshot.nativeFrameSink.operationIds,
					[options.sinkId]: allocated.operationId,
				},
				failure: null,
			},
		},
		{type: 'nativeFrameSink.attach', operationId: allocated.operationId, options},
	);
}

function onDetachRequested(snapshot: VoiceEngineV2Snapshot, sinkId: string): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onDetachRequested snapshot must not be null');
	assert.equal(typeof sinkId, 'string', 'onDetachRequested sinkId must be a string');
	assert.ok(sinkId.length > 0, 'onDetachRequested sinkId must not be empty');
	const allocated = allocateOperation(snapshot);
	const sinks = {...allocated.snapshot.nativeFrameSink.sinks};
	delete sinks[sinkId];
	return queueCommand(
		{
			...allocated.snapshot,
			nativeFrameSink: {
				...allocated.snapshot.nativeFrameSink,
				sinks,
				operationIds: {
					...allocated.snapshot.nativeFrameSink.operationIds,
					[sinkId]: allocated.operationId,
				},
			},
		},
		{type: 'nativeFrameSink.detach', operationId: allocated.operationId, sinkId},
	);
}
