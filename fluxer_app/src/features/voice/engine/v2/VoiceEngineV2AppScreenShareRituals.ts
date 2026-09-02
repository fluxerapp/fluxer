// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import {updateLocalParticipantFromRoom} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import type {
	VoiceScreenShareEvent,
	VoiceScreenShareSourceType,
} from '@app/features/voice/engine/VoiceScreenShareStateMachine';
import {enforceLocalMediaPublicationCap} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {VoiceEngineV2AppScreenShareExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import type {ScreenShareContentSource} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {LocalParticipant, Room, TrackPublishOptions} from 'livekit-client';

export interface ScreenSharePublishPipelineOptions {
	readonly contentSource: ScreenShareContentSource | undefined;
	readonly effectivePublishOptions: TrackPublishOptions | undefined;
}

export type ScreenShareActivationAudioSync =
	| {readonly kind: 'participant-after-watch'}
	| {readonly kind: 'self-stream-before-watch'; readonly published: boolean};

export interface ScreenShareActivationRitualSteps {
	readonly acquireStreamingPriority: boolean;
	readonly enforcePublicationCap: boolean;
	readonly applyState: ((active: boolean) => void) | null;
	readonly applyStatePosition: 'before-pipeline' | 'after-pipeline';
	readonly publishPipeline: ScreenSharePublishPipelineOptions | null;
	readonly deactivateCleanup: (() => Promise<void>) | null;
	readonly updateLocalParticipant: boolean;
	readonly audioSync: ScreenShareActivationAudioSync;
	readonly syncPersistedAudioPreferenceWhenActive: boolean;
	readonly playSound: boolean;
	readonly buildResolveTransition: (() => VoiceScreenShareEvent) | null;
}

export interface ScreenShareActivationRitualArgs {
	readonly adapter: VoiceEngineV2AppScreenShareExecutionAdapter;
	readonly room: Room | null;
	readonly participant: LocalParticipant | null;
	readonly active: boolean;
	readonly steps: ScreenShareActivationRitualSteps;
}

export function applyScreenShareState(
	adapter: VoiceEngineV2AppScreenShareExecutionAdapter,
	enabled: boolean,
	sendUpdate: boolean,
	forceSync: boolean = false,
): void {
	assert.equal(typeof enabled, 'boolean', 'screen-share state must be a boolean');
	assert.equal(typeof sendUpdate, 'boolean', 'screen-share state sendUpdate must be a boolean');
	assert.equal(typeof forceSync, 'boolean', 'screen-share state forceSync must be a boolean');
	adapter.applyScreenShareStateInternal(enabled, {
		...(forceSync ? {forceSync} : {}),
		reason: sendUpdate ? 'user' : 'server',
		sendUpdate,
	});
}

async function runScreenSharePublishPipeline(
	adapter: VoiceEngineV2AppScreenShareExecutionAdapter,
	room: Room | null,
	participant: LocalParticipant,
	pipeline: ScreenSharePublishPipelineOptions,
): Promise<void> {
	assert.ok(participant, 'participant is required for the screen-share publish pipeline');
	assert.ok(pipeline !== null && typeof pipeline === 'object', 'publish pipeline options must be an object');
	if (pipeline.contentSource === undefined) {
		adapter.applyScreenShareContentHintInternal(participant);
	} else {
		adapter.applyScreenShareContentHintInternal(participant, pipeline.contentSource);
	}
	await adapter.enforceScreenShareSenderParametersInternal(participant, pipeline.effectivePublishOptions);
	adapter.ensureScreenShareKeepAliveSinkInternal(participant);
	adapter.applyScreenShareAudioContentHintInternal(participant);
	adapter.monitorActiveScreenShareEndInternal(room, participant);
	adapter.startEncoderVerificationInternal(room, participant, pipeline.effectivePublishOptions?.videoCodec);
}

export async function runScreenShareActivationRitual(args: ScreenShareActivationRitualArgs): Promise<void> {
	const {adapter, room, participant, active, steps} = args;
	assert.equal(typeof active, 'boolean', 'activation ritual active flag must be a boolean');
	if (steps.enforcePublicationCap) {
		assert.ok(participant, 'participant is required to enforce the publication cap');
	}
	if (steps.publishPipeline) {
		assert.ok(participant, 'participant is required to run the publish pipeline');
	}
	if (steps.audioSync.kind === 'participant-after-watch') {
		assert.ok(participant, 'participant is required for participant-scoped audio sync');
	}
	if (steps.acquireStreamingPriority) {
		adapter.setStreamingPriorityInternal(true);
	}
	if (steps.enforcePublicationCap && participant) {
		await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
	}
	if (steps.applyState && steps.applyStatePosition === 'before-pipeline') {
		steps.applyState(active);
	}
	if (active && steps.publishPipeline && participant) {
		await runScreenSharePublishPipeline(adapter, room, participant, steps.publishPipeline);
	}
	if (!active && steps.deactivateCleanup) {
		await steps.deactivateCleanup();
	}
	if (steps.applyState && steps.applyStatePosition === 'after-pipeline') {
		steps.applyState(active);
	}
	if (steps.audioSync.kind === 'self-stream-before-watch') {
		LocalVoiceState.updateSelfStreamAudio(steps.audioSync.published);
	}
	if (steps.updateLocalParticipant) {
		updateLocalParticipantFromRoom(room);
	}
	adapter.syncLocalStreamWatchStateInternal(active);
	if (steps.audioSync.kind === 'participant-after-watch' && participant) {
		adapter.syncLocalScreenShareAudioStateInternal(participant, active);
	}
	if (active && steps.syncPersistedAudioPreferenceWhenActive && participant) {
		adapter.syncPersistedScreenShareAudioPreferenceInternal(participant);
	}
	if (steps.playSound) {
		SoundCommands.playSound(active ? SoundType.ScreenShareStart : SoundType.ScreenShareStop);
	}
	if (steps.buildResolveTransition) {
		adapter.transitionScreenShareLifecycleInternal(steps.buildResolveTransition());
	}
}

export interface ScreenShareFailureSettleArgs {
	readonly adapter: VoiceEngineV2AppScreenShareExecutionAdapter;
	readonly room: Room | null;
	readonly participant: LocalParticipant;
	readonly actual: boolean;
	readonly applyState: (actual: boolean) => void;
	readonly onInactiveAfterSync: (() => void) | null;
	readonly monitorEndOnActive: boolean;
	readonly playSound: boolean;
	readonly buildTransition: (actual: boolean) => VoiceScreenShareEvent;
}

export function settleScreenShareFailure(args: ScreenShareFailureSettleArgs): boolean {
	const {adapter, room, participant, actual} = args;
	assert.ok(participant, 'participant is required to settle a screen-share failure');
	assert.equal(typeof actual, 'boolean', 'settle actual flag must be a boolean');
	args.applyState(actual);
	updateLocalParticipantFromRoom(room);
	adapter.syncLocalStreamWatchStateInternal(actual);
	adapter.syncLocalScreenShareAudioStateInternal(participant, actual);
	if (!actual) {
		args.onInactiveAfterSync?.();
	}
	if (actual && args.monitorEndOnActive) {
		adapter.monitorActiveScreenShareEndInternal(room, participant);
	}
	if (args.playSound) {
		SoundCommands.playSound(actual ? SoundType.ScreenShareStart : SoundType.ScreenShareStop);
	}
	adapter.transitionScreenShareLifecycleInternal(args.buildTransition(actual));
	return actual;
}

export function buildScreenShareFailureTransition(args: {
	readonly cancelled: boolean;
	readonly active: boolean;
	readonly sourceType: VoiceScreenShareSourceType | null;
}): VoiceScreenShareEvent {
	assert.equal(typeof args.cancelled, 'boolean', 'failure transition cancelled flag must be a boolean');
	assert.equal(typeof args.active, 'boolean', 'failure transition active flag must be a boolean');
	if (args.cancelled) {
		return {type: 'share.cancel', active: args.active, sourceType: args.sourceType};
	}
	return {type: 'share.reject', active: args.active, sourceType: args.sourceType};
}
