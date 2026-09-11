// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	PUBLICATION_MISSING_TIMEOUT_MS,
	PUBLISHER_REPUBLISH_GRACE_MS,
	VOICE_MEDIA_GRAPH_FIRST_FRAME_TIMEOUT_FAILURE,
	VOICE_MEDIA_GRAPH_PUBLICATION_MISSING_TIMEOUT_FAILURE,
	VOICE_MEDIA_GRAPH_REPUBLISH_TIMEOUT_FAILURE,
	VOICE_MEDIA_GRAPH_SUBSCRIPTION_ATTACH_TIMEOUT_FAILURE,
	type VoiceMediaGraphDeadline,
	type VoiceMediaGraphTimeoutFailureDescriptor,
	voiceMediaGraphDeferredStopDeadlineKey,
	voiceMediaGraphPublicationMissingDeadlineKey,
	voiceMediaGraphWatchAttemptDeadlineKey,
	WATCH_ATTEMPT_TIMEOUT_MS,
} from './VoiceMediaGraphDeadlines';
import {
	reconcileVoiceMediaGraphSubscriptionEntry,
	voiceMediaGraphCommandAlreadyQueued,
} from './VoiceMediaGraphReconcile';
import type {VoiceMediaGraphPartialTrackInfo} from './VoiceMediaGraphStats';
import {
	type VoiceMediaGraphStatsEntry,
	type VoiceMediaGraphStatsPlatform,
	type VoiceMediaGraphStatsTrackObservation,
	type VoiceMediaGraphStatsTrackTarget,
	voiceMediaGraphStatsObservationMatchesTarget,
	voiceMediaGraphStatsTrackKey,
} from './VoiceMediaGraphStatsObservations';
import type {
	VoiceMediaGraphPublicationLostEvent,
	VoiceMediaGraphPublicationObservedEvent,
	VoiceMediaGraphSubscriptionActualChangedEvent,
	VoiceMediaGraphSubscriptionActualState,
	VoiceMediaGraphSubscriptionCommand,
	VoiceMediaGraphSubscriptionCommandFailedEvent,
	VoiceMediaGraphSubscriptionContext,
	VoiceMediaGraphSubscriptionDesiredState,
	VoiceMediaGraphSubscriptionEntry,
	VoiceMediaGraphSubscriptionEvent,
	VoiceMediaGraphSubscriptionFirstFrameState,
	VoiceMediaGraphSubscriptionIntersectionEvent,
	VoiceMediaGraphSubscriptionObservedElement,
	VoiceMediaGraphSubscriptionPublicationState,
	VoiceMediaGraphSubscriptionReattachAfterPublishEvent,
	VoiceMediaGraphSubscriptionReplaceObserverEvent,
	VoiceMediaGraphSubscriptionSetContextEvent,
	VoiceMediaGraphSubscriptionSetEnabledEvent,
	VoiceMediaGraphSubscriptionSetQualityEvent,
	VoiceMediaGraphSubscriptionSubscribeEvent,
	VoiceMediaGraphSubscriptionTarget,
	VoiceMediaGraphVideoQuality,
} from './VoiceMediaGraphSubscriptionTypes';
import type {VoiceTrackSource} from './VoiceTrackSource';

export function voiceMediaGraphAttemptKeyIsOperation(attemptKey: string): boolean {
	assert.ok(attemptKey.length > 0, 'attemptKey is required');
	return attemptKey.includes(':operation:');
}

export {systemVoiceMediaGraphClock, type VoiceMediaGraphClockPort} from './VoiceMediaGraphClock';
export {
	buildVoiceMediaGraphNativeCameraQualityCommand,
	buildVoiceMediaGraphNativeCameraSubscriptionCommand,
	buildVoiceMediaGraphNativeScreenShareEnabledCommand,
	buildVoiceMediaGraphNativeScreenShareQualityCommand,
	buildVoiceMediaGraphNativeScreenShareSubscriptionCommands,
} from './VoiceMediaGraphCommands';
export {
	PUBLICATION_MISSING_TIMEOUT_MS,
	PUBLISHER_REPUBLISH_GRACE_MS,
	VOICE_MEDIA_GRAPH_REPUBLISH_TIMEOUT_FAILURE,
	type VoiceMediaGraphDeadline,
	type VoiceMediaGraphDeadlineKind,
	voiceMediaGraphDeferredStopDeadlineKey,
	voiceMediaGraphPublicationMissingDeadlineKey,
	voiceMediaGraphWatchAttemptDeadlineKey,
	WATCH_ATTEMPT_TIMEOUT_MS,
} from './VoiceMediaGraphDeadlines';
export {reconcileVoiceMediaGraphSubscriptionEntry, voiceMediaGraphCommandsEquivalent} from './VoiceMediaGraphReconcile';
export {
	mergeVoiceMediaGraphTrackInfo,
	type VoiceMediaGraphNativeStatsTarget,
	type VoiceMediaGraphPartialTrackInfo,
	type VoiceMediaGraphPerTrackStatsTarget,
	type VoiceMediaGraphTrackInfo,
} from './VoiceMediaGraphStats';
export type {
	VoiceMediaGraphStatsDirection,
	VoiceMediaGraphStatsEntry,
	VoiceMediaGraphStatsKind,
	VoiceMediaGraphStatsPlatform,
	VoiceMediaGraphStatsTrackObservation,
	VoiceMediaGraphStatsTrackTarget,
} from './VoiceMediaGraphStatsObservations';
export type {
	VoiceMediaGraphPublicationLostEvent,
	VoiceMediaGraphPublicationObservedEvent,
	VoiceMediaGraphRemoteSubscriptionCommand,
	VoiceMediaGraphRemoteTrackSubscriptionController,
	VoiceMediaGraphSubscriptionActualChangedEvent,
	VoiceMediaGraphSubscriptionActualError,
	VoiceMediaGraphSubscriptionActualState,
	VoiceMediaGraphSubscriptionCleanupEvent,
	VoiceMediaGraphSubscriptionClearCommandsEvent,
	VoiceMediaGraphSubscriptionCommand,
	VoiceMediaGraphSubscriptionCommandFailedEvent,
	VoiceMediaGraphSubscriptionContext,
	VoiceMediaGraphSubscriptionDesiredState,
	VoiceMediaGraphSubscriptionEntry,
	VoiceMediaGraphSubscriptionEvent,
	VoiceMediaGraphSubscriptionFirstFrameState,
	VoiceMediaGraphSubscriptionIntersectionEvent,
	VoiceMediaGraphSubscriptionObservedElement,
	VoiceMediaGraphSubscriptionPublicationMissingEvent,
	VoiceMediaGraphSubscriptionPublicationState,
	VoiceMediaGraphSubscriptionReattachAfterPublishEvent,
	VoiceMediaGraphSubscriptionReconcileEvent,
	VoiceMediaGraphSubscriptionReplaceObserverEvent,
	VoiceMediaGraphSubscriptionSetContextEvent,
	VoiceMediaGraphSubscriptionSetEnabledEvent,
	VoiceMediaGraphSubscriptionSetQualityEvent,
	VoiceMediaGraphSubscriptionSubscribeEvent,
	VoiceMediaGraphSubscriptionTarget,
	VoiceMediaGraphSubscriptionUnsubscribeEvent,
	VoiceMediaGraphVideoQuality,
} from './VoiceMediaGraphSubscriptionTypes';

export const VOICE_MEDIA_GRAPH_ENTRY_LIMIT = 256;
export const VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE = 'screen_share';
const VOICE_MEDIA_GRAPH_COMMAND_LIMIT = VOICE_MEDIA_GRAPH_ENTRY_LIMIT * 4;
const EMPTY_SUBSCRIPTION_COMMANDS: ReadonlyArray<VoiceMediaGraphSubscriptionCommand> = [];
const EMPTY_STRING_SET: ReadonlySet<string> = new Set();

export interface VoiceMediaGraphFailure {
	code: number;
	reason: string;
	reportedAt: number;
	streamKey?: string;
	participantIdentity?: string;
	participantSid?: string;
	trackSid?: string;
	source?: string;
	error?: unknown;
	generation?: number;
}

export interface VoiceMediaGraphFailureTarget {
	streamKey?: string | null;
	participantIdentity?: string | null;
	participantSid?: string | null;
	trackSid?: string | null;
	source?: string | null;
}

export interface VoiceMediaGraphWatchAttempt {
	attemptKey: string;
	startedAt: number;
	hasRenderedVideoFrame: boolean;
	generation: number;
	holders: number;
}

export type VoiceMediaGraphWatchStopReason = 'republish-grace-expired';

export interface VoiceMediaGraphWatchIntent {
	viewerStreamKeys: ReadonlyArray<string>;
	deferredStopKeys: ReadonlySet<string>;
}

export type VoiceMediaGraphWatchIntentEvent =
	| {type: 'watchIntent.replace'; keys: ReadonlyArray<string> | null | undefined}
	| {type: 'watchIntent.add'; key: string}
	| {type: 'watchIntent.remove'; key: string}
	| {type: 'watchIntent.removeMany'; keys: ReadonlyArray<string>}
	| {type: 'watchIntent.deferRemove'; key: string; at?: number}
	| {type: 'watchIntent.cancelDeferredRemove'; key: string}
	| {type: 'watchIntent.reset'};

export interface VoiceMediaGraphSnapshot<TFailure extends VoiceMediaGraphFailure = VoiceMediaGraphFailure> {
	failuresByKey: ReadonlyMap<string, TFailure>;
	watchGenerationByStreamKey: ReadonlyMap<string, number>;
	attemptsByStreamKey: ReadonlyMap<string, VoiceMediaGraphWatchAttempt>;
	watchStopReasonByStreamKey: ReadonlyMap<string, VoiceMediaGraphWatchStopReason>;
	watchIntent: VoiceMediaGraphWatchIntent;
	subscriptionsByKey: ReadonlyMap<string, VoiceMediaGraphSubscriptionEntry>;
	subscriptionCommands: ReadonlyArray<VoiceMediaGraphSubscriptionCommand>;
	deadlinesByKey: ReadonlyMap<string, VoiceMediaGraphDeadline>;
	statsConnectionId: string | null;
	statsByTrackKey: ReadonlyMap<string, VoiceMediaGraphStatsEntry>;
}

export type VoiceMediaGraphEvent<TFailure extends VoiceMediaGraphFailure = VoiceMediaGraphFailure> =
	| {type: 'watch.started'; streamKey: string; at?: number}
	| {type: 'watch.stopped'; streamKey: string}
	| {type: 'watch.attemptEnsured'; streamKey: string; attemptKey: string; startedAt: number; generation?: number}
	| {type: 'watch.attemptReleased'; streamKey: string; attemptKey: string}
	| {type: 'watch.renderedFrame'; streamKey: string; attemptKey: string; renderedAt: number; generation?: number}
	| {type: 'failure.reported'; failure: TFailure; generation?: number}
	| {type: 'failure.cleared'; target: VoiceMediaGraphFailureTarget}
	| {type: 'failureWatch.clearAll'}
	| VoiceMediaGraphPublicationObservedEvent
	| VoiceMediaGraphPublicationLostEvent
	| {type: 'time.deadlineFired'; key: string; at: number}
	| {
			type: 'stats.observed';
			at: number;
			connectionId: string;
			platform: VoiceMediaGraphStatsPlatform;
			tracks: ReadonlyArray<VoiceMediaGraphStatsTrackObservation>;
	  }
	| {type: 'stats.connectionChanged'; connectionId: string | null}
	| VoiceMediaGraphWatchIntentEvent
	| VoiceMediaGraphSubscriptionEvent
	| {type: 'clear.all'};

export function createVoiceMediaGraphSnapshot<
	TFailure extends VoiceMediaGraphFailure = VoiceMediaGraphFailure,
>(): VoiceMediaGraphSnapshot<TFailure> {
	return {
		failuresByKey: new Map(),
		watchGenerationByStreamKey: new Map(),
		attemptsByStreamKey: new Map(),
		watchStopReasonByStreamKey: new Map(),
		watchIntent: {
			viewerStreamKeys: [],
			deferredStopKeys: EMPTY_STRING_SET,
		},
		subscriptionsByKey: new Map(),
		subscriptionCommands: EMPTY_SUBSCRIPTION_COMMANDS,
		deadlinesByKey: new Map(),
		statsConnectionId: null,
		statsByTrackKey: new Map(),
	};
}

export function voiceMediaGraphParticipantSourceKey(participantIdentity: string, source: string): string {
	assert.ok(participantIdentity.length > 0, 'participantIdentity is required');
	assert.ok(source.length > 0, 'source is required');
	return `${participantIdentity}:${source}`;
}

export function voiceMediaGraphParticipantSidSourceKey(participantSid: string, source: string): string {
	assert.ok(participantSid.length > 0, 'participantSid is required');
	assert.ok(source.length > 0, 'source is required');
	return `${participantSid}:${source}`;
}

export function voiceMediaGraphConnectionIdFromStreamKey(streamKey: string): string | null {
	const parts = streamKey.split(':');
	if (parts.length === 2 && parts[0] === 'stream') return parts[1] || null;
	if (parts.length === 3) return parts[2] || null;
	return null;
}

export function voiceMediaGraphConnectionIdFromParticipantIdentity(
	participantIdentity: string | null | undefined,
): string | null {
	const match = participantIdentity?.match(/^user_[^_]+_(.+)$/);
	return match?.[1] ?? null;
}

export function voiceMediaGraphFailureMatchesTarget(
	failure: VoiceMediaGraphFailure,
	target: VoiceMediaGraphFailureTarget,
): boolean {
	if (target.trackSid && failure.trackSid && failure.trackSid !== target.trackSid) return false;
	if (target.source && failure.source && failure.source !== target.source) return false;
	return true;
}

export function voiceMediaGraphFailureMatchesWatchStart(failure: VoiceMediaGraphFailure, streamKey: string): boolean {
	if (failure.streamKey === streamKey) return true;
	const connectionId = voiceMediaGraphConnectionIdFromStreamKey(streamKey);
	if (!connectionId) return false;
	if (voiceMediaGraphConnectionIdFromStreamKey(failure.streamKey ?? '') === connectionId) return true;
	return voiceMediaGraphConnectionIdFromParticipantIdentity(failure.participantIdentity) === connectionId;
}

function voiceMediaGraphFailureKey(failure: VoiceMediaGraphFailure): string {
	const source = failure.source ?? VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE;
	if (failure.streamKey) return `stream:${failure.streamKey}:${source}`;
	if (failure.trackSid) return `track:${failure.trackSid}:${source}`;
	if (failure.participantIdentity) return `identity:${failure.participantIdentity}:${source}`;
	if (failure.participantSid) return `sid:${failure.participantSid}:${source}`;
	return `unknown:${failure.reportedAt}:${source}`;
}

function mapSetBounded<K, V>(current: ReadonlyMap<K, V>, key: K, value: V): ReadonlyMap<K, V> {
	assert.ok(current.size <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'voice media graph map exceeded limit before set');
	const next = new Map(current);
	if (!next.has(key) && next.size >= VOICE_MEDIA_GRAPH_ENTRY_LIMIT) {
		const oldestKey = next.keys().next().value as K | undefined;
		assert.ok(oldestKey !== undefined, 'bounded map eviction requires an oldest key');
		next.delete(oldestKey);
	}
	next.set(key, value);
	assert.ok(next.size <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'voice media graph map exceeded limit after set');
	return next;
}

function mapDelete<K, V>(current: ReadonlyMap<K, V>, key: K): ReadonlyMap<K, V> {
	if (!current.has(key)) return current;
	const next = new Map(current);
	next.delete(key);
	return next;
}

function clearFailuresMatching<TFailure extends VoiceMediaGraphFailure>(
	failures: ReadonlyMap<string, TFailure>,
	matches: (failure: TFailure) => boolean,
): ReadonlyMap<string, TFailure> {
	let next: Map<string, TFailure> | null = null;
	let visited = 0;
	for (const [key, failure] of failures) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'failure scan exceeded graph limit');
		if (!matches(failure)) continue;
		next ??= new Map(failures);
		next.delete(key);
	}
	return next ?? failures;
}

function deleteDeadlinesForStreamKey(
	deadlines: ReadonlyMap<string, VoiceMediaGraphDeadline>,
	streamKey: string,
): ReadonlyMap<string, VoiceMediaGraphDeadline> {
	let next: Map<string, VoiceMediaGraphDeadline> | null = null;
	let visited = 0;
	for (const [key, deadline] of deadlines) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'deadline scan exceeded graph limit');
		if (deadline.streamKey !== streamKey) continue;
		next ??= new Map(deadlines);
		next.delete(key);
	}
	return next ?? deadlines;
}

function pruneDeferredStopDeadlines(
	deadlines: ReadonlyMap<string, VoiceMediaGraphDeadline>,
	deferredStopKeys: ReadonlySet<string>,
): ReadonlyMap<string, VoiceMediaGraphDeadline> {
	let next: Map<string, VoiceMediaGraphDeadline> | null = null;
	let visited = 0;
	for (const [key, deadline] of deadlines) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'deferred deadline scan exceeded graph limit');
		if (deadline.kind !== 'deferredStop') continue;
		if (deadline.streamKey !== null && deferredStopKeys.has(deadline.streamKey)) continue;
		next ??= new Map(deadlines);
		next.delete(key);
	}
	return next ?? deadlines;
}

function isStaleGenerationEvent(
	snapshot: VoiceMediaGraphSnapshot<VoiceMediaGraphFailure>,
	streamKey: string | null | undefined,
	generation: number | undefined,
): boolean {
	if (generation === undefined) return false;
	if (!streamKey) return false;
	return generation < (snapshot.watchGenerationByStreamKey.get(streamKey) ?? 0);
}

function resolveViewerStreamKeyForParticipantIdentity(
	snapshot: VoiceMediaGraphSnapshot<VoiceMediaGraphFailure>,
	participantIdentity: string,
): string | null {
	const connectionId = voiceMediaGraphConnectionIdFromParticipantIdentity(participantIdentity);
	if (!connectionId) return null;
	assert.ok(
		snapshot.watchIntent.viewerStreamKeys.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT,
		'viewer stream key scan exceeded graph limit',
	);
	for (const streamKey of snapshot.watchIntent.viewerStreamKeys) {
		if (voiceMediaGraphConnectionIdFromStreamKey(streamKey) === connectionId) return streamKey;
	}
	return null;
}

function transitionWatchStarted<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	streamKey: string,
	at: number | undefined,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!streamKey) return snapshot;
	const generation = (snapshot.watchGenerationByStreamKey.get(streamKey) ?? 0) + 1;
	let deadlinesByKey = deleteDeadlinesForStreamKey(snapshot.deadlinesByKey, streamKey);
	if (at !== undefined) {
		deadlinesByKey = mapSetBounded(deadlinesByKey, voiceMediaGraphWatchAttemptDeadlineKey(streamKey), {
			kind: 'watchAttempt',
			streamKey,
			subscriptionKey: null,
			generation,
			attemptKey: null,
			dueAt: at + WATCH_ATTEMPT_TIMEOUT_MS,
		});
	}
	return {
		...snapshot,
		failuresByKey: clearFailuresMatching(snapshot.failuresByKey, (failure) =>
			voiceMediaGraphFailureMatchesWatchStart(failure, streamKey),
		),
		watchGenerationByStreamKey: mapSetBounded(snapshot.watchGenerationByStreamKey, streamKey, generation),
		attemptsByStreamKey: mapDelete(snapshot.attemptsByStreamKey, streamKey),
		watchStopReasonByStreamKey: mapDelete(snapshot.watchStopReasonByStreamKey, streamKey),
		deadlinesByKey,
	};
}

function transitionWatchStopped<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	streamKey: string,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!streamKey) return snapshot;
	return {
		...snapshot,
		failuresByKey: clearFailuresMatching(snapshot.failuresByKey, (failure) =>
			voiceMediaGraphFailureMatchesWatchStart(failure, streamKey),
		),
		watchGenerationByStreamKey: mapDelete(snapshot.watchGenerationByStreamKey, streamKey),
		attemptsByStreamKey: mapDelete(snapshot.attemptsByStreamKey, streamKey),
		deadlinesByKey: deleteDeadlinesForStreamKey(snapshot.deadlinesByKey, streamKey),
	};
}

function transitionFailureWatchClearAll<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
): VoiceMediaGraphSnapshot<TFailure> {
	return {
		...snapshot,
		failuresByKey: new Map(),
		watchGenerationByStreamKey: new Map(),
		attemptsByStreamKey: new Map(),
		watchStopReasonByStreamKey: new Map(),
		deadlinesByKey: new Map(),
	};
}

export function normalizeVoiceMediaGraphViewerStreamKeys(
	keys: ReadonlyArray<string> | null | undefined,
): Array<string> {
	if (!keys || keys.length === 0) return [];
	assert.ok(keys.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'viewer stream key input exceeded graph limit');
	const normalized: Array<string> = [];
	const seen = new Set<string>();
	for (const key of keys) {
		assert.ok(normalized.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'viewer stream key normalization exceeded limit');
		if (!key || seen.has(key)) continue;
		seen.add(key);
		normalized.push(key);
	}
	return normalized;
}

function stringSetsEqual(left: ReadonlySet<string>, right: ReadonlySet<string>): boolean {
	if (left === right) return true;
	if (left.size !== right.size) return false;
	let visited = 0;
	for (const key of left) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'string set equality exceeded graph limit');
		if (!right.has(key)) return false;
	}
	return true;
}

function orderedStringArraysEqual(left: ReadonlyArray<string>, right: ReadonlyArray<string>): boolean {
	if (left.length !== right.length) return false;
	assert.ok(left.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'string array equality exceeded graph limit');
	for (let index = 0; index < left.length; index += 1) {
		if (left[index] !== right[index]) return false;
	}
	return true;
}

function normalizeDeferredStopKeys(
	deferredStopKeys: Iterable<string> | null | undefined,
	viewerStreamKeys: ReadonlyArray<string>,
): ReadonlySet<string> {
	if (!deferredStopKeys) return EMPTY_STRING_SET;
	const viewerSet = new Set(viewerStreamKeys);
	const normalized = new Set<string>();
	let visited = 0;
	for (const key of deferredStopKeys) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'deferred stop key normalization exceeded graph limit');
		if (viewerSet.has(key)) normalized.add(key);
	}
	return normalized.size === 0 ? EMPTY_STRING_SET : normalized;
}

function setVoiceMediaGraphWatchIntent<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	watchIntent: VoiceMediaGraphWatchIntent,
): VoiceMediaGraphSnapshot<TFailure> {
	if (
		orderedStringArraysEqual(snapshot.watchIntent.viewerStreamKeys, watchIntent.viewerStreamKeys) &&
		stringSetsEqual(snapshot.watchIntent.deferredStopKeys, watchIntent.deferredStopKeys)
	) {
		return snapshot;
	}
	const deadlinesByKey = pruneDeferredStopDeadlines(snapshot.deadlinesByKey, watchIntent.deferredStopKeys);
	return {...snapshot, watchIntent, deadlinesByKey};
}

function removeManyFromWatchIntentKeys(keys: ReadonlyArray<string>, removedKeys: ReadonlySet<string>): Array<string> {
	if (removedKeys.size === 0) return [...keys];
	assert.ok(keys.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'watch key removal exceeded graph limit');
	return keys.filter((key) => !removedKeys.has(key));
}

function removeManyFromDeferredStopKeys(
	keys: ReadonlySet<string>,
	removedKeys: ReadonlySet<string>,
): ReadonlySet<string> {
	if (keys.size === 0 || removedKeys.size === 0) return keys;
	const next = new Set<string>();
	let visited = 0;
	for (const key of keys) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'deferred stop key removal exceeded graph limit');
		if (!removedKeys.has(key)) next.add(key);
	}
	return next.size === 0 ? EMPTY_STRING_SET : next;
}

function transitionWatchIntentReplace<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	keys: ReadonlyArray<string> | null | undefined,
): VoiceMediaGraphSnapshot<TFailure> {
	const viewerStreamKeys = normalizeVoiceMediaGraphViewerStreamKeys(keys);
	const deferredStopKeys = normalizeDeferredStopKeys(snapshot.watchIntent.deferredStopKeys, viewerStreamKeys);
	return setVoiceMediaGraphWatchIntent(snapshot, {viewerStreamKeys, deferredStopKeys});
}

function transitionWatchIntentAdd<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	key: string,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!key) return snapshot;
	if (snapshot.watchIntent.viewerStreamKeys.includes(key)) {
		return transitionWatchIntentCancelDeferredRemove(snapshot, key);
	}
	const viewerStreamKeys = normalizeVoiceMediaGraphViewerStreamKeys([...snapshot.watchIntent.viewerStreamKeys, key]);
	return setVoiceMediaGraphWatchIntent(snapshot, {
		viewerStreamKeys,
		deferredStopKeys: snapshot.watchIntent.deferredStopKeys,
	});
}

function transitionWatchIntentRemoveMany<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	keys: ReadonlyArray<string>,
): VoiceMediaGraphSnapshot<TFailure> {
	const removedKeys = new Set(normalizeVoiceMediaGraphViewerStreamKeys(keys));
	const viewerStreamKeys = removeManyFromWatchIntentKeys(snapshot.watchIntent.viewerStreamKeys, removedKeys);
	const deferredStopKeys = removeManyFromDeferredStopKeys(snapshot.watchIntent.deferredStopKeys, removedKeys);
	return setVoiceMediaGraphWatchIntent(snapshot, {viewerStreamKeys, deferredStopKeys});
}

function transitionWatchIntentDeferRemove<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	key: string,
	at: number | undefined,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!key) return snapshot;
	if (!snapshot.watchIntent.viewerStreamKeys.includes(key)) return snapshot;
	let next = snapshot;
	if (!snapshot.watchIntent.deferredStopKeys.has(key)) {
		const deferredStopKeys = new Set(snapshot.watchIntent.deferredStopKeys);
		deferredStopKeys.add(key);
		next = setVoiceMediaGraphWatchIntent(snapshot, {
			viewerStreamKeys: snapshot.watchIntent.viewerStreamKeys,
			deferredStopKeys,
		});
	}
	if (at === undefined) return next;
	const deadlinesByKey = mapSetBounded(next.deadlinesByKey, voiceMediaGraphDeferredStopDeadlineKey(key), {
		kind: 'deferredStop',
		streamKey: key,
		subscriptionKey: null,
		generation: next.watchGenerationByStreamKey.get(key) ?? 0,
		attemptKey: null,
		dueAt: at + PUBLISHER_REPUBLISH_GRACE_MS,
	});
	return {...next, deadlinesByKey};
}

function transitionWatchIntentCancelDeferredRemove<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	key: string,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!snapshot.watchIntent.deferredStopKeys.has(key)) return snapshot;
	const deferredStopKeys = new Set(snapshot.watchIntent.deferredStopKeys);
	deferredStopKeys.delete(key);
	return setVoiceMediaGraphWatchIntent(snapshot, {
		viewerStreamKeys: snapshot.watchIntent.viewerStreamKeys,
		deferredStopKeys: deferredStopKeys.size === 0 ? EMPTY_STRING_SET : deferredStopKeys,
	});
}

export function voiceMediaGraphSubscriptionKey(target: VoiceMediaGraphSubscriptionTarget): string {
	return voiceMediaGraphParticipantSourceKey(target.participantIdentity, target.source);
}

export function createVoiceMediaGraphSubscriptionActualState(): VoiceMediaGraphSubscriptionActualState {
	return {subscribed: null, enabled: null, quality: null, lastCommandAt: null, lastError: null};
}

export function createVoiceMediaGraphSubscriptionPublicationState(
	available: boolean,
): VoiceMediaGraphSubscriptionPublicationState {
	return {available, trackSid: null, observedAt: null};
}

interface VoiceMediaGraphSubscriptionEntryParts {
	desired: VoiceMediaGraphSubscriptionDesiredState;
	actual: VoiceMediaGraphSubscriptionActualState;
	publication: VoiceMediaGraphSubscriptionPublicationState;
	firstFrame: VoiceMediaGraphSubscriptionFirstFrameState;
	subscribed: boolean;
}

export function buildVoiceMediaGraphSubscriptionEntry(
	target: VoiceMediaGraphSubscriptionTarget,
	parts: VoiceMediaGraphSubscriptionEntryParts,
): VoiceMediaGraphSubscriptionEntry {
	return {
		participantIdentity: target.participantIdentity,
		source: target.source,
		desired: parts.desired,
		actual: parts.actual,
		publication: parts.publication,
		firstFrame: parts.firstFrame,
		subscribed: parts.subscribed,
		publicationAvailable: parts.publication.available,
		enabled: parts.desired.enabled,
		quality: parts.desired.quality,
		context: parts.desired.context,
		isIntersecting: parts.desired.isIntersecting,
		observedElement: parts.desired.observedElement,
	};
}

function subscriptionEntryParts(entry: VoiceMediaGraphSubscriptionEntry): VoiceMediaGraphSubscriptionEntryParts {
	return {
		desired: entry.desired,
		actual: entry.actual,
		publication: entry.publication,
		firstFrame: entry.firstFrame,
		subscribed: entry.subscribed,
	};
}

function withSubscriptionDesired(
	entry: VoiceMediaGraphSubscriptionEntry,
	patch: Partial<VoiceMediaGraphSubscriptionDesiredState>,
): VoiceMediaGraphSubscriptionEntry {
	return buildVoiceMediaGraphSubscriptionEntry(entry, {
		...subscriptionEntryParts(entry),
		desired: {...entry.desired, ...patch},
	});
}

function publicationWithAvailability(
	publication: VoiceMediaGraphSubscriptionPublicationState,
	available: boolean,
): VoiceMediaGraphSubscriptionPublicationState {
	if (publication.available === available) return publication;
	return {available, trackSid: available ? publication.trackSid : null, observedAt: publication.observedAt};
}

function voiceMediaGraphSubscriptionCommandTarget(
	target: VoiceMediaGraphSubscriptionTarget,
): VoiceMediaGraphSubscriptionTarget {
	return {participantIdentity: target.participantIdentity, source: target.source};
}

export function getVoiceMediaGraphSubscriptionQualityForContext(
	context: VoiceMediaGraphSubscriptionContext,
): VoiceMediaGraphVideoQuality {
	switch (context) {
		case 'focused':
			return 'high';
		case 'carousel':
			return 'medium';
		case 'hidden':
			return 'low';
	}
}

export function shouldEnableVoiceMediaGraphSubscription(
	context: VoiceMediaGraphSubscriptionContext,
	isIntersecting: boolean,
	observedElement: VoiceMediaGraphSubscriptionObservedElement,
): boolean {
	if (context === 'focused') return true;
	if (context === 'hidden') return false;
	return observedElement == null || isIntersecting;
}

function appendVoiceMediaGraphSubscriptionCommands<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	commands: ReadonlyArray<VoiceMediaGraphSubscriptionCommand>,
): VoiceMediaGraphSnapshot<TFailure> {
	if (commands.length === 0) return snapshot;
	const subscriptionCommands = [...snapshot.subscriptionCommands, ...commands];
	assert.ok(
		subscriptionCommands.length <= VOICE_MEDIA_GRAPH_COMMAND_LIMIT,
		'voice media graph command queue exceeded limit',
	);
	return {...snapshot, subscriptionCommands};
}

function setVoiceMediaGraphSubscriptionEntry<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	entry: VoiceMediaGraphSubscriptionEntry,
	commands: ReadonlyArray<VoiceMediaGraphSubscriptionCommand> = EMPTY_SUBSCRIPTION_COMMANDS,
): VoiceMediaGraphSnapshot<TFailure> {
	const subscriptionsByKey = mapSetBounded(snapshot.subscriptionsByKey, voiceMediaGraphSubscriptionKey(entry), entry);
	return appendVoiceMediaGraphSubscriptionCommands({...snapshot, subscriptionsByKey}, commands);
}

function deleteVoiceMediaGraphSubscriptionEntry<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	target: VoiceMediaGraphSubscriptionTarget,
	commands: ReadonlyArray<VoiceMediaGraphSubscriptionCommand>,
): VoiceMediaGraphSnapshot<TFailure> {
	const key = voiceMediaGraphSubscriptionKey(target);
	if (!snapshot.subscriptionsByKey.has(key)) return snapshot;
	const subscriptionsByKey = mapDelete(snapshot.subscriptionsByKey, key);
	return appendVoiceMediaGraphSubscriptionCommands({...snapshot, subscriptionsByKey}, commands);
}

function observerReplacementCommands(
	target: VoiceMediaGraphSubscriptionTarget,
	previousElement: VoiceMediaGraphSubscriptionObservedElement,
	nextElement: VoiceMediaGraphSubscriptionObservedElement,
): Array<VoiceMediaGraphSubscriptionCommand> {
	if (previousElement === nextElement) return [];
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	const commandTarget = voiceMediaGraphSubscriptionCommandTarget(target);
	if (previousElement) commands.push({type: 'disconnectObserver', ...commandTarget});
	if (nextElement) commands.push({type: 'observeElement', ...commandTarget, element: nextElement});
	return commands;
}

function subscribePublicationCommand(
	target: VoiceMediaGraphSubscriptionTarget,
	entry: Pick<VoiceMediaGraphSubscriptionEntry, 'enabled' | 'quality'>,
): VoiceMediaGraphSubscriptionCommand {
	const commandTarget = voiceMediaGraphSubscriptionCommandTarget(target);
	return {
		type: 'subscribePublication',
		...commandTarget,
		enabled: entry.enabled,
		quality: entry.quality,
	};
}

function resubscribePublicationCommand(
	target: VoiceMediaGraphSubscriptionTarget,
	entry: Pick<VoiceMediaGraphSubscriptionEntry, 'enabled' | 'quality'>,
): VoiceMediaGraphSubscriptionCommand {
	const commandTarget = voiceMediaGraphSubscriptionCommandTarget(target);
	return {
		type: 'resubscribePublication',
		...commandTarget,
		enabled: entry.enabled,
		quality: entry.quality,
	};
}

function updateSubscriptionAvailability(
	entry: VoiceMediaGraphSubscriptionEntry,
	hasPublication: boolean,
): VoiceMediaGraphSubscriptionEntry {
	if (entry.publicationAvailable === hasPublication) return entry;
	return buildVoiceMediaGraphSubscriptionEntry(entry, {
		...subscriptionEntryParts(entry),
		publication: publicationWithAvailability(entry.publication, hasPublication),
	});
}

function subscribeToVoiceMediaGraphPublication<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionSubscribeEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const existing = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	const nextContext = event.context ?? existing?.context ?? 'carousel';
	const nextQuality =
		event.quality ??
		(event.context ? getVoiceMediaGraphSubscriptionQualityForContext(nextContext) : existing?.quality) ??
		'low';
	const elementChanged = existing ? existing.observedElement !== event.observedElement : event.observedElement != null;
	const isIntersecting = elementChanged ? false : (existing?.isIntersecting ?? false);
	const enabled = shouldEnableVoiceMediaGraphSubscription(nextContext, isIntersecting, event.observedElement);
	const nextEntry = buildVoiceMediaGraphSubscriptionEntry(event, {
		desired: {
			enabled,
			quality: nextQuality,
			context: nextContext,
			isIntersecting,
			observedElement: event.observedElement,
		},
		actual: existing?.actual ?? createVoiceMediaGraphSubscriptionActualState(),
		publication: publicationWithAvailability(
			existing?.publication ?? createVoiceMediaGraphSubscriptionPublicationState(event.hasPublication),
			event.hasPublication,
		),
		firstFrame: existing?.firstFrame ?? {renderedAt: null},
		subscribed: true,
	});
	const commands = observerReplacementCommands(event, existing?.observedElement ?? null, event.observedElement);
	if (event.hasPublication) {
		if (!existing || !existing.publicationAvailable) {
			commands.push(subscribePublicationCommand(event, nextEntry));
		} else {
			const commandTarget = voiceMediaGraphSubscriptionCommandTarget(event);
			if (existing.enabled !== enabled) commands.push({type: 'setPublicationEnabled', ...commandTarget, enabled});
			if (existing.quality !== nextQuality) {
				commands.push({type: 'setPublicationQuality', ...commandTarget, quality: nextQuality});
			}
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function unsubscribeFromVoiceMediaGraphPublication<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	target: VoiceMediaGraphSubscriptionTarget,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, target.participantIdentity, target.source);
	if (!entry) return snapshot;
	const commands = observerReplacementCommands(target, entry.observedElement, null);
	commands.push({type: 'unsubscribePublication', ...voiceMediaGraphSubscriptionCommandTarget(target)});
	return deleteVoiceMediaGraphSubscriptionEntry(snapshot, target, commands);
}

function replaceVoiceMediaGraphSubscriptionObserver<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionReplaceObserverEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry || entry.observedElement === event.observedElement) return snapshot;
	const enabled = shouldEnableVoiceMediaGraphSubscription(entry.context, false, event.observedElement);
	const nextEntry = updateSubscriptionAvailability(
		withSubscriptionDesired(entry, {observedElement: event.observedElement, isIntersecting: false, enabled}),
		event.hasPublication,
	);
	const commands = observerReplacementCommands(event, entry.observedElement, event.observedElement);
	if (event.hasPublication) {
		if (!entry.publicationAvailable) commands.push(subscribePublicationCommand(event, nextEntry));
		else if (entry.enabled !== enabled) {
			commands.push({
				type: 'setPublicationEnabled',
				...voiceMediaGraphSubscriptionCommandTarget(event),
				enabled,
			});
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function setVoiceMediaGraphSubscriptionIntersection<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionIntersectionEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	const enabled = shouldEnableVoiceMediaGraphSubscription(entry.context, event.isIntersecting, entry.observedElement);
	const nextEntry = updateSubscriptionAvailability(
		withSubscriptionDesired(entry, {isIntersecting: event.isIntersecting, enabled}),
		event.hasPublication,
	);
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	if (event.hasPublication) {
		if (!entry.publicationAvailable) commands.push(subscribePublicationCommand(event, nextEntry));
		else if (entry.enabled !== enabled) {
			commands.push({
				type: 'setPublicationEnabled',
				...voiceMediaGraphSubscriptionCommandTarget(event),
				enabled,
			});
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function setVoiceMediaGraphSubscriptionEnabled<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionSetEnabledEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	const nextEntry = updateSubscriptionAvailability(
		withSubscriptionDesired(entry, {enabled: event.enabled}),
		event.hasPublication,
	);
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	if (event.hasPublication) {
		if (!entry.publicationAvailable) commands.push(subscribePublicationCommand(event, nextEntry));
		else if (entry.enabled !== event.enabled) {
			commands.push({
				type: 'setPublicationEnabled',
				...voiceMediaGraphSubscriptionCommandTarget(event),
				enabled: event.enabled,
			});
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function setVoiceMediaGraphSubscriptionQuality<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionSetQualityEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	const nextEntry = updateSubscriptionAvailability(
		withSubscriptionDesired(entry, {quality: event.quality}),
		event.hasPublication,
	);
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	if (event.hasPublication) {
		if (!entry.publicationAvailable) commands.push(subscribePublicationCommand(event, nextEntry));
		else if (entry.quality !== event.quality) {
			commands.push({
				type: 'setPublicationQuality',
				...voiceMediaGraphSubscriptionCommandTarget(event),
				quality: event.quality,
			});
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function setVoiceMediaGraphSubscriptionContext<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionSetContextEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	const quality = getVoiceMediaGraphSubscriptionQualityForContext(event.context);
	const enabled = shouldEnableVoiceMediaGraphSubscription(event.context, entry.isIntersecting, entry.observedElement);
	const nextEntry = updateSubscriptionAvailability(
		withSubscriptionDesired(entry, {context: event.context, enabled, quality}),
		event.hasPublication,
	);
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	if (event.hasPublication) {
		if (!entry.publicationAvailable) {
			commands.push(subscribePublicationCommand(event, nextEntry));
		} else {
			const commandTarget = voiceMediaGraphSubscriptionCommandTarget(event);
			if (entry.enabled !== enabled) commands.push({type: 'setPublicationEnabled', ...commandTarget, enabled});
			if (entry.quality !== quality) commands.push({type: 'setPublicationQuality', ...commandTarget, quality});
		}
	}
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function reattachVoiceMediaGraphSubscriptionAfterPublish<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionReattachAfterPublishEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	const nextEntry = updateSubscriptionAvailability(entry, event.hasPublication);
	const commands = event.hasPublication
		? [
				event.forceResubscribe
					? resubscribePublicationCommand(event, nextEntry)
					: subscribePublicationCommand(event, nextEntry),
			]
		: EMPTY_SUBSCRIPTION_COMMANDS;
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry, commands);
}

function markVoiceMediaGraphSubscriptionPublicationMissing<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	target: VoiceMediaGraphSubscriptionTarget,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, target.participantIdentity, target.source);
	if (!entry || !entry.publicationAvailable) return snapshot;
	return setVoiceMediaGraphSubscriptionEntry(snapshot, updateSubscriptionAvailability(entry, false));
}

function cleanupVoiceMediaGraphSubscriptions<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	source: VoiceTrackSource | undefined,
): VoiceMediaGraphSnapshot<TFailure> {
	if (snapshot.subscriptionsByKey.size === 0) return snapshot;
	let subscriptionsByKey: Map<string, VoiceMediaGraphSubscriptionEntry> | null = null;
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	let visited = 0;
	for (const [key, entry] of snapshot.subscriptionsByKey) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'subscription cleanup exceeded graph limit');
		if (source !== undefined && entry.source !== source) continue;
		commands.push(...observerReplacementCommands(entry, entry.observedElement, null));
		commands.push({type: 'unsubscribePublication', ...voiceMediaGraphSubscriptionCommandTarget(entry)});
		subscriptionsByKey ??= new Map(snapshot.subscriptionsByKey);
		subscriptionsByKey.delete(key);
	}
	if (!subscriptionsByKey) return snapshot;
	return appendVoiceMediaGraphSubscriptionCommands({...snapshot, subscriptionsByKey}, commands);
}

function transitionSubscriptionActualChanged<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionActualChangedEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	if (isStaleGenerationEvent(snapshot, event.streamKey, event.generation)) return snapshot;
	const actual: VoiceMediaGraphSubscriptionActualState = {
		subscribed: event.subscribed !== undefined ? event.subscribed : entry.actual.subscribed,
		enabled: event.enabled !== undefined ? event.enabled : entry.actual.enabled,
		quality: event.quality !== undefined ? event.quality : entry.actual.quality,
		lastCommandAt: event.at,
		lastError: null,
	};
	let publication = entry.publication;
	if (event.trackSid !== undefined) {
		publication = {available: event.trackSid !== null, trackSid: event.trackSid, observedAt: event.at};
	}
	const nextEntry = buildVoiceMediaGraphSubscriptionEntry(entry, {
		...subscriptionEntryParts(entry),
		actual,
		publication,
	});
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry);
}

function transitionSubscriptionCommandFailed<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphSubscriptionCommandFailedEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) return snapshot;
	if (isStaleGenerationEvent(snapshot, event.streamKey, event.generation)) return snapshot;
	const actual: VoiceMediaGraphSubscriptionActualState = {
		...entry.actual,
		lastCommandAt: event.at,
		lastError: {code: event.code, reason: event.reason, at: event.at},
	};
	const nextEntry = buildVoiceMediaGraphSubscriptionEntry(entry, {...subscriptionEntryParts(entry), actual});
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry);
}

function transitionSubscriptionReconcile<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
): VoiceMediaGraphSnapshot<TFailure> {
	const commands: Array<VoiceMediaGraphSubscriptionCommand> = [];
	let visited = 0;
	for (const entry of snapshot.subscriptionsByKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'subscription reconcile exceeded graph limit');
		for (const command of reconcileVoiceMediaGraphSubscriptionEntry(entry)) {
			if (voiceMediaGraphCommandAlreadyQueued(snapshot.subscriptionCommands, command)) continue;
			if (voiceMediaGraphCommandAlreadyQueued(commands, command)) continue;
			commands.push(command);
		}
	}
	return appendVoiceMediaGraphSubscriptionCommands(snapshot, commands);
}

function voiceMediaGraphFailureMatchesParticipantIdentity(
	failure: VoiceMediaGraphFailure,
	participantIdentity: string,
): boolean {
	if (failure.participantIdentity === participantIdentity) return true;
	const connectionId = voiceMediaGraphConnectionIdFromParticipantIdentity(participantIdentity);
	if (!connectionId) return false;
	return voiceMediaGraphConnectionIdFromStreamKey(failure.streamKey ?? '') === connectionId;
}

function transitionPublicationObserved<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphPublicationObservedEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const subscriptionKey = voiceMediaGraphSubscriptionKey(event);
	const deadlinesByKey = mapDelete(
		snapshot.deadlinesByKey,
		voiceMediaGraphPublicationMissingDeadlineKey(subscriptionKey),
	);
	const failuresByKey = clearFailuresMatching(
		snapshot.failuresByKey,
		(failure) =>
			failure.code === VOICE_MEDIA_GRAPH_PUBLICATION_MISSING_TIMEOUT_FAILURE.code &&
			voiceMediaGraphFailureMatchesParticipantIdentity(failure, event.participantIdentity),
	);
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (!entry) {
		if (deadlinesByKey === snapshot.deadlinesByKey && failuresByKey === snapshot.failuresByKey) return snapshot;
		return {...snapshot, deadlinesByKey, failuresByKey};
	}
	const nextEntry = buildVoiceMediaGraphSubscriptionEntry(entry, {
		...subscriptionEntryParts(entry),
		publication: {available: true, trackSid: event.trackSid, observedAt: event.at},
	});
	return setVoiceMediaGraphSubscriptionEntry({...snapshot, deadlinesByKey, failuresByKey}, nextEntry);
}

function transitionPublicationLost<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphPublicationLostEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	const subscriptionKey = voiceMediaGraphSubscriptionKey(event);
	let next = snapshot;
	const entry = selectVoiceMediaGraphSubscriptionEntry(snapshot, event.participantIdentity, event.source);
	if (entry) {
		const nextEntry = buildVoiceMediaGraphSubscriptionEntry(entry, {
			...subscriptionEntryParts(entry),
			actual: {...entry.actual, subscribed: null, enabled: null, quality: null},
			publication: {available: false, trackSid: null, observedAt: event.at},
			firstFrame: {renderedAt: null},
		});
		next = setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry);
	}
	const streamKey = resolveViewerStreamKeyForParticipantIdentity(next, event.participantIdentity);
	if (!streamKey) return next;
	const deadlinesByKey = mapSetBounded(
		next.deadlinesByKey,
		voiceMediaGraphPublicationMissingDeadlineKey(subscriptionKey),
		{
			kind: 'publicationMissing',
			streamKey,
			subscriptionKey,
			generation: next.watchGenerationByStreamKey.get(streamKey) ?? 0,
			attemptKey: null,
			dueAt: event.at + PUBLICATION_MISSING_TIMEOUT_MS,
		},
	);
	return {...next, deadlinesByKey};
}

function findScreenShareEntryForStreamKey(
	snapshot: VoiceMediaGraphSnapshot<VoiceMediaGraphFailure>,
	streamKey: string,
): VoiceMediaGraphSubscriptionEntry | null {
	const connectionId = voiceMediaGraphConnectionIdFromStreamKey(streamKey);
	if (!connectionId) return null;
	let visited = 0;
	for (const entry of snapshot.subscriptionsByKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'screen share entry scan exceeded graph limit');
		if (entry.source !== VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE) continue;
		if (voiceMediaGraphConnectionIdFromParticipantIdentity(entry.participantIdentity) === connectionId) return entry;
	}
	return null;
}

function watchAttemptTimeoutDescriptor(
	snapshot: VoiceMediaGraphSnapshot<VoiceMediaGraphFailure>,
	streamKey: string,
): VoiceMediaGraphTimeoutFailureDescriptor {
	const entry = findScreenShareEntryForStreamKey(snapshot, streamKey);
	if (!entry) return VOICE_MEDIA_GRAPH_SUBSCRIPTION_ATTACH_TIMEOUT_FAILURE;
	if (!entry.publication.available) return VOICE_MEDIA_GRAPH_PUBLICATION_MISSING_TIMEOUT_FAILURE;
	if (entry.actual.subscribed !== true) return VOICE_MEDIA_GRAPH_SUBSCRIPTION_ATTACH_TIMEOUT_FAILURE;
	return VOICE_MEDIA_GRAPH_FIRST_FRAME_TIMEOUT_FAILURE;
}

function reportDeadlineTimeoutFailure<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	deadline: VoiceMediaGraphDeadline,
	descriptor: VoiceMediaGraphTimeoutFailureDescriptor,
	at: number,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = deadline.streamKey ? findScreenShareEntryForStreamKey(snapshot, deadline.streamKey) : null;
	const failure: VoiceMediaGraphFailure = {
		code: descriptor.code,
		reason: descriptor.reason,
		reportedAt: at,
		source: VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
		generation: deadline.generation,
		...(deadline.streamKey ? {streamKey: deadline.streamKey} : {}),
		...(entry ? {participantIdentity: entry.participantIdentity} : {}),
	};
	return {
		...snapshot,
		failuresByKey: mapSetBounded(snapshot.failuresByKey, voiceMediaGraphFailureKey(failure), failure as TFailure),
	};
}

function applyWatchAttemptDeadline<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	deadline: VoiceMediaGraphDeadline,
	at: number,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!deadline.streamKey) return snapshot;
	const attempt = snapshot.attemptsByStreamKey.get(deadline.streamKey);
	if (!attempt) return snapshot;
	if (attempt.hasRenderedVideoFrame) return snapshot;
	if (deadline.attemptKey !== null && attempt.attemptKey !== deadline.attemptKey) return snapshot;
	const descriptor = voiceMediaGraphAttemptKeyIsOperation(attempt.attemptKey)
		? VOICE_MEDIA_GRAPH_REPUBLISH_TIMEOUT_FAILURE
		: watchAttemptTimeoutDescriptor(snapshot, deadline.streamKey);
	return reportDeadlineTimeoutFailure(snapshot, deadline, descriptor, at);
}

function applyDeferredStopDeadline<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	deadline: VoiceMediaGraphDeadline,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!deadline.streamKey) return snapshot;
	if (!snapshot.watchIntent.deferredStopKeys.has(deadline.streamKey)) return snapshot;
	const stopped: VoiceMediaGraphSnapshot<TFailure> = {
		...snapshot,
		watchStopReasonByStreamKey: mapSetBounded(
			snapshot.watchStopReasonByStreamKey,
			deadline.streamKey,
			'republish-grace-expired',
		),
	};
	return transitionWatchIntentRemoveMany(stopped, [deadline.streamKey]);
}

function applyPublicationMissingDeadline<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	deadline: VoiceMediaGraphDeadline,
	at: number,
): VoiceMediaGraphSnapshot<TFailure> {
	if (!deadline.streamKey) return snapshot;
	const entry = findScreenShareEntryForStreamKey(snapshot, deadline.streamKey);
	if (entry?.publication.available) return snapshot;
	return reportDeadlineTimeoutFailure(snapshot, deadline, VOICE_MEDIA_GRAPH_PUBLICATION_MISSING_TIMEOUT_FAILURE, at);
}

function transitionDeadlineFired<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	key: string,
	at: number,
): VoiceMediaGraphSnapshot<TFailure> {
	const deadline = snapshot.deadlinesByKey.get(key);
	if (!deadline) return snapshot;
	const base: VoiceMediaGraphSnapshot<TFailure> = {
		...snapshot,
		deadlinesByKey: mapDelete(snapshot.deadlinesByKey, key),
	};
	if (deadline.streamKey) {
		const currentGeneration = snapshot.watchGenerationByStreamKey.get(deadline.streamKey) ?? 0;
		if (deadline.generation !== currentGeneration) return base;
	}
	switch (deadline.kind) {
		case 'watchAttempt':
			return applyWatchAttemptDeadline(base, deadline, at);
		case 'deferredStop':
			return applyDeferredStopDeadline(base, deadline);
		case 'publicationMissing':
			return applyPublicationMissingDeadline(base, deadline, at);
	}
}

function transitionStatsObserved<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: {
		at: number;
		connectionId: string;
		platform: VoiceMediaGraphStatsPlatform;
		tracks: ReadonlyArray<VoiceMediaGraphStatsTrackObservation>;
	},
): VoiceMediaGraphSnapshot<TFailure> {
	if (!event.connectionId) return snapshot;
	if (snapshot.statsConnectionId !== null && snapshot.statsConnectionId !== event.connectionId) return snapshot;
	assert.ok(event.tracks.length <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'stats observation batch exceeded graph limit');
	let statsByTrackKey = snapshot.statsByTrackKey;
	for (const observation of event.tracks) {
		const trackKey = voiceMediaGraphStatsTrackKey(observation);
		if (!trackKey) continue;
		statsByTrackKey = mapSetBounded(statsByTrackKey, trackKey, {
			connectionId: event.connectionId,
			platform: event.platform,
			observedAt: event.at,
			observation,
		});
	}
	if (statsByTrackKey === snapshot.statsByTrackKey && snapshot.statsConnectionId === event.connectionId) {
		return snapshot;
	}
	return {...snapshot, statsConnectionId: event.connectionId, statsByTrackKey};
}

function transitionStatsConnectionChanged<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	connectionId: string | null,
): VoiceMediaGraphSnapshot<TFailure> {
	if (snapshot.statsConnectionId === connectionId) return snapshot;
	return {...snapshot, statsConnectionId: connectionId, statsByTrackKey: new Map()};
}

export function transitionVoiceMediaGraph<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphEvent<TFailure>,
): VoiceMediaGraphSnapshot<TFailure> {
	switch (event.type) {
		case 'watch.started':
			return transitionWatchStarted(snapshot, event.streamKey, event.at);
		case 'watch.stopped':
			return transitionWatchStopped(snapshot, event.streamKey);
		case 'watch.attemptEnsured':
			return ensureVoiceMediaGraphWatchAttempt(snapshot, event);
		case 'watch.attemptReleased':
			return releaseVoiceMediaGraphWatchAttempt(snapshot, event);
		case 'watch.renderedFrame':
			return markVoiceMediaGraphRenderedFrame(snapshot, event);
		case 'failure.reported':
			return transitionFailureReported(snapshot, event.failure, event.generation);
		case 'failure.cleared':
			return {...snapshot, failuresByKey: clearVoiceMediaGraphFailures(snapshot.failuresByKey, event.target)};
		case 'failureWatch.clearAll':
			return transitionFailureWatchClearAll(snapshot);
		case 'publication.observed':
			return transitionPublicationObserved(snapshot, event);
		case 'publication.lost':
			return transitionPublicationLost(snapshot, event);
		case 'time.deadlineFired':
			return transitionDeadlineFired(snapshot, event.key, event.at);
		case 'stats.observed':
			return transitionStatsObserved(snapshot, event);
		case 'stats.connectionChanged':
			return transitionStatsConnectionChanged(snapshot, event.connectionId);
		case 'clear.all':
			return createVoiceMediaGraphSnapshot<TFailure>();
		default:
			return transitionVoiceMediaGraphIntentOrSubscription(snapshot, event);
	}
}

function transitionVoiceMediaGraphIntentOrSubscription<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: VoiceMediaGraphWatchIntentEvent | VoiceMediaGraphSubscriptionEvent,
): VoiceMediaGraphSnapshot<TFailure> {
	switch (event.type) {
		case 'watchIntent.replace':
			return transitionWatchIntentReplace(snapshot, event.keys);
		case 'watchIntent.add':
			return transitionWatchIntentAdd(snapshot, event.key);
		case 'watchIntent.remove':
			return transitionWatchIntentRemoveMany(snapshot, [event.key]);
		case 'watchIntent.removeMany':
			return transitionWatchIntentRemoveMany(snapshot, event.keys);
		case 'watchIntent.deferRemove':
			return transitionWatchIntentDeferRemove(snapshot, event.key, event.at);
		case 'watchIntent.cancelDeferredRemove':
			return transitionWatchIntentCancelDeferredRemove(snapshot, event.key);
		case 'watchIntent.reset':
			return transitionWatchIntentReplace(snapshot, []);
		case 'subscription.subscribe':
			return subscribeToVoiceMediaGraphPublication(snapshot, event);
		case 'subscription.unsubscribe':
			return unsubscribeFromVoiceMediaGraphPublication(snapshot, event);
		case 'subscription.replaceObserver':
			return replaceVoiceMediaGraphSubscriptionObserver(snapshot, event);
		case 'subscription.intersection':
			return setVoiceMediaGraphSubscriptionIntersection(snapshot, event);
		case 'subscription.setEnabled':
			return setVoiceMediaGraphSubscriptionEnabled(snapshot, event);
		case 'subscription.setQuality':
			return setVoiceMediaGraphSubscriptionQuality(snapshot, event);
		case 'subscription.setContext':
			return setVoiceMediaGraphSubscriptionContext(snapshot, event);
		case 'subscription.reattachAfterPublish':
			return reattachVoiceMediaGraphSubscriptionAfterPublish(snapshot, event);
		case 'subscription.publicationMissing':
			return markVoiceMediaGraphSubscriptionPublicationMissing(snapshot, event);
		case 'subscription.cleanup':
			return cleanupVoiceMediaGraphSubscriptions(snapshot, event.source);
		case 'subscription.clearCommands':
			return snapshot.subscriptionCommands.length === 0
				? snapshot
				: {...snapshot, subscriptionCommands: EMPTY_SUBSCRIPTION_COMMANDS};
		case 'subscription.actualChanged':
			return transitionSubscriptionActualChanged(snapshot, event);
		case 'subscription.commandFailed':
			return transitionSubscriptionCommandFailed(snapshot, event);
		case 'subscription.reconcile':
			return transitionSubscriptionReconcile(snapshot);
	}
}

function transitionFailureReported<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	failure: TFailure,
	generation: number | undefined,
): VoiceMediaGraphSnapshot<TFailure> {
	if (isStaleGenerationEvent(snapshot, failure.streamKey, generation)) return snapshot;
	const currentGeneration = failure.streamKey
		? (snapshot.watchGenerationByStreamKey.get(failure.streamKey) ?? 0)
		: undefined;
	const recordedGeneration = generation ?? failure.generation ?? currentGeneration;
	const recorded =
		recordedGeneration === undefined || recordedGeneration === failure.generation
			? failure
			: {...failure, generation: recordedGeneration};
	return {
		...snapshot,
		failuresByKey: mapSetBounded(snapshot.failuresByKey, voiceMediaGraphFailureKey(recorded), recorded),
	};
}

function voiceMediaGraphWatchAttemptDeadline(
	streamKey: string,
	attemptKey: string,
	generation: number,
	startedAt: number,
): VoiceMediaGraphDeadline {
	return {
		kind: 'watchAttempt',
		streamKey,
		subscriptionKey: null,
		generation,
		attemptKey,
		dueAt: startedAt + WATCH_ATTEMPT_TIMEOUT_MS,
	};
}

function ensureVoiceMediaGraphWatchAttempt<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: {streamKey: string; attemptKey: string; startedAt: number; generation?: number},
): VoiceMediaGraphSnapshot<TFailure> {
	if (isStaleGenerationEvent(snapshot, event.streamKey, event.generation)) return snapshot;
	const deadlineKey = voiceMediaGraphWatchAttemptDeadlineKey(event.streamKey);
	const existing = snapshot.attemptsByStreamKey.get(event.streamKey);
	if (existing?.attemptKey === event.attemptKey) {
		const attemptsByStreamKey = mapSetBounded(snapshot.attemptsByStreamKey, event.streamKey, {
			...existing,
			holders: existing.holders + 1,
		});
		if (existing.hasRenderedVideoFrame) return {...snapshot, attemptsByStreamKey};
		if (snapshot.deadlinesByKey.has(deadlineKey)) return {...snapshot, attemptsByStreamKey};
		const deadlinesByKey = mapSetBounded(
			snapshot.deadlinesByKey,
			deadlineKey,
			voiceMediaGraphWatchAttemptDeadline(event.streamKey, event.attemptKey, existing.generation, existing.startedAt),
		);
		return {...snapshot, attemptsByStreamKey, deadlinesByKey};
	}
	const generation = event.generation ?? snapshot.watchGenerationByStreamKey.get(event.streamKey) ?? 0;
	const attempt: VoiceMediaGraphWatchAttempt = {
		attemptKey: event.attemptKey,
		startedAt: event.startedAt,
		hasRenderedVideoFrame: false,
		generation,
		holders: 1,
	};
	const deadlinesByKey = mapSetBounded(
		snapshot.deadlinesByKey,
		deadlineKey,
		voiceMediaGraphWatchAttemptDeadline(event.streamKey, event.attemptKey, generation, event.startedAt),
	);
	return {
		...snapshot,
		attemptsByStreamKey: mapSetBounded(snapshot.attemptsByStreamKey, event.streamKey, attempt),
		deadlinesByKey,
	};
}

function releaseVoiceMediaGraphWatchAttempt<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: {streamKey: string; attemptKey: string},
): VoiceMediaGraphSnapshot<TFailure> {
	if (!event.streamKey) return snapshot;
	const existing = snapshot.attemptsByStreamKey.get(event.streamKey);
	if (!existing || existing.attemptKey !== event.attemptKey) return snapshot;
	const holders = Math.max(existing.holders - 1, 0);
	const attemptsByStreamKey = mapSetBounded(snapshot.attemptsByStreamKey, event.streamKey, {...existing, holders});
	if (holders > 0) return {...snapshot, attemptsByStreamKey};
	const deadlineKey = voiceMediaGraphWatchAttemptDeadlineKey(event.streamKey);
	const deadline = snapshot.deadlinesByKey.get(deadlineKey);
	if (!deadline || deadline.attemptKey !== event.attemptKey) return {...snapshot, attemptsByStreamKey};
	return {...snapshot, attemptsByStreamKey, deadlinesByKey: mapDelete(snapshot.deadlinesByKey, deadlineKey)};
}

function markRenderedFrameOnSubscriptionEntry<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	streamKey: string,
	renderedAt: number,
): VoiceMediaGraphSnapshot<TFailure> {
	const entry = findScreenShareEntryForStreamKey(snapshot, streamKey);
	if (!entry) return snapshot;
	if (entry.firstFrame.renderedAt !== null && entry.actual.lastError === null) return snapshot;
	const nextEntry = buildVoiceMediaGraphSubscriptionEntry(entry, {
		...subscriptionEntryParts(entry),
		actual: entry.actual.lastError === null ? entry.actual : {...entry.actual, lastError: null},
		firstFrame: entry.firstFrame.renderedAt !== null ? entry.firstFrame : {renderedAt},
	});
	return setVoiceMediaGraphSubscriptionEntry(snapshot, nextEntry);
}

function markVoiceMediaGraphRenderedFrame<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	event: {streamKey: string; attemptKey: string; renderedAt: number; generation?: number},
): VoiceMediaGraphSnapshot<TFailure> {
	if (isStaleGenerationEvent(snapshot, event.streamKey, event.generation)) return snapshot;
	const existing = snapshot.attemptsByStreamKey.get(event.streamKey);
	if (existing && existing.attemptKey !== event.attemptKey) return snapshot;
	const attempt: VoiceMediaGraphWatchAttempt = {
		attemptKey: event.attemptKey,
		startedAt: existing?.startedAt ?? event.renderedAt,
		hasRenderedVideoFrame: true,
		generation: existing?.generation ?? snapshot.watchGenerationByStreamKey.get(event.streamKey) ?? 0,
		holders: existing?.holders ?? 0,
	};
	const next: VoiceMediaGraphSnapshot<TFailure> = {
		...snapshot,
		failuresByKey: clearFailuresMatching(snapshot.failuresByKey, (failure) =>
			voiceMediaGraphFailureMatchesWatchStart(failure, event.streamKey),
		),
		attemptsByStreamKey: mapSetBounded(snapshot.attemptsByStreamKey, event.streamKey, attempt),
		deadlinesByKey: mapDelete(snapshot.deadlinesByKey, voiceMediaGraphWatchAttemptDeadlineKey(event.streamKey)),
	};
	return markRenderedFrameOnSubscriptionEntry(next, event.streamKey, event.renderedAt);
}

function clearVoiceMediaGraphFailures<TFailure extends VoiceMediaGraphFailure>(
	failures: ReadonlyMap<string, TFailure>,
	target: VoiceMediaGraphFailureTarget,
): ReadonlyMap<string, TFailure> {
	return clearFailuresMatching(failures, (failure) => {
		if (target.streamKey && failure.streamKey !== target.streamKey) return false;
		if (target.trackSid && failure.trackSid !== target.trackSid) return false;
		if (target.participantIdentity && failure.participantIdentity !== target.participantIdentity) return false;
		if (target.participantSid && failure.participantSid !== target.participantSid) return false;
		return voiceMediaGraphFailureMatchesTarget(failure, target);
	});
}

export function selectVoiceMediaGraphFailure<TFailure extends VoiceMediaGraphFailure>(
	snapshot: VoiceMediaGraphSnapshot<TFailure>,
	target: VoiceMediaGraphFailureTarget,
): TFailure | null {
	let visited = 0;
	for (const failure of snapshot.failuresByKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'failure selector exceeded graph limit');
		if (
			target.streamKey &&
			failure.streamKey === target.streamKey &&
			voiceMediaGraphFailureMatchesTarget(failure, target)
		)
			return failure;
		if (target.trackSid && failure.trackSid === target.trackSid && voiceMediaGraphFailureMatchesTarget(failure, target))
			return failure;
		if (
			target.participantIdentity &&
			failure.participantIdentity === target.participantIdentity &&
			voiceMediaGraphFailureMatchesTarget(failure, target)
		)
			return failure;
		if (
			target.participantSid &&
			failure.participantSid === target.participantSid &&
			voiceMediaGraphFailureMatchesTarget(failure, target)
		)
			return failure;
	}
	return null;
}

export function selectVoiceMediaGraphHasFailureForStreamKey(
	snapshot: VoiceMediaGraphSnapshot,
	streamKey: string | null | undefined,
): boolean {
	if (!streamKey) return false;
	return selectVoiceMediaGraphFailure(snapshot, {streamKey}) !== null;
}

export function selectVoiceMediaGraphWatchGeneration(snapshot: VoiceMediaGraphSnapshot, streamKey: string): number {
	if (!streamKey) return 0;
	return snapshot.watchGenerationByStreamKey.get(streamKey) ?? 0;
}

export function selectVoiceMediaGraphAttempt(
	snapshot: VoiceMediaGraphSnapshot,
	streamKey: string,
): VoiceMediaGraphWatchAttempt | null {
	if (!streamKey) return null;
	return snapshot.attemptsByStreamKey.get(streamKey) ?? null;
}

export function selectVoiceMediaGraphWatchStopReason(
	snapshot: VoiceMediaGraphSnapshot,
	streamKey: string,
): VoiceMediaGraphWatchStopReason | null {
	if (!streamKey) return null;
	return snapshot.watchStopReasonByStreamKey.get(streamKey) ?? null;
}

export function selectVoiceMediaGraphViewerStreamKeys(snapshot: VoiceMediaGraphSnapshot): ReadonlyArray<string> {
	return snapshot.watchIntent.viewerStreamKeys;
}

export function selectVoiceMediaGraphDeferredStopKeys(snapshot: VoiceMediaGraphSnapshot): ReadonlySet<string> {
	return snapshot.watchIntent.deferredStopKeys;
}

export function getVoiceMediaGraphWatchIntentStateValue(snapshot: VoiceMediaGraphSnapshot): 'idle' | 'watching' {
	return snapshot.watchIntent.viewerStreamKeys.length > 0 ? 'watching' : 'idle';
}

export function transitionVoiceMediaGraphViewerStreamKeys(
	viewerStreamKeys: ReadonlyArray<string>,
	event: VoiceMediaGraphWatchIntentEvent,
): Array<string> {
	const snapshot = transitionVoiceMediaGraph(
		{
			...createVoiceMediaGraphSnapshot(),
			watchIntent: {
				viewerStreamKeys: normalizeVoiceMediaGraphViewerStreamKeys(viewerStreamKeys),
				deferredStopKeys: EMPTY_STRING_SET,
			},
		},
		event,
	);
	return [...snapshot.watchIntent.viewerStreamKeys];
}

export function selectVoiceMediaGraphSubscriptionEntry(
	snapshot: VoiceMediaGraphSnapshot,
	participantIdentity: string,
	source: VoiceTrackSource,
): VoiceMediaGraphSubscriptionEntry | null {
	if (!participantIdentity) return null;
	return snapshot.subscriptionsByKey.get(voiceMediaGraphParticipantSourceKey(participantIdentity, source)) ?? null;
}

export function selectVoiceMediaGraphSubscriptionCommands(
	snapshot: VoiceMediaGraphSnapshot,
): ReadonlyArray<VoiceMediaGraphSubscriptionCommand> {
	return snapshot.subscriptionCommands;
}

export function selectVoiceMediaGraphDeadline(
	snapshot: VoiceMediaGraphSnapshot,
	key: string,
): VoiceMediaGraphDeadline | null {
	if (!key) return null;
	return snapshot.deadlinesByKey.get(key) ?? null;
}

export function selectVoiceMediaGraphDeadlines(
	snapshot: VoiceMediaGraphSnapshot,
): ReadonlyMap<string, VoiceMediaGraphDeadline> {
	return snapshot.deadlinesByKey;
}

export function selectVoiceMediaGraphStatsEntry(
	snapshot: VoiceMediaGraphSnapshot,
	target: VoiceMediaGraphStatsTrackTarget,
): VoiceMediaGraphStatsEntry | null {
	let visited = 0;
	for (const entry of snapshot.statsByTrackKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'stats selector exceeded graph limit');
		if (snapshot.statsConnectionId !== null && entry.connectionId !== snapshot.statsConnectionId) continue;
		if (voiceMediaGraphStatsObservationMatchesTarget(entry.observation, target)) return entry;
	}
	return null;
}

export function selectVoiceMediaGraphStatsTrackInfo(
	snapshot: VoiceMediaGraphSnapshot,
	target: VoiceMediaGraphStatsTrackTarget,
): VoiceMediaGraphPartialTrackInfo | null {
	const entry = selectVoiceMediaGraphStatsEntry(snapshot, target);
	if (!entry) return null;
	const observation = entry.observation;
	const width = observation.width ?? observation.sourceWidth;
	const height = observation.height ?? observation.sourceHeight;
	const fps = observation.fps ?? observation.sourceFps;
	const info: VoiceMediaGraphPartialTrackInfo = {};
	if (width !== null && width > 0 && height !== null && height > 0) {
		info.width = width;
		info.height = height;
	}
	if (fps !== null && fps > 0) info.fps = fps;
	if (info.width === undefined && info.fps === undefined) return null;
	return info;
}
