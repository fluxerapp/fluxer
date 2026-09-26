// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {appendTransition, commandIfConnected} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2Track,
	VoiceEngineV2WatchedStream,
	VoiceEngineV2WatchedStreamKey,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2RemoteTrackSubscriptionEvent = Extract<
	VoiceEngineV2Event,
	{
		type: `remoteTrackSubscription.${string}` | `watchedStream.${string}` | `watchedStreams.${string}`;
	}
>;

function sameRemoteTrackSubscriptionOptions(
	a: VoiceEngineV2RemoteTrackSubscriptionOptions,
	b: VoiceEngineV2RemoteTrackSubscriptionOptions,
): boolean {
	assert.ok(a != null, 'sameRemoteTrackSubscriptionOptions a must not be null');
	assert.ok(b != null, 'sameRemoteTrackSubscriptionOptions b must not be null');
	return (
		a.participantIdentity === b.participantIdentity &&
		a.source === b.source &&
		a.subscribed === b.subscribed &&
		a.enabled === b.enabled &&
		a.quality === b.quality
	);
}

function subscriptionKey(options: VoiceEngineV2RemoteTrackSubscriptionOptions): string {
	assert.ok(options != null, 'subscriptionKey options must not be null');
	assert.equal(typeof options.participantIdentity, 'string', 'options.participantIdentity must be a string');
	return `${options.participantIdentity}:${options.source}`;
}

function watchedStreamKey(stream: VoiceEngineV2WatchedStreamKey): string {
	assert.ok(stream != null, 'watchedStreamKey stream must not be null');
	assert.equal(typeof stream.participantIdentity, 'string', 'stream.participantIdentity must be a string');
	return `${stream.participantIdentity}:${stream.source}`;
}

function findMatchingPublishedTrack(
	snapshot: VoiceEngineV2Snapshot,
	stream: VoiceEngineV2WatchedStreamKey,
): VoiceEngineV2Track | null {
	assert.ok(snapshot != null, 'findMatchingPublishedTrack snapshot must not be null');
	assert.ok(stream != null, 'findMatchingPublishedTrack stream must not be null');
	return (
		Object.values(snapshot.room.tracks).find(
			(track) => track.participantIdentity === stream.participantIdentity && track.source === stream.source,
		) ?? null
	);
}

function watchedStreamToSubscriptionOptions(
	stream: VoiceEngineV2WatchedStream,
): VoiceEngineV2RemoteTrackSubscriptionOptions {
	assert.ok(stream != null, 'watchedStreamToSubscriptionOptions stream must not be null');
	assert.equal(typeof stream.participantIdentity, 'string', 'stream.participantIdentity must be a string');
	return {
		participantIdentity: stream.participantIdentity,
		source: stream.source,
		subscribed: stream.enabled,
		enabled: stream.enabled,
		...(stream.quality ? {quality: stream.quality} : {}),
	};
}

function applyRemoteTrackSubscriptionRequest(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2RemoteTrackSubscriptionOptions,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'applyRemoteTrackSubscriptionRequest snapshot must not be null');
	assert.ok(options != null, 'applyRemoteTrackSubscriptionRequest options must not be null');
	const current = snapshot.remoteTrackSubscriptions[subscriptionKey(options)];
	if (current && sameRemoteTrackSubscriptionOptions(current, options)) return {snapshot, commands: []};
	const base = {
		...snapshot,
		remoteTrackSubscriptions: {
			...snapshot.remoteTrackSubscriptions,
			[subscriptionKey(options)]: options,
		},
	};
	return commandIfConnected(base, 'remoteTrackSubscription', {
		type: 'remoteTrackSubscription.set',
		options,
	});
}

function applyWatchedStream(
	snapshot: VoiceEngineV2Snapshot,
	stream: VoiceEngineV2WatchedStream,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'applyWatchedStream snapshot must not be null');
	assert.ok(stream != null, 'applyWatchedStream stream must not be null');
	const existingTrack = stream.trackSid ? null : findMatchingPublishedTrack(snapshot, stream);
	const desired: VoiceEngineV2WatchedStream = {
		...stream,
		trackSid: stream.trackSid ?? existingTrack?.trackSid ?? null,
	};
	const base = {
		...snapshot,
		watchedStreams: {
			...snapshot.watchedStreams,
			[watchedStreamKey(desired)]: desired,
		},
	};
	return applyRemoteTrackSubscriptionRequest(base, watchedStreamToSubscriptionOptions(desired));
}

function applyUnwatchedStream(
	snapshot: VoiceEngineV2Snapshot,
	stream: VoiceEngineV2WatchedStreamKey,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'applyUnwatchedStream snapshot must not be null');
	assert.ok(stream != null, 'applyUnwatchedStream stream must not be null');
	const key = watchedStreamKey(stream);
	const previous = snapshot.watchedStreams[key];
	const watchedStreams = {...snapshot.watchedStreams};
	delete watchedStreams[key];
	const base = {...snapshot, watchedStreams};
	return applyRemoteTrackSubscriptionRequest(base, {
		participantIdentity: stream.participantIdentity,
		source: stream.source,
		subscribed: false,
		enabled: false,
		...(previous?.quality ? {quality: previous.quality} : {}),
	});
}

function replaceWatchedStreams(
	snapshot: VoiceEngineV2Snapshot,
	streams: Array<VoiceEngineV2WatchedStream>,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'replaceWatchedStreams snapshot must not be null');
	assert.ok(Array.isArray(streams), 'replaceWatchedStreams streams must be an array');
	const desiredKeys = new Set(streams.map(watchedStreamKey));
	let transition: VoiceEngineV2Transition = {snapshot, commands: []};
	for (const key of Object.keys(snapshot.watchedStreams).sort()) {
		if (desiredKeys.has(key)) continue;
		const previous = snapshot.watchedStreams[key];
		if (!previous) continue;
		transition = appendTransition(
			transition,
			applyUnwatchedStream(transition.snapshot, {
				participantIdentity: previous.participantIdentity,
				source: previous.source,
			}),
		);
	}
	for (const stream of [...streams].sort((a, b) => watchedStreamKey(a).localeCompare(watchedStreamKey(b)))) {
		transition = appendTransition(transition, applyWatchedStream(transition.snapshot, stream));
	}
	return transition;
}

export function syncWatchedStreamTrack(
	snapshot: VoiceEngineV2Snapshot,
	track: VoiceEngineV2Track,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'syncWatchedStreamTrack snapshot must not be null');
	assert.ok(track != null, 'syncWatchedStreamTrack track must not be null');
	const key = watchedStreamKey({participantIdentity: track.participantIdentity, source: track.source});
	const stream = snapshot.watchedStreams[key];
	if (!stream || stream.trackSid === track.trackSid) return snapshot;
	return {
		...snapshot,
		watchedStreams: {
			...snapshot.watchedStreams,
			[key]: {...stream, trackSid: track.trackSid},
		},
	};
}

export function clearWatchedStreamTrack(snapshot: VoiceEngineV2Snapshot, trackSid: string): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'clearWatchedStreamTrack snapshot must not be null');
	assert.equal(typeof trackSid, 'string', 'clearWatchedStreamTrack trackSid must be a string');
	let changed = false;
	const watchedStreams: Record<string, VoiceEngineV2WatchedStream> = {};
	for (const key of Object.keys(snapshot.watchedStreams).sort()) {
		const stream = snapshot.watchedStreams[key];
		if (!stream) continue;
		if (stream.trackSid === trackSid) {
			changed = true;
			watchedStreams[key] = {...stream, trackSid: null};
		} else {
			watchedStreams[key] = stream;
		}
	}
	return changed ? {...snapshot, watchedStreams} : snapshot;
}

export function transitionRemoteTrackSubscription(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2RemoteTrackSubscriptionEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionRemoteTrackSubscription snapshot must not be null');
	assert.ok(event != null, 'transitionRemoteTrackSubscription event must not be null');
	assert.equal(typeof event.type, 'string', 'remoteTrackSubscription event type must be a string');
	assert.ok(
		event.type.startsWith('remoteTrackSubscription.') ||
			event.type.startsWith('watchedStream.') ||
			event.type.startsWith('watchedStreams.'),
		'remoteTrackSubscription reducer received unrelated event',
	);
	switch (event.type) {
		case 'remoteTrackSubscription.setRequested':
			return applyRemoteTrackSubscriptionRequest(snapshot, event.options);
		case 'remoteTrackSubscription.setSucceeded':
			return {snapshot, commands: []};
		case 'remoteTrackSubscription.setFailed':
			return {snapshot: {...snapshot, lastFailure: event.error}, commands: []};
		case 'watchedStream.watchRequested':
			return applyWatchedStream(snapshot, event.stream);
		case 'watchedStream.unwatchRequested':
			return applyUnwatchedStream(snapshot, event.stream);
		case 'watchedStreams.replaced':
			return replaceWatchedStreams(snapshot, event.streams);
	}
}
