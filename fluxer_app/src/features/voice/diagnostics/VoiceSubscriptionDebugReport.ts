// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {
	summarizeVoiceSubscriptionDebug,
	type VoiceSubscriptionDebugMediaStreamTrack,
	type VoiceSubscriptionDebugParticipant,
	type VoiceSubscriptionDebugPublication,
	type VoiceSubscriptionDebugSubscription,
	type VoiceSubscriptionDebugSummary,
	type VoiceSubscriptionDebugTransceiver,
} from '@app/features/voice/diagnostics/VoiceSubscriptionDebugSummary';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {VoiceMediaGraphSubscriptionCommand} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import type {RemoteTrackPublication, Room} from 'livekit-client';

interface VoiceSubscriptionDebugFailure {
	key: string;
	code: number;
	reason: string;
	reportedAt: number;
	streamKey: string | null;
	participantIdentity: string | null;
	participantSid: string | null;
	trackSid: string | null;
	source: string | null;
	generation: number | null;
	error: string | null;
}

interface VoiceSubscriptionDebugReport {
	capturedAt: string;
	buildVersion: string;
	graphNowMs: number;
	connection: {
		connected: boolean;
		connecting: boolean;
		reconnecting: boolean;
		guildId: string | null;
		channelId: string | null;
		connectionId: string | null;
		voiceServerEndpoint: string | null;
		roomState: string | null;
		localIdentity: string | null;
	};
	summary: VoiceSubscriptionDebugSummary;
	publisherTransceivers: Array<VoiceSubscriptionDebugTransceiver>;
	subscriberTransportPresent: boolean;
	remoteParticipants: Array<VoiceSubscriptionDebugParticipant>;
	subscriptions: Array<VoiceSubscriptionDebugSubscription>;
	pendingSubscriptionCommands: Array<VoiceMediaGraphSubscriptionCommand>;
	watchIntent: {viewerStreamKeys: Array<string>; deferredStopKeys: Array<string>};
	watchAttempts: Array<{
		streamKey: string;
		attemptKey: string;
		startedAt: number;
		hasRenderedVideoFrame: boolean;
		generation: number;
	}>;
	deadlines: Array<{
		key: string;
		kind: string;
		streamKey: string | null;
		subscriptionKey: string | null;
		attemptKey: string | null;
		generation: number;
		dueAt: number;
	}>;
	failures: Array<VoiceSubscriptionDebugFailure>;
}

function describeMediaStreamTrack(
	track: MediaStreamTrack | null | undefined,
): VoiceSubscriptionDebugMediaStreamTrack | null {
	if (!track) {
		return null;
	}
	return {
		id: track.id,
		kind: track.kind,
		enabled: track.enabled,
		muted: track.muted,
		readyState: track.readyState,
	};
}

function describeTransceivers(room: Room | null): Array<VoiceSubscriptionDebugTransceiver> {
	const publisher = room?.engine?.pcManager?.publisher;
	if (!publisher) {
		return [];
	}
	return publisher.getTransceivers().map((transceiver) => ({
		mid: transceiver.mid,
		direction: transceiver.direction,
		currentDirection: transceiver.currentDirection,
		receiver: describeMediaStreamTrack(transceiver.receiver?.track),
		sender: describeMediaStreamTrack(transceiver.sender?.track),
	}));
}

function describePublication(publication: RemoteTrackPublication): VoiceSubscriptionDebugPublication {
	const dimensions = publication.dimensions;
	return {
		trackSid: publication.trackSid,
		trackName: publication.trackName,
		kind: publication.kind,
		source: publication.source,
		mimeType: publication.mimeType ?? null,
		dimensions: dimensions ? {width: dimensions.width, height: dimensions.height} : null,
		simulcasted: publication.simulcasted ?? null,
		muted: publication.isMuted,
		enabled: publication.isEnabled,
		subscribed: publication.isSubscribed,
		desired: publication.isDesired,
		subscriptionStatus: publication.subscriptionStatus,
		permissionStatus: publication.permissionStatus,
		videoQuality: publication.videoQuality ?? null,
		track: describeMediaStreamTrack(publication.track?.mediaStreamTrack),
	};
}

function describeRemoteParticipants(room: Room | null): Array<VoiceSubscriptionDebugParticipant> {
	if (!room) {
		return [];
	}
	return Array.from(room.remoteParticipants.values()).map((participant) => ({
		identity: participant.identity,
		sid: participant.sid,
		publications: Array.from(participant.trackPublications.values()).map(describePublication),
	}));
}

function describeError(error: unknown): string | null {
	if (error == null) {
		return null;
	}
	if (error instanceof Error) {
		return `${error.name}: ${error.message}`;
	}
	return String(error);
}

export function collectVoiceSubscriptionDebugReport(): VoiceSubscriptionDebugReport {
	const room = MediaEngine.room;
	const snapshot = voiceMediaGraphStore.getGraphSnapshot();
	const transceivers = describeTransceivers(room);
	const remoteParticipants = describeRemoteParticipants(room);
	const subscriptions = Array.from(snapshot.subscriptionsByKey.entries()).map(([key, entry]) => ({
		key,
		participantIdentity: entry.participantIdentity,
		source: entry.source,
		desired: {
			enabled: entry.desired.enabled,
			quality: entry.desired.quality,
			context: entry.desired.context,
			isIntersecting: entry.desired.isIntersecting,
			hasObservedElement: entry.desired.observedElement != null,
		},
		actual: {
			subscribed: entry.actual.subscribed,
			enabled: entry.actual.enabled,
			quality: entry.actual.quality,
			lastCommandAt: entry.actual.lastCommandAt,
			lastError: entry.actual.lastError,
		},
		publication: {
			available: entry.publication.available,
			trackSid: entry.publication.trackSid,
			observedAt: entry.publication.observedAt,
		},
		firstFrame: {renderedAt: entry.firstFrame.renderedAt},
		subscribed: entry.subscribed,
	}));
	const pendingSubscriptionCommands = Array.from(snapshot.subscriptionCommands);
	return {
		capturedAt: new Date().toISOString(),
		buildVersion: Config.PUBLIC_BUILD_VERSION ?? 'dev',
		graphNowMs: voiceMediaGraphStore.nowMs(),
		connection: {
			connected: MediaEngine.connected,
			connecting: MediaEngine.connecting,
			reconnecting: MediaEngine.reconnecting,
			guildId: MediaEngine.guildId,
			channelId: MediaEngine.channelId,
			connectionId: MediaEngine.connectionId,
			voiceServerEndpoint: MediaEngine.voiceServerEndpoint,
			roomState: room?.state ?? null,
			localIdentity: room?.localParticipant.identity ?? null,
		},
		summary: summarizeVoiceSubscriptionDebug(
			transceivers,
			remoteParticipants,
			subscriptions,
			pendingSubscriptionCommands.length,
		),
		publisherTransceivers: transceivers,
		subscriberTransportPresent: room?.engine?.pcManager?.subscriber != null,
		remoteParticipants,
		subscriptions,
		pendingSubscriptionCommands,
		watchIntent: {
			viewerStreamKeys: Array.from(snapshot.watchIntent.viewerStreamKeys),
			deferredStopKeys: Array.from(snapshot.watchIntent.deferredStopKeys),
		},
		watchAttempts: Array.from(snapshot.attemptsByStreamKey.entries()).map(([streamKey, attempt]) => ({
			streamKey,
			attemptKey: attempt.attemptKey,
			startedAt: attempt.startedAt,
			hasRenderedVideoFrame: attempt.hasRenderedVideoFrame,
			generation: attempt.generation,
		})),
		deadlines: Array.from(snapshot.deadlinesByKey.entries()).map(([key, deadline]) => ({
			key,
			kind: deadline.kind,
			streamKey: deadline.streamKey,
			subscriptionKey: deadline.subscriptionKey,
			attemptKey: deadline.attemptKey,
			generation: deadline.generation,
			dueAt: deadline.dueAt,
		})),
		failures: Array.from(snapshot.failuresByKey.entries()).map(([key, failure]) => ({
			key,
			code: failure.code,
			reason: failure.reason,
			reportedAt: failure.reportedAt,
			streamKey: failure.streamKey ?? null,
			participantIdentity: failure.participantIdentity ?? null,
			participantSid: failure.participantSid ?? null,
			trackSid: failure.trackSid ?? null,
			source: failure.source ?? null,
			generation: failure.generation ?? null,
			error: describeError(failure.error),
		})),
	};
}
