// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE} from '@app/features/voice/engine/VoiceMediaGraph';

export interface VoiceSubscriptionDebugMediaStreamTrack {
	id: string;
	kind: string;
	enabled: boolean;
	muted: boolean;
	readyState: string;
}

export interface VoiceSubscriptionDebugTransceiver {
	mid: string | null;
	direction: string;
	currentDirection: string | null;
	receiver: VoiceSubscriptionDebugMediaStreamTrack | null;
	sender: VoiceSubscriptionDebugMediaStreamTrack | null;
}

export interface VoiceSubscriptionDebugPublication {
	trackSid: string;
	trackName: string;
	kind: string;
	source: string;
	mimeType: string | null;
	dimensions: {width: number; height: number} | null;
	simulcasted: boolean | null;
	muted: boolean;
	enabled: boolean;
	subscribed: boolean;
	desired: boolean;
	subscriptionStatus: string;
	permissionStatus: string;
	videoQuality: number | null;
	track: VoiceSubscriptionDebugMediaStreamTrack | null;
}

export interface VoiceSubscriptionDebugParticipant {
	identity: string;
	sid: string;
	publications: Array<VoiceSubscriptionDebugPublication>;
}

export interface VoiceSubscriptionDebugSubscription {
	key: string;
	participantIdentity: string;
	source: string;
	desired: {
		enabled: boolean;
		quality: string;
		context: string;
		isIntersecting: boolean;
		hasObservedElement: boolean;
	};
	actual: {
		subscribed: boolean | null;
		enabled: boolean | null;
		quality: string | null;
		lastCommandAt: number | null;
		lastError: {code: number; reason: string; at: number} | null;
	};
	publication: {available: boolean; trackSid: string | null; observedAt: number | null};
	firstFrame: {renderedAt: number | null};
	subscribed: boolean;
}

export interface VoiceSubscriptionDebugSummary {
	videoReceiveTransceivers: number;
	negotiatedVideoReceiveTransceivers: number;
	remoteScreenSharePublications: number;
	subscribedScreenSharePublications: number;
	screenSharePublicationsWithTrack: number;
	screenShareSubscriptions: number;
	screenShareSubscriptionsWithCommandIssued: number;
	screenShareSubscriptionsWithFirstFrame: number;
	pendingSubscriptionCommands: number;
}

function isReceiveDirection(direction: string | null): boolean {
	return direction === 'recvonly' || direction === 'sendrecv';
}

export function summarizeVoiceSubscriptionDebug(
	transceivers: ReadonlyArray<VoiceSubscriptionDebugTransceiver>,
	participants: ReadonlyArray<VoiceSubscriptionDebugParticipant>,
	subscriptions: ReadonlyArray<VoiceSubscriptionDebugSubscription>,
	pendingSubscriptionCommands: number,
): VoiceSubscriptionDebugSummary {
	const videoTransceivers = transceivers.filter((transceiver) => transceiver.receiver?.kind === 'video');
	const screenSharePublications = participants
		.flatMap((participant) => participant.publications)
		.filter(
			(publication) => publication.source === VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE && publication.kind === 'video',
		);
	const screenShareSubscriptions = subscriptions.filter(
		(subscription) => subscription.source === VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
	);
	return {
		videoReceiveTransceivers: videoTransceivers.filter((transceiver) => isReceiveDirection(transceiver.direction))
			.length,
		negotiatedVideoReceiveTransceivers: videoTransceivers.filter((transceiver) =>
			isReceiveDirection(transceiver.currentDirection),
		).length,
		remoteScreenSharePublications: screenSharePublications.length,
		subscribedScreenSharePublications: screenSharePublications.filter((publication) => publication.subscribed).length,
		screenSharePublicationsWithTrack: screenSharePublications.filter((publication) => publication.track != null).length,
		screenShareSubscriptions: screenShareSubscriptions.length,
		screenShareSubscriptionsWithCommandIssued: screenShareSubscriptions.filter(
			(subscription) => subscription.actual.lastCommandAt != null,
		).length,
		screenShareSubscriptionsWithFirstFrame: screenShareSubscriptions.filter(
			(subscription) => subscription.firstFrame.renderedAt != null,
		).length,
		pendingSubscriptionCommands,
	};
}
