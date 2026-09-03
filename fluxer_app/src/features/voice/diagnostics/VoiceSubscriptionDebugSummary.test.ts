// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	summarizeVoiceSubscriptionDebug,
	type VoiceSubscriptionDebugParticipant,
	type VoiceSubscriptionDebugPublication,
	type VoiceSubscriptionDebugSubscription,
	type VoiceSubscriptionDebugTransceiver,
} from '@app/features/voice/diagnostics/VoiceSubscriptionDebugSummary';
import {describe, expect, it} from 'vitest';

function transceiver(overrides: Partial<VoiceSubscriptionDebugTransceiver>): VoiceSubscriptionDebugTransceiver {
	return {
		mid: '0',
		direction: 'recvonly',
		currentDirection: 'recvonly',
		receiver: {id: 'receiver', kind: 'video', enabled: true, muted: false, readyState: 'live'},
		sender: null,
		...overrides,
	};
}

function publication(overrides: Partial<VoiceSubscriptionDebugPublication>): VoiceSubscriptionDebugPublication {
	return {
		trackSid: 'TR_1',
		trackName: 'screen',
		kind: 'video',
		source: 'screen_share',
		mimeType: 'video/VP9',
		dimensions: {width: 1920, height: 1080},
		simulcasted: false,
		muted: false,
		enabled: true,
		subscribed: false,
		desired: true,
		subscriptionStatus: 'desired',
		permissionStatus: 'allowed',
		videoQuality: 2,
		track: null,
		...overrides,
	};
}

function participant(publications: Array<VoiceSubscriptionDebugPublication>): VoiceSubscriptionDebugParticipant {
	return {identity: 'user_1_connection-a', sid: 'PA_1', publications};
}

function subscription(overrides: Partial<VoiceSubscriptionDebugSubscription>): VoiceSubscriptionDebugSubscription {
	return {
		key: 'user_1_connection-a:screen_share',
		participantIdentity: 'user_1_connection-a',
		source: 'screen_share',
		desired: {enabled: true, quality: 'high', context: 'focused', isIntersecting: true, hasObservedElement: false},
		actual: {subscribed: null, enabled: null, quality: null, lastCommandAt: null, lastError: null},
		publication: {available: true, trackSid: 'TR_1', observedAt: 1000},
		firstFrame: {renderedAt: null},
		subscribed: true,
		...overrides,
	};
}

describe('summarizeVoiceSubscriptionDebug', () => {
	it('reports zero issued commands when the client never asked for the stream', () => {
		const summary = summarizeVoiceSubscriptionDebug([], [participant([publication({})])], [subscription({})], 0);
		expect(summary.remoteScreenSharePublications).toBe(1);
		expect(summary.screenShareSubscriptions).toBe(1);
		expect(summary.screenShareSubscriptionsWithCommandIssued).toBe(0);
		expect(summary.videoReceiveTransceivers).toBe(0);
		expect(summary.negotiatedVideoReceiveTransceivers).toBe(0);
		expect(summary.screenSharePublicationsWithTrack).toBe(0);
	});

	it('counts a negotiated receive transceiver once the answer applies it', () => {
		const summary = summarizeVoiceSubscriptionDebug(
			[transceiver({}), transceiver({mid: '1', currentDirection: null})],
			[participant([publication({subscribed: true, track: null})])],
			[subscription({actual: {subscribed: true, enabled: true, quality: 'high', lastCommandAt: 5, lastError: null}})],
			2,
		);
		expect(summary.videoReceiveTransceivers).toBe(2);
		expect(summary.negotiatedVideoReceiveTransceivers).toBe(1);
		expect(summary.subscribedScreenSharePublications).toBe(1);
		expect(summary.screenShareSubscriptionsWithCommandIssued).toBe(1);
		expect(summary.pendingSubscriptionCommands).toBe(2);
	});

	it('ignores send-only video, audio receivers, and non-screen-share sources', () => {
		const summary = summarizeVoiceSubscriptionDebug(
			[
				transceiver({direction: 'sendonly', currentDirection: 'sendonly'}),
				transceiver({
					mid: '1',
					receiver: {id: 'audio', kind: 'audio', enabled: true, muted: false, readyState: 'live'},
				}),
			],
			[
				participant([
					publication({source: 'camera'}),
					publication({trackSid: 'TR_2', kind: 'audio', source: 'screen_share_audio'}),
				]),
			],
			[subscription({source: 'camera'})],
			0,
		);
		expect(summary.videoReceiveTransceivers).toBe(0);
		expect(summary.negotiatedVideoReceiveTransceivers).toBe(0);
		expect(summary.remoteScreenSharePublications).toBe(0);
		expect(summary.screenShareSubscriptions).toBe(0);
	});

	it('counts a rendered first frame separately from an attached track', () => {
		const summary = summarizeVoiceSubscriptionDebug(
			[transceiver({})],
			[
				participant([
					publication({
						subscribed: true,
						track: {id: 'track', kind: 'video', enabled: true, muted: false, readyState: 'live'},
					}),
				]),
			],
			[subscription({firstFrame: {renderedAt: 42}})],
			0,
		);
		expect(summary.screenSharePublicationsWithTrack).toBe(1);
		expect(summary.screenShareSubscriptionsWithFirstFrame).toBe(1);
	});
});
