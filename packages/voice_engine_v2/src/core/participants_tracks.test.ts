// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import inboundVideoFrameStatsFixtureJson from '../../fixtures/event_logs/inbound_video_frame_stats.json';
import inboundVideoFramesFixtureJson from '../../fixtures/event_logs/inbound_video_frames.json';
import participantsTracksFixtureJson from '../../fixtures/event_logs/participants_tracks.json';
import subscriptionQualityFixtureJson from '../../fixtures/event_logs/subscription_quality.json';
import watchedStreamsFixtureJson from '../../fixtures/event_logs/watched_streams.json';
import type {VoiceEngineV2Command} from '../protocol/commands';
import type {VoiceEngineV2Event} from '../protocol/events';
import type {
	VoiceEngineV2InboundVideoTrack,
	VoiceEngineV2Participant,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2Track,
	VoiceEngineV2WatchedStream,
} from '../protocol/types';
import {transitionVoiceEngineV2} from './reducer';
import {selectVoiceEngineV2ParticipantProjection, selectVoiceEngineV2WatchedStreams} from './selectors';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from './state';

interface ParticipantsTracksFixtureExpected {
	participants?: Array<VoiceEngineV2Participant>;
	tracks?: Array<VoiceEngineV2Track>;
	inboundVideoTracks?: Array<VoiceEngineV2InboundVideoTrack>;
	droppedFrameCount?: number;
	watchedStreams?: Array<VoiceEngineV2WatchedStream>;
	remoteTrackSubscriptions?: Record<string, VoiceEngineV2RemoteTrackSubscriptionOptions>;
	participantVolumes?: Record<string, number>;
	commands?: Array<VoiceEngineV2Command>;
}

interface ParticipantsTracksFixture {
	name: string;
	events: Array<VoiceEngineV2Event>;
	expected: ParticipantsTracksFixtureExpected;
}

function replayFixture(fixture: ParticipantsTracksFixture): {
	snapshot: VoiceEngineV2Snapshot;
	commands: Array<VoiceEngineV2Command>;
} {
	let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	const commands: Array<VoiceEngineV2Command> = [];
	for (const event of fixture.events) {
		const transition = transitionVoiceEngineV2(snapshot, event);
		snapshot = transition.snapshot;
		commands.push(...transition.commands);
	}
	return {snapshot, commands};
}

function assertFixture(fixture: ParticipantsTracksFixture): void {
	const {snapshot, commands} = replayFixture(fixture);
	const participantProjection = selectVoiceEngineV2ParticipantProjection(snapshot);
	if (fixture.expected.participants) {
		expect(participantProjection.participants).toEqual(fixture.expected.participants);
	}
	if (fixture.expected.tracks) {
		expect(participantProjection.tracks).toEqual(fixture.expected.tracks);
	}
	if (fixture.expected.inboundVideoTracks) {
		expect(participantProjection.inboundVideoTracks).toEqual(fixture.expected.inboundVideoTracks);
	}
	if (fixture.expected.droppedFrameCount !== undefined) {
		expect(snapshot.inboundVideo.droppedFrameCount).toBe(fixture.expected.droppedFrameCount);
	}
	if (fixture.expected.watchedStreams) {
		expect(selectVoiceEngineV2WatchedStreams(snapshot)).toEqual(fixture.expected.watchedStreams);
	}
	if (fixture.expected.remoteTrackSubscriptions) {
		expect(snapshot.remoteTrackSubscriptions).toEqual(fixture.expected.remoteTrackSubscriptions);
	}
	if (fixture.expected.participantVolumes) {
		expect(snapshot.participantVolumes).toEqual(fixture.expected.participantVolumes);
	}
	if (fixture.expected.commands) {
		expect(commands).toEqual(fixture.expected.commands);
	}
}

describe('voice engine v2 participants, subscriptions, and inbound video fixtures', () => {
	for (const fixture of [
		participantsTracksFixtureJson,
		subscriptionQualityFixtureJson,
		watchedStreamsFixtureJson,
		inboundVideoFramesFixtureJson,
		inboundVideoFrameStatsFixtureJson,
	] as Array<ParticipantsTracksFixture>) {
		it(`replays ${fixture.name}`, () => {
			assertFixture(fixture);
		});
	}

	it('removes participant-owned room and inbound-video state when a participant leaves by sid', () => {
		const fixture: ParticipantsTracksFixture = {
			name: 'participant sid cleanup',
			events: [
				{type: 'room.participantJoined', participant: {sid: 'PA_alice', identity: 'alice', name: 'Alice'}},
				{
					type: 'room.trackPublished',
					track: {
						participantIdentity: 'alice',
						participantSid: 'PA_alice',
						trackSid: 'TR_screen',
						trackName: 'screen',
						kind: 'video',
						source: 'screen',
						muted: false,
					},
				},
				{
					type: 'inboundVideo.trackSubscribed',
					track: {
						participantSid: 'PA_alice',
						participantIdentity: 'alice',
						trackSid: 'TR_screen',
						source: 'screen',
					},
				},
				{type: 'room.participantLeft', participantSid: 'PA_alice'},
			],
			expected: {
				participants: [],
				tracks: [],
				inboundVideoTracks: [],
				commands: [],
			},
		};

		assertFixture(fixture);
	});

	it('projects a subscribed video track and its first frame into room and inbound-video state', () => {
		const events: Array<VoiceEngineV2Event> = [
			{
				type: 'room.trackPublished',
				track: {
					participantIdentity: 'alice',
					participantSid: 'PA_alice',
					trackSid: 'TR_screen',
					trackName: 'screen_share',
					kind: 'video',
					source: 'screen',
					muted: false,
				},
			},
			{
				type: 'inboundVideo.trackSubscribed',
				track: {
					participantSid: 'PA_alice',
					participantIdentity: 'alice',
					trackSid: 'TR_screen',
					source: 'screen',
				},
			},
			{
				type: 'inboundVideo.frameReceived',
				frame: {
					participantSid: 'PA_alice',
					trackSid: 'TR_screen',
					width: 320,
					height: 180,
					timestampUs: 100,
					byteLength: 86_400,
				},
			},
		];
		let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		for (const event of events) {
			snapshot = transitionVoiceEngineV2(snapshot, event).snapshot;
		}

		expect(snapshot.room.tracks.TR_screen).toMatchObject({
			participantIdentity: 'alice',
			participantSid: 'PA_alice',
			source: 'screen',
			kind: 'video',
		});
		expect(snapshot.inboundVideo.tracks.TR_screen).toMatchObject({
			participantSid: 'PA_alice',
			participantIdentity: 'alice',
			source: 'screen',
			width: 320,
			height: 180,
			frameCount: 1,
			lastFrameByteLength: 86_400,
		});
	});
});
