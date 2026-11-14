// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {removeInboundVideoTrack} from '@fluxer/voice_engine_v2/src/core/reducers/inboundVideo';
import {
	clearWatchedStreamTrack,
	syncWatchedStreamTrack,
} from '@fluxer/voice_engine_v2/src/core/reducers/remoteTrackSubscription';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {VoiceEngineV2Participant, VoiceEngineV2Track} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2RoomEvent = Extract<VoiceEngineV2Event, {type: `room.${string}`}>;

function participantMatches(participant: VoiceEngineV2Participant, identity?: string, sid?: string): boolean {
	assert.ok(participant != null, 'participantMatches participant must not be null');
	assert.equal(typeof participant.identity, 'string', 'participant.identity must be a string');
	return (
		(identity !== undefined && participant.identity === identity) || (sid !== undefined && participant.sid === sid)
	);
}

function trackMatchesParticipant(track: VoiceEngineV2Track, identities: Set<string>, sid?: string): boolean {
	assert.ok(track != null, 'trackMatchesParticipant track must not be null');
	assert.ok(identities instanceof Set, 'trackMatchesParticipant identities must be a Set');
	return identities.has(track.participantIdentity) || (sid !== undefined && track.participantSid === sid);
}

function removeParticipantRoomState(
	snapshot: VoiceEngineV2Snapshot,
	participantIdentity?: string,
	participantSid?: string,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'removeParticipantRoomState snapshot must not be null');
	assert.ok(snapshot.room != null, 'removeParticipantRoomState snapshot.room must not be null');
	const identities = new Set<string>();
	if (participantIdentity) identities.add(participantIdentity);
	const participants = {...snapshot.room.participants};
	for (const identity of Object.keys(snapshot.room.participants).sort()) {
		const participant = snapshot.room.participants[identity];
		if (!participant) continue;
		if (!participantMatches(participant, participantIdentity, participantSid)) continue;
		identities.add(identity);
		delete participants[identity];
	}
	const tracks: Record<string, VoiceEngineV2Track> = {};
	const inboundTracks = {...snapshot.inboundVideo.tracks};
	for (const trackSid of Object.keys(snapshot.room.tracks).sort()) {
		const track = snapshot.room.tracks[trackSid];
		if (!track) continue;
		if (trackMatchesParticipant(track, identities, participantSid)) {
			delete inboundTracks[trackSid];
			continue;
		}
		tracks[trackSid] = track;
	}
	for (const trackSid of Object.keys(snapshot.inboundVideo.tracks).sort()) {
		const track = snapshot.inboundVideo.tracks[trackSid];
		if (!track) continue;
		if (
			(participantSid && track.participantSid === participantSid) ||
			(track.participantIdentity && identities.has(track.participantIdentity))
		) {
			delete inboundTracks[trackSid];
		}
	}
	return {
		...snapshot,
		room: {...snapshot.room, participants, tracks},
		inboundVideo: {...snapshot.inboundVideo, tracks: inboundTracks},
	};
}

function onParticipantJoined(
	snapshot: VoiceEngineV2Snapshot,
	participant: VoiceEngineV2Participant,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onParticipantJoined snapshot must not be null');
	assert.ok(participant != null, 'onParticipantJoined participant must not be null');
	assert.equal(typeof participant.identity, 'string', 'participant.identity must be a string');
	return {
		snapshot: {
			...snapshot,
			room: {
				...snapshot.room,
				participants: {
					...snapshot.room.participants,
					[participant.identity]: participant,
				},
			},
		},
		commands: [],
	};
}

function onTrackPublished(snapshot: VoiceEngineV2Snapshot, track: VoiceEngineV2Track): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onTrackPublished snapshot must not be null');
	assert.ok(track != null, 'onTrackPublished track must not be null');
	assert.equal(typeof track.trackSid, 'string', 'track.trackSid must be a string');
	return {
		snapshot: syncWatchedStreamTrack(
			{
				...snapshot,
				room: {
					...snapshot.room,
					tracks: {
						...snapshot.room.tracks,
						[track.trackSid]: track,
					},
				},
			},
			track,
		),
		commands: [],
	};
}

function onTrackUnpublished(snapshot: VoiceEngineV2Snapshot, trackSid: string): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onTrackUnpublished snapshot must not be null');
	assert.equal(typeof trackSid, 'string', 'onTrackUnpublished trackSid must be a string');
	const tracks = {...snapshot.room.tracks};
	delete tracks[trackSid];
	return {
		snapshot: removeInboundVideoTrack(
			clearWatchedStreamTrack({...snapshot, room: {...snapshot.room, tracks}}, trackSid),
			trackSid,
		),
		commands: [],
	};
}

function onTrackMuteChanged(
	snapshot: VoiceEngineV2Snapshot,
	trackSid: string,
	muted: boolean,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onTrackMuteChanged snapshot must not be null');
	assert.equal(typeof trackSid, 'string', 'onTrackMuteChanged trackSid must be a string');
	assert.equal(typeof muted, 'boolean', 'onTrackMuteChanged muted must be a boolean');
	const track = snapshot.room.tracks[trackSid];
	if (!track) return {snapshot, commands: []};
	return {
		snapshot: {
			...snapshot,
			room: {
				...snapshot.room,
				tracks: {
					...snapshot.room.tracks,
					[trackSid]: {...track, muted},
				},
			},
		},
		commands: [],
	};
}

export function transitionRoom(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2RoomEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionRoom snapshot must not be null');
	assert.ok(event != null, 'transitionRoom event must not be null');
	assert.equal(typeof event.type, 'string', 'room event type must be a string');
	assert.ok(event.type.startsWith('room.'), 'room reducer received unrelated event');
	switch (event.type) {
		case 'room.participantJoined':
			return onParticipantJoined(snapshot, event.participant);
		case 'room.participantLeft':
			return {
				snapshot: removeParticipantRoomState(snapshot, event.participantIdentity, event.participantSid),
				commands: [],
			};
		case 'room.trackPublished':
			return onTrackPublished(snapshot, event.track);
		case 'room.trackUnpublished':
			return onTrackUnpublished(snapshot, event.trackSid);
		case 'room.trackMuted':
			return onTrackMuteChanged(snapshot, event.trackSid, true);
		case 'room.trackUnmuted':
			return onTrackMuteChanged(snapshot, event.trackSid, false);
	}
}
