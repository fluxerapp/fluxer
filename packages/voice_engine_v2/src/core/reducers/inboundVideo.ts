// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2InboundVideoFrame,
	VoiceEngineV2InboundVideoTrackSubscription,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2InboundVideoEvent = Extract<VoiceEngineV2Event, {type: `inboundVideo.${string}`}>;

export function removeInboundVideoTrack(snapshot: VoiceEngineV2Snapshot, trackSid: string): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'removeInboundVideoTrack snapshot must not be null');
	assert.equal(typeof trackSid, 'string', 'removeInboundVideoTrack trackSid must be a string');
	if (!snapshot.inboundVideo.tracks[trackSid]) return snapshot;
	const tracks = {...snapshot.inboundVideo.tracks};
	delete tracks[trackSid];
	return {
		...snapshot,
		inboundVideo: {
			...snapshot.inboundVideo,
			tracks,
		},
	};
}

function upsertInboundVideoTrack(
	snapshot: VoiceEngineV2Snapshot,
	track: VoiceEngineV2InboundVideoTrackSubscription,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'upsertInboundVideoTrack snapshot must not be null');
	assert.ok(track != null, 'upsertInboundVideoTrack track must not be null');
	assert.equal(typeof track.trackSid, 'string', 'track.trackSid must be a string');
	const existing = snapshot.inboundVideo.tracks[track.trackSid];
	return {
		...snapshot,
		inboundVideo: {
			...snapshot.inboundVideo,
			tracks: {
				...snapshot.inboundVideo.tracks,
				[track.trackSid]: {
					participantSid: track.participantSid,
					...(track.participantIdentity ? {participantIdentity: track.participantIdentity} : {}),
					trackSid: track.trackSid,
					source: track.source,
					width: track.width ?? existing?.width ?? 0,
					height: track.height ?? existing?.height ?? 0,
					frameCount: existing?.frameCount ?? 0,
					lastFrameTimestampUs: existing?.lastFrameTimestampUs ?? null,
					lastFrameByteLength: existing?.lastFrameByteLength ?? null,
				},
			},
		},
	};
}

function applyInboundVideoFrame(
	snapshot: VoiceEngineV2Snapshot,
	frame: VoiceEngineV2InboundVideoFrame,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'applyInboundVideoFrame snapshot must not be null');
	assert.ok(frame != null, 'applyInboundVideoFrame frame must not be null');
	assert.equal(typeof frame.trackSid, 'string', 'frame.trackSid must be a string');
	const track = snapshot.inboundVideo.tracks[frame.trackSid];
	if (!track) {
		return {
			...snapshot,
			inboundVideo: {
				...snapshot.inboundVideo,
				droppedFrameCount: snapshot.inboundVideo.droppedFrameCount + 1,
			},
		};
	}
	return {
		...snapshot,
		inboundVideo: {
			...snapshot.inboundVideo,
			tracks: {
				...snapshot.inboundVideo.tracks,
				[frame.trackSid]: {
					...track,
					participantSid: frame.participantSid,
					...(frame.participantIdentity ? {participantIdentity: frame.participantIdentity} : {}),
					width: frame.width,
					height: frame.height,
					frameCount: track.frameCount + 1,
					lastFrameTimestampUs: frame.timestampUs,
					lastFrameByteLength: frame.byteLength ?? null,
				},
			},
		},
	};
}

export function transitionInboundVideo(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2InboundVideoEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionInboundVideo snapshot must not be null');
	assert.ok(event != null, 'transitionInboundVideo event must not be null');
	assert.equal(typeof event.type, 'string', 'inboundVideo event type must be a string');
	assert.ok(event.type.startsWith('inboundVideo.'), 'inboundVideo reducer received unrelated event');
	switch (event.type) {
		case 'inboundVideo.trackSubscribed':
			return {snapshot: upsertInboundVideoTrack(snapshot, event.track), commands: []};

		case 'inboundVideo.frameReceived':
			return {snapshot: applyInboundVideoFrame(snapshot, event.frame), commands: []};
	}
}
