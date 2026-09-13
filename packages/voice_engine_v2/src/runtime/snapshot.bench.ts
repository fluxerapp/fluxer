// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {
	VoiceEngineV2Participant,
	VoiceEngineV2Track,
	VoiceEngineV2TrackSource,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {bench, describe} from 'vitest';

const PARTICIPANT_COUNT = 8;
const TRACKS_PER_PARTICIPANT = 4;
const TRACK_SOURCES: ReadonlyArray<VoiceEngineV2TrackSource | string> = [
	'microphone',
	'camera',
	'screen',
	'screenAudio',
];

function buildRealisticSnapshot(): VoiceEngineV2Snapshot {
	const base = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	const participants: Record<string, VoiceEngineV2Participant> = {};
	const tracks: Record<string, VoiceEngineV2Track> = {};
	for (let participantIndex = 0; participantIndex < PARTICIPANT_COUNT; participantIndex += 1) {
		const identity = `identity-${participantIndex}`;
		const sid = `sid-${participantIndex}`;
		participants[identity] = {sid, identity, name: `Participant ${participantIndex}`};
		for (let trackIndex = 0; trackIndex < TRACKS_PER_PARTICIPANT; trackIndex += 1) {
			const trackSid = `track-${participantIndex}-${trackIndex}`;
			tracks[trackSid] = {
				participantIdentity: identity,
				participantSid: sid,
				trackSid,
				trackName: `${TRACK_SOURCES[trackIndex] ?? 'unknown'}-${participantIndex}`,
				kind: trackIndex === 0 ? 'audio' : 'video',
				source: TRACK_SOURCES[trackIndex] ?? 'unknown',
				muted: false,
			};
		}
	}
	return {
		...base,
		connection: {
			...base.connection,
			status: 'connected',
			active: {url: 'wss://voice.example.test', token: 'token-bench'},
		},
		room: {participants, tracks},
	};
}

const realisticSnapshot = buildRealisticSnapshot();
const serializedSnapshot = JSON.stringify(realisticSnapshot);

describe('voice engine v2 snapshot serialise/deserialise baseline', () => {
	bench('JSON.stringify snapshot (8 participants × 4 tracks)', () => {
		JSON.stringify(realisticSnapshot);
	});

	bench('JSON.parse snapshot (8 participants × 4 tracks)', () => {
		JSON.parse(serializedSnapshot);
	});

	bench('JSON.stringify + JSON.parse round-trip', () => {
		JSON.parse(JSON.stringify(realisticSnapshot));
	});
});
