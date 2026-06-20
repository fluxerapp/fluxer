// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {formatActivityDisplay, formatActivityMemberListLine} from './formatActivityDisplay.ts';

const listening = formatActivityDisplay({
	type: 2,
	name: 'We Will Fly',
	details: 'CHRIST DILLINGER, Lil Darkie - We Will Fly',
	state: 'We Will Fly',
});

assert.equal(listening.primary, 'We Will Fly');
assert.equal(listening.secondary, 'CHRIST DILLINGER, Lil Darkie');
assert.equal(listening.headerSuffix, null);
assert.equal(listening.listeningSource, null);

const album = formatActivityDisplay({
	type: 2,
	name: 'Hybrid Theory',
	details: 'Linkin Park - In the End',
	state: 'Linkin Park',
});

assert.equal(album.primary, 'In the End');
assert.equal(album.secondary, 'Linkin Park');
assert.equal(album.headerSuffix, 'Hybrid Theory');
assert.equal(album.listeningSource, 'Hybrid Theory');

const spotifyStyle = formatActivityDisplay({
	type: 2,
	name: 'Spotify',
	details: 'Knocked Loose - Counting Worms',
	state: 'Knocked Loose',
});

assert.equal(spotifyStyle.listeningSource, 'Spotify');
assert.equal(spotifyStyle.primary, 'Counting Worms');

const spit = formatActivityDisplay({
	type: 2,
	name: 'SPIT',
	details: 'Lil Darkie - SPIT',
	state: 'YIN',
});

assert.equal(spit.listeningSource, null);
assert.equal(spit.primary, 'SPIT');
assert.equal(spit.secondary, 'Lil Darkie');

const memberListSpit = formatActivityMemberListLine({
	type: 2,
	name: 'SPIT',
	details: 'Lil Darkie - SPIT',
	state: 'YIN',
});
assert.equal(memberListSpit.text, 'SPIT — Lil Darkie');

const liveTrack = formatActivityDisplay({
	type: 2,
	name: 'Bleed It Out - Live',
	details: 'Bleed It Out - Live',
	state: 'Linkin Park',
});

assert.equal(liveTrack.primary, 'Bleed It Out - Live');
assert.equal(liveTrack.secondary, 'Linkin Park');
assert.equal(liveTrack.listeningSource, null);

const liveTrackCombined = formatActivityDisplay({
	type: 2,
	name: 'Spotify',
	details: 'Linkin Park - Bleed It Out - Live',
	state: 'Linkin Park',
});

assert.equal(liveTrackCombined.primary, 'Bleed It Out - Live');
assert.equal(liveTrackCombined.secondary, 'Linkin Park');
assert.equal(liveTrackCombined.listeningSource, 'Spotify');

const memberListListening = formatActivityMemberListLine({
	type: 2,
	name: 'We Will Fly',
	details: 'CHRIST DILLINGER, Lil Darkie - We Will Fly',
	state: 'We Will Fly',
});
assert.equal(memberListListening.kind, 'listening');
assert.equal(memberListListening.text, 'We Will Fly — CHRIST DILLINGER, Lil Darkie');

const memberListPlaying = formatActivityMemberListLine({
	type: 0,
	name: 'Balatro',
	details: 'Playing a run',
	state: 'ante 69',
});
assert.equal(memberListPlaying.kind, 'playing');
assert.equal(memberListPlaying.text, 'ante 69');

const detailsFirst = formatActivityDisplay({
	type: 0,
	status_display_type: 1,
	name: 'Tauon',
	details: 'Turned Around',
	state: 'Cicada Sirens & 1000 Eyes',
});
assert.equal(detailsFirst.primary, 'Turned Around');
assert.equal(detailsFirst.secondary, 'Cicada Sirens & 1000 Eyes');

const stateFirst = formatActivityDisplay({
	type: 0,
	status_display_type: 2,
	name: 'Balatro',
	details: 'Playing a run',
	state: 'ante 69',
});
assert.equal(stateFirst.primary, 'ante 69');
assert.equal(stateFirst.secondary, 'Playing a run');

const memberListDetailsFirst = formatActivityMemberListLine({
	type: 0,
	status_display_type: 1,
	name: 'Tauon',
	details: 'Turned Around',
	state: 'Cicada Sirens & 1000 Eyes',
});
assert.equal(memberListDetailsFirst.text, 'Turned Around');

console.log('formatActivityDisplay test passed');
