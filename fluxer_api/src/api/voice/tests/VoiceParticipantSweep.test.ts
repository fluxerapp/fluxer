// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	sweepVoiceParticipants,
	type VoiceParticipantSightings,
	type VoiceParticipantSweepDeps,
} from '@app/api/voice/VoiceParticipantSweep';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {describe, expect, it} from 'vitest';

class QuietLogger implements LoggerInterface {
	trace(): void {}
	debug(): void {}
	info(): void {}
	warn(): void {}
	error(): void {}
	fatal(): void {}
	child(): LoggerInterface {
		return this;
	}
}

const ROOM = 'guild_10_channel_20';
const KNOWN = 'user_1_known';
const STRAY = 'user_2_stray';

function createHarness() {
	const state = {
		now: 0,
		joinedAt: 1000,
		gatewayReads: 0,
		strayKnownFromRead: Number.POSITIVE_INFINITY,
		removed: [] as Array<string>,
	};
	const deps: VoiceParticipantSweepDeps = {
		logger: new QuietLogger(),
		now: () => state.now,
		dryRun: false,
		liveKitService: {
			listServers: () => [{regionId: 'eu', serverId: 'eu-1'}],
			listRooms: async () => ({status: 'ok', rooms: [{name: ROOM, numParticipants: 2}]}),
			listParticipants: async () => ({
				status: 'ok',
				participants: [
					{identity: KNOWN, joinedAt: 500, metadata: ''},
					{identity: STRAY, joinedAt: state.joinedAt, metadata: ''},
				],
			}),
			getParticipant: async ({identity}) => ({
				status: 'ok',
				participant: {joinedAt: identity === STRAY ? state.joinedAt : 500, metadata: ''},
			}),
			removeParticipant: async ({identity}) => {
				state.removed.push(identity);
			},
		},
		gatewayService: {
			getVoiceStatesForChannel: async () => {
				state.gatewayReads += 1;
				const known = [{connectionId: 'known', userId: '1', channelId: '20'}];
				if (state.gatewayReads >= state.strayKnownFromRead) {
					known.push({connectionId: 'stray', userId: '2', channelId: '20'});
				}
				return {voiceStates: known};
			},
		},
	};
	let sightings: VoiceParticipantSightings = {};
	const sweepAt = async (minute: number) => {
		state.now = minute * 60_000;
		const result = await sweepVoiceParticipants(deps, sightings);
		sightings = result.sightings;
		return result;
	};
	return {state, sweepAt};
}

describe('sweepVoiceParticipants', () => {
	it('disconnects a participant only after it stays unknown with the same join time for two minutes', async () => {
		const {state, sweepAt} = createHarness();
		expect((await sweepAt(0)).removed).toBe(0);
		expect((await sweepAt(1)).removed).toBe(0);
		const settled = await sweepAt(2);
		expect(settled.removed).toBe(1);
		expect(state.removed).toEqual([STRAY]);
	});

	it('starts over when the participant rejoins with a new join time', async () => {
		const {state, sweepAt} = createHarness();
		await sweepAt(0);
		await sweepAt(1);
		state.joinedAt = 2000;
		expect((await sweepAt(2)).removed).toBe(0);
		expect((await sweepAt(3)).removed).toBe(0);
		expect((await sweepAt(4)).removed).toBe(1);
	});

	it('keeps a participant the gateway knows by the time of removal', async () => {
		const {state, sweepAt} = createHarness();
		await sweepAt(0);
		state.strayKnownFromRead = state.gatewayReads + 2;
		expect((await sweepAt(2)).removed).toBe(0);
		expect(state.removed).toEqual([]);
	});
});
