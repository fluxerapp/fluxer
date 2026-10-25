// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import {describe, expect, it, vi} from 'vitest';
import {VoiceJoinError} from '../fluxer/GatewayClient';
import {VoiceStates} from '../fluxer/VoiceStates';
import type {VoiceStateWire} from '../fluxer/Wire';
import type {ChannelSession, EndReason} from './ChannelSession';
import {SessionManager} from './SessionManager';

const BOT = 'bot-1';

class FakeSession extends EventEmitter {
	connectionId: string | null = null;
	ended = false;
	start = vi.fn(async () => {
		this.connectionId = `conn-${this.channelId}`;
	});
	end = vi.fn(async (reason: EndReason) => {
		if (this.ended) return;
		this.ended = true;
		this.emit('ended', reason);
	});

	constructor(
		readonly guildId: string,
		readonly channelId: string,
	) {
		super();
	}
}

function setup() {
	const voiceStates = new VoiceStates();
	const created: Array<FakeSession> = [];
	const factory = vi.fn((guildId: string, channelId: string) => {
		const s = new FakeSession(guildId, channelId);
		created.push(s);
		return s as unknown as ChannelSession;
	});
	const bots = new Set([BOT, 'other-bot']);
	const manager = new SessionManager(factory, voiceStates, (id) => bots.has(id));
	/** Applies a voice state the way Main does: VoiceStates first, then the manager. */
	const apply = (state: VoiceStateWire) => {
		voiceStates.apply(state);
		manager.onVoiceState(state, BOT);
	};
	return {manager, voiceStates, factory, created, apply};
}

const vs = (user_id: string, channel_id: string | null, connection_id: string): VoiceStateWire => ({
	guild_id: 'g1',
	channel_id,
	user_id,
	connection_id,
});

describe('SessionManager', () => {
	it('concurrent getOrStart shares one start', async () => {
		const {manager, factory, created} = setup();
		const [a, b] = await Promise.all([manager.getOrStart('g1', 'c1'), manager.getOrStart('g1', 'c1')]);
		expect(a).toBe(b);
		expect(factory).toHaveBeenCalledTimes(1);
		expect(created[0]?.start).toHaveBeenCalledTimes(1);
		expect(manager.get('c1')).toBe(a);
		expect(await manager.getOrStart('g1', 'c1')).toBe(a);
	});

	it('failed start is not cached', async () => {
		const {manager, factory} = setup();
		factory.mockImplementationOnce((guildId, channelId) => {
			const s = new FakeSession(guildId, channelId);
			s.start.mockRejectedValue(new VoiceJoinError('P2P_GRANT'));
			return s as unknown as ChannelSession;
		});
		await expect(manager.getOrStart('g1', 'c1')).rejects.toMatchObject({code: 'P2P_GRANT'});
		expect(manager.get('c1')).toBeUndefined();
		const session = await manager.getOrStart('g1', 'c1');
		expect(manager.get('c1')).toBe(session);
		expect(factory).toHaveBeenCalledTimes(2);
	});

	it('voiceDropped ends the session', async () => {
		const {manager, created, apply} = setup();
		apply(vs('u1', 'c1', 'u1-conn'));
		await manager.getOrStart('g1', 'c1');
		apply(vs(BOT, 'c1', 'conn-c1'));
		expect(created[0]?.end).not.toHaveBeenCalled();
		apply(vs(BOT, null, 'conn-c1'));
		expect(created[0]?.end).toHaveBeenCalledWith('dropped');
		expect(manager.get('c1')).toBeUndefined();
	});

	it('the bot moved to another channel ends with dropped', async () => {
		const {manager, created, apply} = setup();
		apply(vs('u1', 'c1', 'u1-conn'));
		await manager.getOrStart('g1', 'c1');
		apply(vs(BOT, 'c2', 'conn-c1'));
		expect(created[0]?.end).toHaveBeenCalledWith('dropped');
	});

	it('last human leaving ends with empty', async () => {
		const {manager, created, apply} = setup();
		apply(vs('u1', 'c1', 'u1-conn'));
		apply(vs('u2', 'c1', 'u2-conn'));
		await manager.getOrStart('g1', 'c1');
		apply(vs(BOT, 'c1', 'conn-c1'));
		apply(vs('other-bot', 'c1', 'ob-conn'));
		apply(vs('u1', null, 'u1-conn'));
		expect(created[0]?.end).not.toHaveBeenCalled();
		apply(vs('u2', 'c9', 'u2-conn'));
		expect(created[0]?.end).toHaveBeenCalledWith('empty');
		expect(manager.get('c1')).toBeUndefined();
	});

	it('ignores voice states of channels without a session', async () => {
		const {manager, created, apply} = setup();
		apply(vs('u1', 'c1', 'u1-conn'));
		await manager.getOrStart('g1', 'c1');
		apply({...vs('u3', 'c7', 'u3-conn'), guild_id: 'g2'});
		apply({...vs('u3', null, 'u3-conn'), guild_id: 'g2'});
		expect(created[0]?.end).not.toHaveBeenCalled();
	});

	it('endAll ends every session', async () => {
		const {manager, created} = setup();
		await manager.getOrStart('g1', 'c1');
		await manager.getOrStart('g1', 'c2');
		await manager.endAll();
		expect(created.map((s) => s.end.mock.calls[0]?.[0])).toEqual(['stop', 'stop']);
		expect(manager.get('c1')).toBeUndefined();
	});
});
