// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {VoiceStates} from './VoiceStates';
import type {VoiceStateWire} from './Wire';

const vs = (user: string, channel: string | null, conn: string | null = `c-${user}`, guild = 'g1'): VoiceStateWire => ({
	guild_id: guild,
	channel_id: channel,
	user_id: user,
	connection_id: conn,
});

describe('VoiceStates', () => {
	it('tracks joins, moves and leaves per connection', () => {
		const s = new VoiceStates();
		s.apply(vs('u1', 'A'));
		s.apply(vs('u2', 'A'));
		s.apply(vs('u1', 'B', 'c-other'));
		expect([...s.usersIn('A')].sort()).toEqual(['u1', 'u2']);
		expect(s.isIn('u1', 'B')).toBe(true);
		expect(s.connectionsOf('u1')).toHaveLength(2);
		s.apply(vs('u1', 'C'));
		expect(s.isIn('u1', 'A')).toBe(false);
		expect(s.isIn('u1', 'C')).toBe(true);
		s.apply(vs('u1', null));
		expect(s.isIn('u1', 'C')).toBe(false);
		expect(s.connectionsOf('u1')).toEqual([{guildId: 'g1', channelId: 'B', connectionId: 'c-other'}]);
		expect(s.usersIn('nowhere').size).toBe(0);
	});

	it("applyGuild replaces the guild's states", () => {
		const s = new VoiceStates();
		s.apply(vs('u1', 'A'));
		s.apply(vs('u9', 'Z', 'c-9', 'g2'));
		s.applyGuild('g1', [vs('u2', 'B')]);
		expect(s.isIn('u1', 'A')).toBe(false);
		expect(s.isIn('u2', 'B')).toBe(true);
		expect(s.isIn('u9', 'Z')).toBe(true);
	});

	it('ignores states without a connection id or guild', () => {
		const s = new VoiceStates();
		s.apply(vs('u1', 'A', null));
		s.apply({guild_id: null, channel_id: 'A', user_id: 'u1', connection_id: 'x'});
		expect(s.usersIn('A').size).toBe(0);
	});
});
