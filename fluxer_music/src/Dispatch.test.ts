// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';
import type {CommandHandler} from './chat/CommandHandler';
import {createDispatchHandler} from './Dispatch';
import {VoiceStates} from './fluxer/VoiceStates';
import type {SessionManager} from './session/SessionManager';

function setup() {
	const voiceStates = new VoiceStates();
	const sessions = {get: vi.fn(() => undefined), onVoiceState: vi.fn()};
	const commands = {onMessageCreate: vi.fn(async () => {}), onReactionAdd: vi.fn(async () => {})};
	const gateway = {userId: 'bot1' as string | null, leaveVoice: vi.fn()};
	const bots = new Set<string>();
	const dispatch = createDispatchHandler({
		voiceStates,
		sessions: sessions as unknown as SessionManager,
		commands: commands as unknown as CommandHandler,
		gateway,
		bots,
	});
	return {dispatch, voiceStates, sessions, commands, gateway, bots};
}

describe('dispatch handler', () => {
	afterEach(() => vi.restoreAllMocks());

	it('a throwing handler is logged, not thrown', () => {
		const {dispatch, sessions} = setup();
		sessions.onVoiceState.mockImplementation(() => {
			throw new Error('boom');
		});
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		const state = {guild_id: 'g1', channel_id: 'c1', user_id: 'u1', connection_id: 'cn1'};
		expect(() => dispatch('VOICE_STATE_UPDATE', state)).not.toThrow();
		expect(error).toHaveBeenCalledWith('[music] VOICE_STATE_UPDATE handler failed: boom');
	});

	it('a malformed payload is logged, not thrown', () => {
		const {dispatch} = setup();
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		expect(() => dispatch('GUILD_CREATE', null)).not.toThrow();
		expect(error).toHaveBeenCalledWith(expect.stringMatching(/^\[music\] GUILD_CREATE handler failed: /));
	});

	it('routes voice states, notes bots and leaves stale bot connections', () => {
		const {dispatch, voiceStates, sessions, gateway, bots} = setup();
		dispatch('GUILD_CREATE', {
			id: 'g1',
			voice_states: [
				{channel_id: 'c1', user_id: 'b2', connection_id: 'x', member: {user: {bot: true}}},
				{channel_id: 'c1', user_id: 'bot1', connection_id: 'old'},
			],
		});
		expect(bots.has('b2')).toBe(true);
		expect(gateway.leaveVoice).toHaveBeenCalledWith('g1', 'old');
		const state = {guild_id: 'g1', channel_id: 'c2', user_id: 'u1', connection_id: 'cn1'};
		dispatch('VOICE_STATE_UPDATE', state);
		expect(voiceStates.isIn('u1', 'c2')).toBe(true);
		expect(sessions.onVoiceState).toHaveBeenCalledWith(state, 'bot1');
	});

	it('logs a rejected message handler', async () => {
		const {dispatch, commands} = setup();
		commands.onMessageCreate.mockRejectedValueOnce(new Error('nope'));
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		dispatch('MESSAGE_CREATE', {id: 'm1'});
		await vi.waitFor(() => expect(error).toHaveBeenCalledWith('[music] message handler failed: nope'));
	});
});
