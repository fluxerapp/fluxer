// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {MusicError} from '../Errors';
import type {FluxerRest} from '../fluxer/FluxerRest';
import {VoiceJoinError} from '../fluxer/GatewayClient';
import {VoiceStates} from '../fluxer/VoiceStates';
import type {MessageWire, ReactionWire} from '../fluxer/Wire';
import type {ChannelSession} from '../session/ChannelSession';
import type {SessionManager} from '../session/SessionManager';
import {CommandHandler} from './CommandHandler';

const CH = 'ch1';
const BOT = 'bot1';

function tr(n: number) {
	return {
		id: `t${n}`,
		title: `Song ${n}`,
		author: `A${n}`,
		durationMs: 83_000,
		thumbnailUrl: null,
		url: 'u',
		source: 'youtube',
		addedBy: 'u',
	};
}

function setup(opts: {session?: boolean; member?: boolean; channelType?: number} = {}) {
	const voiceStates = new VoiceStates();
	if (opts.member !== false) {
		voiceStates.apply({guild_id: 'g1', channel_id: CH, user_id: 'u1', connection_id: 'cn1'});
	}
	const session = {
		cardMessageId: 'card1',
		state: vi.fn(() => ({status: 'playing', loop: 'off'})),
		add: vi.fn(async () => 1),
		pause: vi.fn(),
		resume: vi.fn(),
		skip: vi.fn(),
		stop: vi.fn(async () => {}),
		shuffle: vi.fn(),
		setLoop: vi.fn(),
		remove: vi.fn(),
		move: vi.fn(),
		seek: vi.fn(),
		clear: vi.fn(),
		repostCard: vi.fn(async () => {}),
		queuePage: vi.fn(() => ({tracks: [tr(1), tr(2), tr(3)], total: 3})),
	};
	const sessions = {
		get: vi.fn(() => (opts.session ? (session as unknown as ChannelSession) : undefined)),
		getOrStart: vi.fn(async () => session as unknown as ChannelSession),
	};
	const rest = {
		sendMessage: vi.fn(async () => ({id: 'r1'})),
		removeUserReaction: vi.fn(async () => {}),
		getChannel: vi.fn(async (id: string) => ({id, guildId: 'g1', type: opts.channelType ?? 2, bitrate: null})),
	};
	const handler = new CommandHandler({
		sessions: sessions as unknown as SessionManager,
		voiceStates,
		rest: rest as unknown as FluxerRest,
		botUserId: () => BOT,
		botSessionId: () => 'sess1',
	});
	return {handler, session, sessions, rest};
}

function msg(content: string, over: Partial<MessageWire> = {}): MessageWire {
	return {id: 'm1', channel_id: CH, guild_id: 'g1', content, author: {id: 'u1'}, ...over};
}

function react(emoji: string, user = 'u1', over: Partial<ReactionWire> = {}): ReactionWire {
	return {user_id: user, channel_id: CH, message_id: 'card1', guild_id: 'g1', emoji: {name: emoji, id: null}, ...over};
}

const reply = (rest: ReturnType<typeof setup>['rest']) =>
	rest.sendMessage.mock.calls[0] as unknown as [string, {content: string; message_reference: unknown}];

describe('CommandHandler messages', () => {
	it('ignores bot authors', async () => {
		const {handler, rest, sessions} = setup({session: true});
		await handler.onMessageCreate(msg('!play x', {author: {id: 'u1', bot: true}}));
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(sessions.get).not.toHaveBeenCalled();
		expect(sessions.getOrStart).not.toHaveBeenCalled();
	});

	it('ignores non-commands and DMs', async () => {
		const {handler, rest, sessions} = setup();
		await handler.onMessageCreate(msg('hello'));
		await handler.onMessageCreate(msg('!ban x'));
		await handler.onMessageCreate(msg('!play x', {guild_id: undefined}));
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(sessions.getOrStart).not.toHaveBeenCalled();
	});

	it('ignores commands outside guild voice channel chats', async () => {
		const {handler, rest, sessions} = setup({channelType: 0});
		await handler.onMessageCreate(msg('!help'));
		await handler.onMessageCreate(msg('!play x'));
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(sessions.getOrStart).not.toHaveBeenCalled();
	});

	it('caches the channel type per channel', async () => {
		const {handler, rest} = setup();
		await handler.onMessageCreate(msg('!help'));
		await handler.onMessageCreate(msg('!help'));
		expect(rest.getChannel).toHaveBeenCalledTimes(1);
		expect(rest.getChannel).toHaveBeenCalledWith(CH);
		expect(rest.sendMessage).toHaveBeenCalledTimes(2);
	});

	it('stays silent when the channel lookup fails', async () => {
		const {handler, rest} = setup();
		rest.getChannel.mockRejectedValueOnce(new Error('HTTP 500'));
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		await handler.onMessageCreate(msg('!help'));
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(error).toHaveBeenCalled();
		error.mockRestore();
	});

	it('requires being in the channel', async () => {
		const {handler, rest, sessions} = setup({member: false});
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1]).toEqual({content: 'Join the voice channel first.', message_reference: {message_id: 'm1'}});
		expect(sessions.getOrStart).not.toHaveBeenCalled();
	});

	it('play starts a session and adds', async () => {
		const {handler, rest, sessions, session} = setup();
		await handler.onMessageCreate(msg('!play never gonna'));
		expect(sessions.getOrStart).toHaveBeenCalledWith('g1', CH);
		expect(session.add).toHaveBeenCalledWith('u1', 'never gonna', false);
		expect(rest.sendMessage).not.toHaveBeenCalled();
	});

	it('playnext adds to the front', async () => {
		const {handler, session} = setup();
		await handler.onMessageCreate(msg('!playnext x'));
		expect(session.add).toHaveBeenCalledWith('u1', 'x', true);
	});

	it('p2p join failure replies with the P2P message', async () => {
		const {handler, rest, sessions} = setup();
		sessions.getOrStart.mockRejectedValueOnce(new VoiceJoinError('P2P_GRANT'));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe("Music needs a channel that isn't peer-to-peer.");
	});

	it.each(['VOICE_P2P_CONSENT_REQUIRED', 'VOICE_P2P_UNAVAILABLE'])('%s replies with the P2P message', async (code) => {
		const {handler, rest, sessions} = setup();
		sessions.getOrStart.mockRejectedValueOnce(new VoiceJoinError(code));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe("Music needs a channel that isn't peer-to-peer.");
	});

	it('an E2EE join failure replies with the encryption message', async () => {
		const {handler, rest, sessions} = setup();
		sessions.getOrStart.mockRejectedValueOnce(new VoiceJoinError('E2EE'));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe("Music doesn't work in end-to-end encrypted voice channels yet.");
	});

	it('other join failures reply with the code', async () => {
		const {handler, rest, sessions} = setup();
		sessions.getOrStart.mockRejectedValueOnce(new VoiceJoinError('TIMEOUT'));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe("Couldn't join the voice channel (TIMEOUT).");
	});

	it('MusicError replies with its message; others are generic', async () => {
		const {handler, rest, session} = setup();
		session.add.mockRejectedValueOnce(new MusicError('QUEUE_FULL'));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe('The queue is full.');
		rest.sendMessage.mockClear();
		const log = vi.spyOn(console, 'error').mockImplementation(() => {});
		session.add.mockRejectedValueOnce(new Error('boom'));
		await handler.onMessageCreate(msg('!play x'));
		expect(reply(rest)[1].content).toBe('Something went wrong.');
		expect(log).toHaveBeenCalled();
		log.mockRestore();
	});

	it('replies Nothing is playing without a session', async () => {
		const {handler, rest} = setup();
		await handler.onMessageCreate(msg('!skip'));
		expect(reply(rest)[1].content).toBe('Nothing is playing.');
	});

	it('remove maps 1-based index to id', async () => {
		const {handler, session} = setup({session: true});
		await handler.onMessageCreate(msg('!remove 2'));
		expect(session.queuePage).toHaveBeenCalledWith(0, 500);
		expect(session.remove).toHaveBeenCalledWith('u1', 't2');
	});

	it('remove out of range replies not found', async () => {
		const {handler, rest, session} = setup({session: true});
		await handler.onMessageCreate(msg('!remove 9'));
		expect(session.remove).not.toHaveBeenCalled();
		expect(reply(rest)[1].content).toBe("I couldn't find that.");
	});

	it('move maps both positions', async () => {
		const {handler, session} = setup({session: true});
		await handler.onMessageCreate(msg('!move 3 1'));
		expect(session.move).toHaveBeenCalledWith('u1', 't3', 0);
	});

	it('queue lists a page', async () => {
		const {handler, rest} = setup({session: true});
		await handler.onMessageCreate(msg('!queue'));
		expect(reply(rest)[1].content).toBe(
			'Queue (page 1/1):\n1. Song 1 — A1 (1:23)\n2. Song 2 — A2 (1:23)\n3. Song 3 — A3 (1:23)',
		);
	});

	it('queue says when empty', async () => {
		const {handler, rest, session} = setup({session: true});
		session.queuePage.mockReturnValue({tracks: [], total: 0});
		await handler.onMessageCreate(msg('!queue'));
		expect(reply(rest)[1].content).toBe('Queue is empty.');
	});

	it('np reposts the card; seek and loop pass through', async () => {
		const {handler, session} = setup({session: true});
		await handler.onMessageCreate(msg('!np'));
		await handler.onMessageCreate(msg('!seek 1:23'));
		await handler.onMessageCreate(msg('!loop track'));
		expect(session.repostCard).toHaveBeenCalled();
		expect(session.seek).toHaveBeenCalledWith('u1', 83_000);
		expect(session.setLoop).toHaveBeenCalledWith('u1', 'track');
	});

	it('invalid commands reply with usage and help lists commands', async () => {
		const {handler, rest} = setup();
		await handler.onMessageCreate(msg('!remove 0'));
		expect(reply(rest)[1].content).toBe('Usage: !remove <position>');
		rest.sendMessage.mockClear();
		await handler.onMessageCreate(msg('!help'));
		expect(reply(rest)[1].content).toContain('!playnext');
	});
});

describe('CommandHandler reactions', () => {
	it('reaction from member toggles pause and removes the reaction', async () => {
		const {handler, session, rest} = setup({session: true});
		await handler.onReactionAdd(react('⏯️'));
		expect(session.pause).toHaveBeenCalledWith('u1');
		expect(rest.removeUserReaction).toHaveBeenCalledWith(CH, 'card1', '⏯️', 'u1', 'sess1');
	});

	it('resumes when paused', async () => {
		const {handler, session} = setup({session: true});
		session.state.mockReturnValue({status: 'paused', loop: 'off'});
		await handler.onReactionAdd(react('⏯'));
		expect(session.resume).toHaveBeenCalledWith('u1');
	});

	it('maps the other reactions', async () => {
		const {handler, session} = setup({session: true});
		session.state.mockReturnValue({status: 'playing', loop: 'queue'});
		await handler.onReactionAdd(react('⏭'));
		await handler.onReactionAdd(react('🔀'));
		await handler.onReactionAdd(react('🔁'));
		await handler.onReactionAdd(react('⏹'));
		expect(session.skip).toHaveBeenCalledWith('u1');
		expect(session.shuffle).toHaveBeenCalledWith('u1');
		expect(session.setLoop).toHaveBeenCalledWith('u1', 'off');
		expect(session.stop).toHaveBeenCalledWith('u1');
	});

	it('reaction from non-member is only removed', async () => {
		const {handler, session, rest} = setup({session: true});
		await handler.onReactionAdd(react('⏭', 'stranger'));
		expect(session.skip).not.toHaveBeenCalled();
		expect(rest.removeUserReaction).toHaveBeenCalledWith(CH, 'card1', '⏭', 'stranger', 'sess1');
	});

	it('ignores its own reactions', async () => {
		const {handler, session, rest} = setup({session: true});
		await handler.onReactionAdd(react('⏭', BOT));
		expect(session.skip).not.toHaveBeenCalled();
		expect(rest.removeUserReaction).not.toHaveBeenCalled();
	});

	it('ignores reactions on other messages', async () => {
		const {handler, session, rest} = setup({session: true});
		await handler.onReactionAdd(react('⏭', 'u1', {message_id: 'other'}));
		expect(session.skip).not.toHaveBeenCalled();
		expect(rest.removeUserReaction).not.toHaveBeenCalled();
	});
});
