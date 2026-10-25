// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import type {MusicState} from '@fluxer/music_protocol/src/Types';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {MusicError} from '../Errors';
import {type VoiceGrant, VoiceJoinError} from '../fluxer/GatewayClient';
import type {ResolvedTrack} from '../Types';
import {ChannelSession, type EndReason, type SessionDeps} from './ChannelSession';

function yt(title: string, durationMs = 180_000): ResolvedTrack {
	return {title, author: 'Artist', durationMs, thumbnailUrl: null, url: `https://youtu.be/${title}`, source: 'youtube'};
}

function spotify(title: string): ResolvedTrack {
	return {
		title,
		author: 'Artist',
		durationMs: 180_000,
		thumbnailUrl: null,
		url: `https://open.spotify.com/track/${title}`,
		source: 'spotify',
	};
}

function grant(connectionId: string): VoiceGrant {
	return {token: 't', endpoint: 'wss://lk', connectionId, channelId: 'c1', guildId: 'g1'};
}

function deferred<T>() {
	let resolve!: (v: T) => void;
	let reject!: (e: unknown) => void;
	const promise = new Promise<T>((res, rej) => {
		resolve = res;
		reject = rej;
	});
	return {promise, resolve, reject};
}

class FakePlayer extends EventEmitter {
	positionMs = 0;
	state: 'stopped' | 'playing' | 'paused' = 'stopped';
	play = vi.fn((_url: string, startMs = 0) => {
		this.positionMs = startMs;
		this.state = 'playing';
	});
	pause = vi.fn(() => {
		if (this.state === 'playing') this.state = 'paused';
	});
	resume = vi.fn(() => {
		if (this.state === 'paused') this.state = 'playing';
	});
	seek = vi.fn((ms: number) => {
		this.positionMs = ms;
	});
	stop = vi.fn(() => {
		this.state = 'stopped';
	});
}

class FakeRoom extends EventEmitter {
	sink = {captureFrame: async () => {}};
	connect = vi.fn(async () => {});
	reconnect = vi.fn(async () => {});
	broadcast = vi.fn(async (_state: MusicState) => {});
	disconnect = vi.fn(async () => {});
}

function setup(opts: {idleMs?: number} = {}) {
	const player = new FakePlayer();
	const room = new FakeRoom();
	const resolver = {
		resolve: vi.fn(async (input: string) => [yt(input)]),
		search: vi.fn(async () => [yt('found')]),
		matchSpotify: vi.fn(async (t: ResolvedTrack): Promise<ResolvedTrack | null> => yt(`${t.title}-yt`)),
		streamUrl: vi.fn(async (t: ResolvedTrack) => `stream:${t.url}`),
	};
	const voice = {
		join: vi.fn(async () => grant('conn-1')),
		refresh: vi.fn(async (_id: string) => grant('conn-2')),
		leave: vi.fn((_id: string) => {}),
	};
	const card = {
		start: vi.fn(async (_s: MusicState) => {}),
		update: vi.fn((_s: MusicState) => {}),
		repost: vi.fn(async (_s: MusicState) => {}),
		notice: vi.fn(async (_t: string) => {}),
		finish: vi.fn(async () => {}),
		messageId: 'card-1' as string | null,
	};
	const deps: SessionDeps = {
		resolver,
		createPlayer: () => player,
		room,
		voice,
		card,
		now: () => Date.now(),
	};
	const session = new ChannelSession('g1', 'c1', deps, opts);
	const ended: Array<EndReason> = [];
	session.on('ended', (r: EndReason) => ended.push(r));
	return {session, player, room, resolver, voice, card, ended};
}

/** Lets pending promise chains settle under fake timers. */
async function flush(): Promise<void> {
	for (let i = 0; i < 20; i++) await Promise.resolve();
}

describe('ChannelSession', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		vi.spyOn(console, 'error').mockImplementation(() => {});
	});
	afterEach(() => {
		vi.useRealTimers();
		vi.restoreAllMocks();
	});

	it('add starts playback and broadcasts loading then playing', async () => {
		const {session, player, room} = setup();
		await session.start();
		expect(await session.add('u1', 'A')).toBe(1);
		await flush();
		expect(player.play).toHaveBeenCalledWith('stream:https://youtu.be/A');
		player.emit('started');
		const statuses = room.broadcast.mock.calls.map(([s]) => s.status);
		expect(statuses).toContain('loading');
		expect(statuses.at(-1)).toBe('playing');
		expect(statuses.indexOf('loading')).toBeLessThan(statuses.lastIndexOf('playing'));
		const state = session.state();
		expect(state.current?.title).toBe('A');
		expect(state.lastAction).toMatchObject({kind: 'add', by: 'u1', trackTitle: 'A'});
	});

	it('ended advances to the next track', async () => {
		const {session, player} = setup();
		await session.start();
		await session.add('u1', 'A');
		await session.add('u1', 'B');
		await flush();
		player.emit('started');
		player.emit('ended');
		await flush();
		expect(player.play).toHaveBeenLastCalledWith('stream:https://youtu.be/B');
		expect(session.state().current?.title).toBe('B');
		expect(session.state().queueLength).toBe(0);
	});

	it('skip during loading plays only the following track', async () => {
		const {session, player, resolver} = setup();
		const a = deferred<string>();
		resolver.streamUrl.mockImplementationOnce(() => a.promise);
		await session.start();
		await session.add('u1', 'A');
		await session.add('u1', 'B');
		await flush();
		expect(session.state().status).toBe('loading');
		session.skip('u1');
		await flush();
		a.resolve('stream:A-late');
		await flush();
		expect(player.play).toHaveBeenCalledTimes(1);
		expect(player.play).toHaveBeenCalledWith('stream:https://youtu.be/B');
		expect(session.state().current?.title).toBe('B');
	});

	it('three failures pause and post the blocking notice', async () => {
		const {session, player, resolver, card} = setup();
		resolver.streamUrl.mockRejectedValue(new Error('blocked'));
		resolver.resolve.mockResolvedValueOnce([yt('A'), yt('B'), yt('C'), yt('D')]);
		await session.start();
		await session.add('u1', 'list');
		await flush();
		expect(resolver.streamUrl).toHaveBeenCalledTimes(3);
		expect(player.play).not.toHaveBeenCalled();
		const state = session.state();
		expect(state.status).toBe('paused');
		expect(state.current?.title).toBe('D');
		expect(state.lastAction).toMatchObject({kind: 'blocked', by: null});
		expect(card.notice).toHaveBeenCalledWith('YouTube may be blocking us — see FRIENDS.md.');
	});

	it('a started track resets the failure count', async () => {
		const {session, player, resolver, card} = setup();
		resolver.streamUrl
			.mockRejectedValueOnce(new Error('x'))
			.mockRejectedValueOnce(new Error('x'))
			.mockResolvedValueOnce('stream:C')
			.mockRejectedValueOnce(new Error('x'));
		resolver.resolve.mockResolvedValueOnce([yt('A'), yt('B'), yt('C'), yt('D'), yt('E')]);
		await session.start();
		await session.add('u1', 'list');
		await flush();
		player.emit('started');
		player.emit('error', new Error('ffmpeg died'));
		await flush();
		expect(session.state().lastAction).toMatchObject({kind: 'failed', trackTitle: 'D'});
		expect(card.notice).not.toHaveBeenCalled();
	});

	it('unmatched spotify track is skipped with lastAction unmatched', async () => {
		const {session, player, resolver} = setup();
		resolver.resolve.mockResolvedValueOnce([spotify('S1'), yt('B')]);
		resolver.matchSpotify.mockResolvedValue(null);
		await session.start();
		await session.add('u1', 'https://open.spotify.com/album/x');
		await flush();
		expect(player.play).toHaveBeenCalledTimes(1);
		expect(player.play).toHaveBeenCalledWith('stream:https://youtu.be/B');
		expect(session.state().lastAction).toMatchObject({kind: 'unmatched', by: null, trackTitle: 'S1'});
	});

	it('matched spotify track plays the YouTube match', async () => {
		const {session, player, resolver} = setup();
		resolver.resolve.mockResolvedValueOnce([spotify('S1')]);
		await session.start();
		await session.add('u1', 'https://open.spotify.com/track/x');
		await flush();
		expect(resolver.matchSpotify).toHaveBeenCalledTimes(1);
		expect(player.play).toHaveBeenCalledWith('stream:https://youtu.be/S1-yt');
		expect(session.state().current).toMatchObject({title: 'S1', url: 'https://youtu.be/S1-yt'});
	});

	it('matches upcoming spotify tracks ahead when they enter the top 3', async () => {
		const {session, resolver} = setup();
		resolver.resolve.mockResolvedValueOnce([yt('A'), spotify('S1'), spotify('S2'), spotify('S3'), spotify('S4')]);
		await session.start();
		await session.add('u1', 'list');
		await flush();
		expect(resolver.matchSpotify.mock.calls.map(([t]) => t.title)).toEqual(['S1', 'S2', 'S3']);
	});

	it('prefetches the next stream 15s before the end and uses it', async () => {
		const {session, player, resolver} = setup();
		await session.start();
		await session.add('u1', 'A');
		await session.add('u1', 'B');
		await flush();
		player.emit('started');
		expect(resolver.streamUrl).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(180_000 - 15_000 - 1);
		expect(resolver.streamUrl).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(1);
		expect(resolver.streamUrl).toHaveBeenCalledTimes(2);
		player.emit('ended');
		await flush();
		expect(resolver.streamUrl).toHaveBeenCalledTimes(2);
		expect(player.play).toHaveBeenLastCalledWith('stream:https://youtu.be/B');
	});

	it('a prefetched stream is not used once the next track changed', async () => {
		const {session, player, resolver} = setup();
		await session.start();
		await session.add('u1', 'A');
		await session.add('u1', 'B');
		await flush();
		player.emit('started');
		await vi.advanceTimersByTimeAsync(165_000);
		await session.add('u1', 'C', true);
		player.emit('ended');
		await flush();
		expect(player.play).toHaveBeenLastCalledWith('stream:https://youtu.be/C');
		expect(resolver.streamUrl).toHaveBeenCalledTimes(3);
	});

	it('idle timer ends the session after 5 minutes', async () => {
		const {session, ended, player} = setup();
		await session.start();
		await session.add('u1', 'A');
		await flush();
		player.emit('started');
		player.emit('ended');
		await flush();
		expect(session.state().status).toBe('idle');
		await vi.advanceTimersByTimeAsync(299_999);
		expect(ended).toEqual([]);
		await vi.advanceTimersByTimeAsync(1);
		expect(ended).toEqual(['idle']);
	});

	it('add cancels the idle timer', async () => {
		const {session, ended} = setup();
		await session.start();
		await vi.advanceTimersByTimeAsync(200_000);
		await session.add('u1', 'A');
		await vi.advanceTimersByTimeAsync(200_000);
		expect(ended).toEqual([]);
	});

	it('seek beyond duration → BAD_REQUEST', async () => {
		const {session, player} = setup();
		await session.start();
		expect(() => session.seek('u1', 1000)).toThrow(expect.objectContaining({code: 'BAD_REQUEST'}));
		await session.add('u1', 'A');
		await flush();
		player.emit('started');
		expect(() => session.seek('u1', 180_001)).toThrow(MusicError);
		expect(() => session.seek('u1', 180_001)).toThrow(expect.objectContaining({code: 'BAD_REQUEST'}));
		session.seek('u1', 60_000);
		expect(player.seek).toHaveBeenCalledWith(60_000);
	});

	it('a paused session stays paused across a seek', async () => {
		const {session, player} = setup();
		await session.start();
		await session.add('u1', 'A');
		await flush();
		player.emit('started');
		session.pause('u1');
		session.seek('u1', 30_000);
		expect(session.state()).toMatchObject({status: 'paused', positionMs: 30_000});
		expect(session.state().lastAction).toMatchObject({kind: 'seek', by: 'u1', trackTitle: 'A'});
	});

	it('remove unknown → NOT_FOUND', async () => {
		const {session} = setup();
		await session.start();
		expect(() => session.remove('u1', 'nope')).toThrow(expect.objectContaining({code: 'NOT_FOUND'}));
		expect(() => session.move('u1', 'nope', 0)).toThrow(expect.objectContaining({code: 'NOT_FOUND'}));
	});

	it('state is broadcast every 5s', async () => {
		const {session, room} = setup();
		await session.start();
		const before = room.broadcast.mock.calls.length;
		await vi.advanceTimersByTimeAsync(15_000);
		expect(room.broadcast.mock.calls.length - before).toBe(3);
		await session.end('stop');
		await vi.advanceTimersByTimeAsync(15_000);
		expect(room.broadcast.mock.calls.length - before).toBe(3);
	});

	it('a rejected broadcast is caught', async () => {
		const {session, room} = setup();
		room.broadcast.mockRejectedValue(new Error('no room'));
		await session.start();
		await session.add('u1', 'A');
		await vi.advanceTimersByTimeAsync(5_000);
		expect(console.error).toHaveBeenCalled();
	});

	it('room disconnect refreshes the grant and resumes at position', async () => {
		const {session, player, room, voice} = setup();
		await session.start();
		await session.add('u1', 'A');
		await flush();
		player.emit('started');
		player.positionMs = 42_000;
		room.emit('disconnected');
		expect(player.pause).toHaveBeenCalled();
		await flush();
		expect(voice.refresh).toHaveBeenCalledWith('conn-1');
		expect(room.reconnect).toHaveBeenCalledWith(grant('conn-2'));
		expect(player.resume).toHaveBeenCalled();
		expect(session.connectionId).toBe('conn-2');
		expect(session.state()).toMatchObject({status: 'playing', positionMs: 42_000});
	});

	it('room lost after 5 failed rejoins ends with room-lost', async () => {
		const {session, room, voice, card, ended} = setup();
		voice.refresh.mockRejectedValue(new VoiceJoinError('TIMEOUT'));
		await session.start();
		room.emit('disconnected');
		await vi.advanceTimersByTimeAsync(60_000);
		expect(voice.refresh).toHaveBeenCalledTimes(5);
		expect(card.notice).toHaveBeenCalledWith('Lost the voice connection.');
		expect(ended).toEqual(['room-lost']);
		expect(room.disconnect).toHaveBeenCalled();
	});

	it('flapping room (reconnect succeeds then drops) ends with room-lost', async () => {
		const {session, room, voice, card, ended} = setup();
		room.reconnect.mockImplementation(async () => {
			setTimeout(() => room.emit('disconnected'), 10);
		});
		await session.start();
		room.emit('disconnected');
		await vi.advanceTimersByTimeAsync(60_000);
		expect(room.reconnect).toHaveBeenCalledTimes(5);
		expect(voice.refresh).toHaveBeenCalledTimes(5);
		expect(card.notice).toHaveBeenCalledWith('Lost the voice connection.');
		expect(ended).toEqual(['room-lost']);
	});

	it('a room lost again during its reconnect counts toward the cap and backs off', async () => {
		const {session, room, voice, ended} = setup();
		room.reconnect.mockImplementation(async () => {
			room.emit('disconnected');
		});
		await session.start();
		room.emit('disconnected');
		await flush();
		expect(voice.refresh).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(999);
		expect(voice.refresh).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(1);
		expect(voice.refresh).toHaveBeenCalledTimes(2);
		await vi.advanceTimersByTimeAsync(60_000);
		expect(voice.refresh).toHaveBeenCalledTimes(5);
		expect(ended).toEqual(['room-lost']);
	});

	it('rejoin attempts spread over more than 2 minutes do not end the session', async () => {
		const {session, room, voice, ended} = setup({idleMs: 3_600_000});
		await session.start();
		for (let i = 0; i < 8; i++) {
			room.emit('disconnected');
			await vi.advanceTimersByTimeAsync(40_000);
		}
		expect(voice.refresh).toHaveBeenCalledTimes(8);
		expect(ended).toEqual([]);
	});

	it('end during an in-flight reconnect leaves no room connected', async () => {
		const {session, room} = setup();
		const reconnecting = deferred<void>();
		room.reconnect.mockImplementationOnce(() => reconnecting.promise);
		await session.start();
		room.emit('disconnected');
		await flush();
		expect(room.reconnect).toHaveBeenCalled();
		const ending = session.end('stop');
		await flush();
		expect(room.disconnect).not.toHaveBeenCalled();
		reconnecting.resolve();
		await ending;
		expect(room.disconnect).toHaveBeenCalledTimes(1);
	});

	it('start() failure ends the session', async () => {
		const {session, voice, room, ended} = setup();
		voice.join.mockRejectedValue(new VoiceJoinError('P2P_GRANT'));
		await expect(session.start()).rejects.toMatchObject({code: 'P2P_GRANT'});
		expect(ended).toEqual(['join-failed']);
		expect(room.connect).not.toHaveBeenCalled();
	});

	it('end is idempotent and later actions throw NOT_FOUND', async () => {
		const {session, player, room, voice, card, ended} = setup();
		await session.start();
		await Promise.all([session.end('stop'), session.end('idle')]);
		expect(ended).toEqual(['stop']);
		expect(player.stop).toHaveBeenCalled();
		expect(room.disconnect).toHaveBeenCalledTimes(1);
		expect(voice.leave).toHaveBeenCalledWith('conn-1');
		expect(card.finish).toHaveBeenCalledTimes(1);
		await expect(session.add('u1', 'A')).rejects.toMatchObject({code: 'NOT_FOUND'});
		expect(() => session.skip('u1')).toThrow(expect.objectContaining({code: 'NOT_FOUND'}));
	});

	it('stop records the action and ends the session', async () => {
		const {session, room, ended} = setup();
		await session.start();
		await session.stop('u1');
		expect(room.broadcast.mock.calls.at(-1)?.[0].lastAction).toMatchObject({kind: 'stop', by: 'u1'});
		expect(ended).toEqual(['stop']);
	});

	it('state copies upcoming and caps it at 25', async () => {
		const {session, resolver} = setup();
		resolver.resolve.mockResolvedValueOnce(Array.from({length: 40}, (_, i) => yt(`T${i}`)));
		await session.start();
		await session.add('u1', 'list');
		const state = session.state();
		expect(state.upcoming).toHaveLength(25);
		expect(state.queueLength).toBe(39);
		state.upcoming.pop();
		expect(session.state().upcoming).toHaveLength(25);
	});

	it('handlers map RPCs with the caller as by', async () => {
		const {session, player} = setup();
		await session.start();
		const handlers = session.handlers();
		expect(await handlers['music.add']('42', {input: 'A'})).toEqual({added: 1});
		await flush();
		player.emit('started');
		await handlers['music.skip']('42', {});
		expect(session.state().lastAction).toMatchObject({kind: 'skip', by: '42', trackTitle: 'A'});
		await handlers['music.loop']('7', {mode: 'queue'});
		expect(session.state()).toMatchObject({loop: 'queue', lastAction: {kind: 'loop', by: '7', trackTitle: null}});
		expect(await handlers['music.queue']('7', {offset: 0, limit: 10})).toEqual({tracks: [], total: 0});
		expect(await handlers['music.search']('7', {q: 'x'})).toEqual({tracks: [yt('found')]});
	});
});
