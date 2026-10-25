// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChildProcess, execFileSync, spawn} from 'node:child_process';
import {resolve} from 'node:path';
import {decodeRpcResult} from '@fluxer/music_protocol/src/Rpc';
import {decodeState} from '@fluxer/music_protocol/src/State';
import {MUSIC_STATE_TOPIC, type MusicState} from '@fluxer/music_protocol/src/Types';
import {type RemoteTrack, Room, RoomEvent, TrackKind} from '@livekit/rtc-node';
import {AccessToken, RoomServiceClient} from 'livekit-server-sdk';
import {afterAll, beforeAll, describe, expect, it, vi} from 'vitest';
import {MusicError} from '../Errors';
import type {VoiceGrant} from '../fluxer/GatewayClient';
import {RoomLink, type RpcHandlers} from './RoomLink';

const KEY = 'devkey';
const SECRET = 'secret';
const HTTP_URL = 'http://127.0.0.1:17880';
const WS_URL = 'ws://127.0.0.1:17880';
const ROOM = 'guild_1_channel_2';
const BOT = 'user_900_bot';

let server: ChildProcess;

async function token(identity: string): Promise<string> {
	const at = new AccessToken(KEY, SECRET, {identity});
	at.addGrant({
		roomJoin: true,
		room: ROOM,
		canPublish: true,
		canSubscribe: true,
		canPublishData: true,
	});
	return at.toJwt();
}

async function grant(identity = BOT): Promise<VoiceGrant> {
	return {token: await token(identity), endpoint: WS_URL, connectionId: 'c', channelId: '2', guildId: '1'};
}

async function join(identity: string): Promise<Room> {
	const room = new Room();
	await room.connect(WS_URL, await token(identity), {autoSubscribe: true, dynacast: false});
	return room;
}

const STATE: MusicState = {
	v: 1,
	status: 'idle',
	current: null,
	positionMs: 0,
	positionAt: 0,
	loop: 'off',
	upcoming: [],
	queueLength: 0,
	lastAction: null,
};

function makeHandlers(overrides: Partial<RpcHandlers> = {}): RpcHandlers {
	const handlers = {} as Record<string, unknown>;
	for (const m of [
		'music.search',
		'music.add',
		'music.queue',
		'music.pause',
		'music.resume',
		'music.skip',
		'music.seek',
		'music.remove',
		'music.move',
		'music.clear',
		'music.shuffle',
		'music.loop',
		'music.stop',
	]) {
		handlers[m] = async () => ({});
	}
	return {...(handlers as unknown as RpcHandlers), ...overrides};
}

async function waitForServer(): Promise<void> {
	for (let i = 0; i < 100; i++) {
		try {
			const res = await fetch(HTTP_URL);
			if (res.ok) return;
		} catch {
			// not up yet
		}
		await new Promise((r) => setTimeout(r, 100));
	}
	throw new Error('livekit-server did not start');
}

beforeAll(async () => {
	const bin = execFileSync('bash', [resolve(__dirname, '../../scripts/livekit-test-server.sh')], {
		encoding: 'utf8',
	}).trim();
	server = spawn(
		bin,
		[
			'--dev',
			'--bind',
			'127.0.0.1',
			'--node-ip',
			'127.0.0.1',
			'--port',
			'17880',
			'--rtc.tcp_port',
			'17881',
			'--udp-port',
			'17882',
		],
		{stdio: 'ignore'},
	);
	await waitForServer();
});

afterAll(() => {
	server?.kill('SIGKILL');
});

async function withLink(
	handlers: RpcHandlers,
	fn: (link: RoomLink) => Promise<void>,
	disposables: Array<Room> = [],
): Promise<void> {
	const link = new RoomLink({bitrate: 64_000});
	try {
		await link.connect(await grant(), handlers);
		await fn(link);
	} finally {
		for (const r of disposables) await r.disconnect();
		await link.disconnect();
	}
}

describe('RoomLink', () => {
	it('listener receives music.state broadcasts', async () => {
		const listener = await join('user_42_abc');
		await withLink(
			makeHandlers(),
			async (link) => {
				const received = new Promise<MusicState | null>((res) => {
					listener.on(RoomEvent.DataReceived, (payload, _p, _k, topic) => {
						if (topic === MUSIC_STATE_TOPIC) res(decodeState(payload));
					});
				});
				// The listener may not be subscribed to data yet; broadcast until it arrives.
				const timer = setInterval(() => void link.broadcast(STATE), 200);
				try {
					expect(await received).toEqual(STATE);
				} finally {
					clearInterval(timer);
				}
			},
			[listener],
		);
	});

	it('rpc from listener reaches handler with caller id', async () => {
		const skip = vi.fn(async () => ({}));
		const listener = await join('user_42_abc');
		await withLink(makeHandlers({'music.skip': skip}), async () => {
			const out = await listener.localParticipant?.performRpc({
				destinationIdentity: BOT,
				method: 'music.skip',
				payload: '{}',
			});
			expect(decodeRpcResult(out ?? '')).toEqual({ok: true, value: {}});
			expect(skip).toHaveBeenCalledWith('42', {});
		}, [listener]);
	});

	it('rpc with bad payload → BAD_REQUEST', async () => {
		const seek = vi.fn(async () => ({}));
		const listener = await join('user_42_abc');
		await withLink(makeHandlers({'music.seek': seek}), async () => {
			const out = await listener.localParticipant?.performRpc({
				destinationIdentity: BOT,
				method: 'music.seek',
				payload: '{"ms":"x"}',
			});
			expect(decodeRpcResult(out ?? '')).toEqual({ok: false, error: 'BAD_REQUEST'});
			expect(seek).not.toHaveBeenCalled();
		}, [listener]);
	});

	it('rpc from non-user identity → BAD_REQUEST', async () => {
		const skip = vi.fn(async () => ({}));
		const stranger = await join('admin_x');
		await withLink(makeHandlers({'music.skip': skip}), async () => {
			const out = await stranger.localParticipant?.performRpc({
				destinationIdentity: BOT,
				method: 'music.skip',
				payload: '{}',
			});
			expect(decodeRpcResult(out ?? '')).toEqual({ok: false, error: 'BAD_REQUEST'});
			expect(skip).not.toHaveBeenCalled();
		}, [stranger]);
	});

	it('maps MusicError to its code and other errors to SOURCE_FAILED', async () => {
		const listener = await join('user_42_abc');
		const errSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
		await withLink(makeHandlers({
			'music.skip': async () => {
				throw new MusicError('QUEUE_FULL');
			},
			'music.stop': async () => {
				throw new Error('boom');
			},
		}), async () => {
			const call = async (method: string) =>
				decodeRpcResult(
					(await listener.localParticipant?.performRpc({
						destinationIdentity: BOT,
						method,
						payload: '{}',
					})) ?? '',
				);
			expect(await call('music.skip')).toEqual({ok: false, error: 'QUEUE_FULL'});
			expect(await call('music.stop')).toEqual({ok: false, error: 'SOURCE_FAILED'});
		}, [listener]);
		errSpy.mockRestore();
	});

	it('listener subscribes to an audio track named music', async () => {
		const listener = new Room();
		await listener.connect(WS_URL, await token('user_42_abc'), {autoSubscribe: true, dynacast: false});
		const subscribed = new Promise<{track: RemoteTrack; name: string}>((res) => {
			listener.on(RoomEvent.TrackSubscribed, (track, publication) => {
				res({track, name: publication.name ?? ''});
			});
		});
		await withLink(
			makeHandlers(),
			async (link) => {
				const silence = new Int16Array(960);
				for (let i = 0; i < 100; i++) await link.sink.captureFrame(silence);
				const {track, name} = await subscribed;
				expect(name).toBe('music');
				expect(track.kind).toBe(TrackKind.KIND_AUDIO);
			},
			[listener],
		);
	});

	it('reconnect keeps handlers', async () => {
		const skip = vi.fn(async () => ({}));
		const listener = await join('user_42_abc');
		await withLink(
			makeHandlers({'music.skip': skip}),
			async (link) => {
				const dropped = new Promise<void>((res) => link.once('disconnected', () => res()));
				await new RoomServiceClient(HTTP_URL, KEY, SECRET).removeParticipant(ROOM, BOT);
				await dropped;
				await link.reconnect(await grant());
				const out = await listener.localParticipant?.performRpc({
					destinationIdentity: BOT,
					method: 'music.skip',
					payload: '{}',
				});
				expect(decodeRpcResult(out ?? '')).toEqual({ok: true, value: {}});
				expect(skip).toHaveBeenCalledWith('42', {});
			},
			[listener],
		);
	});
});
