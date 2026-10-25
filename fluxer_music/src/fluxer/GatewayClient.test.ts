// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AddressInfo} from 'node:net';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {type RawData, type WebSocket as ServerSocket, WebSocketServer} from 'ws';
import {GatewayClient, VoiceJoinError} from './GatewayClient';

type Frame = {op: number; d?: any};

class FakeServer {
	readonly wss: WebSocketServer;
	readonly sockets: Array<ServerSocket> = [];
	readonly frames: Array<Array<Frame>> = [];
	readonly urls: Array<string> = [];
	heartbeatInterval = 10_000;
	ackHeartbeats = true;
	onFrame: (frame: Frame, socket: ServerSocket, conn: number) => void = () => {};

	constructor() {
		this.wss = new WebSocketServer({port: 0, host: '127.0.0.1'});
		this.wss.on('connection', (socket, req) => {
			const conn = this.sockets.length;
			this.sockets.push(socket);
			this.frames.push([]);
			this.urls.push(req.url ?? '');
			socket.on('message', (raw: RawData) => {
				const frame = JSON.parse(raw.toString()) as Frame;
				this.frames[conn]?.push(frame);
				if (frame.op === 1 && this.ackHeartbeats) socket.send(JSON.stringify({op: 11}));
				this.onFrame(frame, socket, conn);
			});
			socket.send(JSON.stringify({op: 10, d: {heartbeat_interval: this.heartbeatInterval}}));
		});
	}

	listening(): Promise<void> {
		return new Promise((resolve) => this.wss.once('listening', () => resolve()));
	}

	get url(): string {
		return `ws://127.0.0.1:${(this.wss.address() as AddressInfo).port}`;
	}

	/** Answers Identify with Ready and Resume with Resumed. */
	autoSession(): void {
		this.onFrame = (frame, socket) => {
			if (frame.op === 2) this.dispatch(socket, 'READY', 1, {session_id: 'sess-1', user: {id: 'bot-1'}});
			if (frame.op === 6) this.dispatch(socket, 'RESUMED', frame.d.seq, {});
		};
	}

	dispatch(socket: ServerSocket, t: string, s: number, d: unknown): void {
		socket.send(JSON.stringify({op: 0, t, s, d}));
	}

	async close(): Promise<void> {
		for (const s of this.wss.clients) s.terminate();
		await new Promise<void>((resolve) => this.wss.close(() => resolve()));
	}
}

async function until(cond: () => boolean, ms = 2000): Promise<void> {
	const start = Date.now();
	while (!cond()) {
		if (Date.now() - start > ms) throw new Error('timed out waiting for condition');
		await new Promise((r) => setTimeout(r, 5));
	}
}

describe('GatewayClient', () => {
	let server: FakeServer;
	let client: GatewayClient;

	const make = (extra: Partial<ConstructorParameters<typeof GatewayClient>[0]> = {}) => {
		client = new GatewayClient({
			token: 'tok',
			url: server.url,
			backoff: {minMs: 10, maxMs: 40},
			invalidSessionWaitMs: {minMs: 10, maxMs: 20},
			voiceTimeoutMs: 1000,
			...extra,
		});
		return client;
	};

	beforeEach(async () => {
		server = new FakeServer();
		await server.listening();
		server.autoSession();
	});

	afterEach(async () => {
		client?.close();
		await server.close();
	});

	it('identifies with ignored_events and becomes ready', async () => {
		make();
		await client.connect();
		expect(server.urls[0]).toContain('v=1');
		expect(server.urls[0]).toContain('encoding=json');
		expect(server.urls[0]).not.toContain('compress');
		expect(server.frames[0]?.[0]).toEqual({
			op: 2,
			d: {
				token: 'tok',
				properties: {os: 'linux', browser: 'fluxer-music', device: 'fluxer-music'},
				ignored_events: ['TYPING_START', 'PRESENCE_UPDATE'],
			},
		});
		expect(client.ready).toBe(true);
		expect(client.userId).toBe('bot-1');
		expect(client.sessionId).toBe('sess-1');
	});

	it('emits dispatch events', async () => {
		make();
		const seen: Array<[string, unknown]> = [];
		client.on('dispatch', (t, d) => seen.push([t, d]));
		await client.connect();
		server.dispatch(server.sockets[0] as ServerSocket, 'MESSAGE_CREATE', 2, {id: 'm'});
		await until(() => seen.length === 2);
		expect(seen[1]).toEqual(['MESSAGE_CREATE', {id: 'm'}]);
	});

	it('heartbeats with last sequence', async () => {
		server.heartbeatInterval = 100;
		make();
		await client.connect();
		server.dispatch(server.sockets[0] as ServerSocket, 'MESSAGE_CREATE', 2, {});
		server.dispatch(server.sockets[0] as ServerSocket, 'MESSAGE_CREATE', 3, {});
		await until(() => (server.frames[0] ?? []).filter((f) => f.op === 1 && f.d === 3).length >= 2);
	});

	it('resumes when a heartbeat is not acknowledged', async () => {
		server.heartbeatInterval = 30;
		make();
		await client.connect();
		server.ackHeartbeats = false;
		await until(() => server.frames.length >= 2);
		await until(() => (server.frames[1] ?? []).some((f) => f.op === 6));
	});

	it('resumes after a drop', async () => {
		make();
		await client.connect();
		server.dispatch(server.sockets[0] as ServerSocket, 'MESSAGE_CREATE', 2, {});
		server.dispatch(server.sockets[0] as ServerSocket, 'MESSAGE_CREATE', 3, {});
		await new Promise((r) => setTimeout(r, 30));
		server.sockets[0]?.terminate();
		await until(() => (server.frames[1] ?? []).length > 0);
		expect(server.frames[1]?.[0]).toEqual({op: 6, d: {token: 'tok', session_id: 'sess-1', seq: 3}});
		await until(() => client.ready);
	});

	it('resumes on op 7', async () => {
		make();
		await client.connect();
		server.sockets[0]?.send(JSON.stringify({op: 7}));
		await until(() => (server.frames[1] ?? []).length > 0);
		expect(server.frames[1]?.[0]?.op).toBe(6);
	});

	it('re-identifies after op 9', async () => {
		make();
		await client.connect();
		server.sockets[0]?.send(JSON.stringify({op: 9, d: false}));
		await until(() => (server.frames[0] ?? []).filter((f) => f.op === 2).length === 2);
		expect(server.frames.length).toBe(1);
	});

	it('4004 emits fatal and does not reconnect', async () => {
		server.onFrame = (frame, socket) => {
			if (frame.op === 2) socket.close(4004, 'Invalid token');
		};
		make();
		const fatal: Array<number> = [];
		client.on('fatal', (code) => fatal.push(code));
		await expect(client.connect()).rejects.toThrow(/4004/);
		expect(fatal).toEqual([4004]);
		await new Promise((r) => setTimeout(r, 100));
		expect(server.sockets).toHaveLength(1);
	});

	describe('voice', () => {
		const voiceFrame = (server: FakeServer) => server.frames[0]?.find((f) => f.op === 4) as Frame | undefined;

		it('joinVoice resolves with the matching grant', async () => {
			make();
			await client.connect();
			const grant = client.joinVoice('g1', 'ch1');
			await until(() => voiceFrame(server) !== undefined);
			expect(voiceFrame(server)?.d).toEqual({
				guild_id: 'g1',
				channel_id: 'ch1',
				self_mute: false,
				self_deaf: false,
				self_video: false,
				self_stream: false,
			});
			const sock = server.sockets[0] as ServerSocket;
			server.dispatch(sock, 'VOICE_SERVER_UPDATE', 2, {
				token: 'other',
				endpoint: 'wss://x',
				connection_id: 'cX',
				channel_id: 'other-channel',
				guild_id: 'g1',
			});
			server.dispatch(sock, 'VOICE_SERVER_UPDATE', 3, {
				token: 'lk-token',
				endpoint: 'wss://lk.example',
				connection_id: 'conn-1',
				channel_id: 'ch1',
				guild_id: 'g1',
			});
			await expect(grant).resolves.toEqual({
				token: 'lk-token',
				endpoint: 'wss://lk.example',
				connectionId: 'conn-1',
				channelId: 'ch1',
				guildId: 'g1',
			});
		});

		it('refreshVoice sends the connection id and leaveVoice leaves it', async () => {
			make();
			await client.connect();
			void client.refreshVoice('g1', 'ch1', 'conn-1').catch(() => {});
			client.leaveVoice('g1', 'conn-1');
			await until(() => (server.frames[0] ?? []).filter((f) => f.op === 4).length === 2);
			const [refresh, leave] = (server.frames[0] ?? []).filter((f) => f.op === 4);
			expect(refresh?.d.connection_id).toBe('conn-1');
			expect(refresh?.d.channel_id).toBe('ch1');
			expect(leave?.d).toEqual({guild_id: 'g1', channel_id: null, connection_id: 'conn-1'});
		});

		it('joinVoice rejects a p2p grant', async () => {
			make();
			await client.connect();
			const grant = client.joinVoice('g1', 'ch1');
			await until(() => voiceFrame(server) !== undefined);
			server.dispatch(server.sockets[0] as ServerSocket, 'VOICE_SERVER_UPDATE', 2, {
				p2p: true,
				ice_servers: [],
				connection_id: 'conn-p',
				channel_id: 'ch1',
				guild_id: 'g1',
			});
			await expect(grant).rejects.toMatchObject({code: 'P2P_GRANT'});
			// The connection the grant opened is released again.
			await until(() => (server.frames[0] ?? []).filter((f) => f.op === 4).length === 2);
			expect((server.frames[0] ?? []).filter((f) => f.op === 4)[1]?.d).toEqual({
				guild_id: 'g1',
				channel_id: null,
				connection_id: 'conn-p',
			});
		});

		it('joinVoice rejects an end-to-end encrypted grant and leaves', async () => {
			make();
			await client.connect();
			const grant = client.joinVoice('g1', 'ch1');
			await until(() => voiceFrame(server) !== undefined);
			server.dispatch(server.sockets[0] as ServerSocket, 'VOICE_SERVER_UPDATE', 2, {
				token: 'lk-token',
				endpoint: 'wss://lk',
				connection_id: 'conn-e',
				channel_id: 'ch1',
				guild_id: 'g1',
				e2ee_key: 'secret-key',
			});
			await expect(grant).rejects.toMatchObject({code: 'E2EE'});
			await until(() => (server.frames[0] ?? []).filter((f) => f.op === 4).length === 2);
			expect((server.frames[0] ?? []).filter((f) => f.op === 4)[1]?.d).toEqual({
				guild_id: 'g1',
				channel_id: null,
				connection_id: 'conn-e',
			});
		});

		it.each(['VOICE_P2P_CONSENT_REQUIRED', 'VOICE_P2P_UNAVAILABLE'])('joinVoice rejects with %s', async (code) => {
			make();
			await client.connect();
			const grant = client.joinVoice('g1', 'ch1');
			await until(() => voiceFrame(server) !== undefined);
			server.sockets[0]?.send(JSON.stringify({op: 12, d: {code, message: 'p2p'}}));
			await expect(grant).rejects.toMatchObject({code});
		});

		it('a gateway error rejects every pending join (op 12 names no channel)', async () => {
			make();
			await client.connect();
			const a = client.joinVoice('g1', 'ch1');
			const b = client.joinVoice('g1', 'ch2');
			await until(() => (server.frames[0] ?? []).filter((f) => f.op === 4).length === 2);
			server.sockets[0]?.send(JSON.stringify({op: 12, d: {code: 'VOICE_CHANNEL_FULL', message: 'full'}}));
			await expect(a).rejects.toMatchObject({code: 'VOICE_CHANNEL_FULL'});
			await expect(b).rejects.toMatchObject({code: 'VOICE_CHANNEL_FULL'});
		});

		it('joinVoice rejects on gateway error', async () => {
			make();
			await client.connect();
			const grant = client.joinVoice('g1', 'ch1');
			await until(() => voiceFrame(server) !== undefined);
			server.sockets[0]?.send(JSON.stringify({op: 12, d: {code: 'VOICE_CHANNEL_FULL', message: 'full'}}));
			const err = await grant.catch((e: unknown) => e);
			expect(err).toBeInstanceOf(VoiceJoinError);
			expect((err as VoiceJoinError).code).toBe('VOICE_CHANNEL_FULL');
		});

		it('joinVoice times out', async () => {
			make({voiceTimeoutMs: 50});
			await client.connect();
			await expect(client.joinVoice('g1', 'ch1')).rejects.toMatchObject({code: 'TIMEOUT'});
		});

		it('joinVoice rejects when not connected', async () => {
			make();
			await expect(client.joinVoice('g1', 'ch1')).rejects.toMatchObject({code: 'NOT_CONNECTED'});
		});
	});
});
