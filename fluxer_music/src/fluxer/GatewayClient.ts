// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export interface VoiceGrant {
	token: string;
	endpoint: string;
	connectionId: string;
	channelId: string;
	guildId: string;
}

/** `code` is the gateway error code, 'P2P_GRANT', 'E2EE', 'TIMEOUT' or 'NOT_CONNECTED'. */
export class VoiceJoinError extends Error {
	constructor(readonly code: string) {
		super(`Voice join failed: ${code}`);
		this.name = 'VoiceJoinError';
	}
}

export interface GatewayClientOptions {
	token: string;
	url: string;
	WebSocketImpl?: typeof WebSocket;
	backoff?: {minMs: number; maxMs: number};
	voiceTimeoutMs?: number;
	/** Wait before identifying again after an Invalid Session (default 1000-5000 ms). */
	invalidSessionWaitMs?: {minMs: number; maxMs: number};
}

interface Payload {
	op: number;
	d?: any;
	s?: number | null;
	t?: string | null;
}

interface PendingVoice {
	channelId: string;
	resolve: (grant: VoiceGrant) => void;
	reject: (err: VoiceJoinError) => void;
	timer: ReturnType<typeof setTimeout>;
}

const OP = {
	Dispatch: 0,
	Heartbeat: 1,
	Identify: 2,
	VoiceStateUpdate: 4,
	Resume: 6,
	Reconnect: 7,
	InvalidSession: 9,
	Hello: 10,
	HeartbeatAck: 11,
	GatewayError: 12,
} as const;

// Close codes after which reconnecting cannot work without changing the token, shard or version.
const FATAL_CLOSE_CODES = new Set([4004, 4010, 4011, 4012]);
const CLOSE_INVALID_SEQUENCE = 4007;
const CLIENT_CLOSE_CODE = 4000;

export class GatewayClient extends EventEmitter {
	private readonly token: string;
	private readonly url: string;
	private readonly WebSocketImpl: typeof WebSocket;
	private readonly backoff: {minMs: number; maxMs: number};
	private readonly voiceTimeoutMs: number;
	private readonly invalidSessionWait: {minMs: number; maxMs: number};

	private socket: WebSocket | null = null;
	private seq: number | null = null;
	private attempt = 0;
	private stopped = false;
	private heartbeatTimer: ReturnType<typeof setInterval> | null = null;
	private heartbeatAcked = true;
	private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
	private identifyTimer: ReturnType<typeof setTimeout> | null = null;
	private readyWaiters: Array<{resolve: () => void; reject: (err: Error) => void}> = [];
	private pendingVoice: Array<PendingVoice> = [];

	private _userId: string | null = null;
	private _sessionId: string | null = null;
	private _ready = false;

	constructor(opts: GatewayClientOptions) {
		super();
		this.token = opts.token;
		this.url = opts.url;
		this.WebSocketImpl = opts.WebSocketImpl ?? WebSocket;
		this.backoff = opts.backoff ?? {minMs: 1000, maxMs: 60_000};
		this.voiceTimeoutMs = opts.voiceTimeoutMs ?? 15_000;
		this.invalidSessionWait = opts.invalidSessionWaitMs ?? {minMs: 1000, maxMs: 5000};
	}

	get userId(): string | null {
		return this._userId;
	}

	get sessionId(): string | null {
		return this._sessionId;
	}

	get ready(): boolean {
		return this._ready;
	}

	/** Opens the socket and resolves on the first Ready. Rejects if closed or on a fatal close code. */
	connect(): Promise<void> {
		this.stopped = false;
		const promise = new Promise<void>((resolve, reject) => {
			this.readyWaiters.push({resolve, reject});
		});
		this.open();
		return promise;
	}

	close(): void {
		this.stopped = true;
		this.clearTimers();
		this._ready = false;
		this.failPendingVoice('NOT_CONNECTED');
		this.settleWaiters(new Error('Gateway client closed'));
		const socket = this.socket;
		this.socket = null;
		socket?.close(1000);
	}

	joinVoice(guildId: string, channelId: string): Promise<VoiceGrant> {
		return this.requestGrant(channelId, {guild_id: guildId, channel_id: channelId});
	}

	refreshVoice(guildId: string, channelId: string, connectionId: string): Promise<VoiceGrant> {
		return this.requestGrant(channelId, {guild_id: guildId, channel_id: channelId, connection_id: connectionId});
	}

	leaveVoice(guildId: string, connectionId: string): void {
		this.send({
			op: OP.VoiceStateUpdate,
			d: {guild_id: guildId, channel_id: null, connection_id: connectionId},
		});
	}

	private requestGrant(channelId: string, fields: Record<string, string>): Promise<VoiceGrant> {
		return new Promise<VoiceGrant>((resolve, reject) => {
			if (!this._ready) {
				reject(new VoiceJoinError('NOT_CONNECTED'));
				return;
			}
			const pending: PendingVoice = {
				channelId,
				resolve,
				reject,
				timer: setTimeout(() => {
					this.removePending(pending);
					reject(new VoiceJoinError('TIMEOUT'));
				}, this.voiceTimeoutMs),
			};
			this.pendingVoice.push(pending);
			this.send({
				op: OP.VoiceStateUpdate,
				d: {...fields, self_mute: false, self_deaf: false, self_video: false, self_stream: false},
			});
		});
	}

	private open(): void {
		const url = new URL(this.url);
		url.searchParams.set('v', '1');
		url.searchParams.set('encoding', 'json');
		const socket = new this.WebSocketImpl(url.toString());
		this.socket = socket;
		socket.addEventListener('message', (event) => {
			if (this.socket === socket) this.onMessage(event.data);
		});
		socket.addEventListener('close', (event) => {
			if (this.socket === socket) this.onClose(event.code);
		});
		// Errors are followed by a close event, which drives the reconnect.
		socket.addEventListener('error', () => {});
	}

	private onMessage(data: unknown): void {
		let payload: Payload;
		try {
			payload = JSON.parse(typeof data === 'string' ? data : String(data)) as Payload;
		} catch {
			return;
		}
		switch (payload.op) {
			case OP.Hello:
				this.onHello(payload.d?.heartbeat_interval);
				break;
			case OP.HeartbeatAck:
				this.heartbeatAcked = true;
				break;
			case OP.Heartbeat:
				this.sendHeartbeat();
				break;
			case OP.Reconnect:
				this.dropSocket();
				break;
			case OP.InvalidSession:
				this.onInvalidSession();
				break;
			case OP.GatewayError:
				// Opcode 12 doesn't name the channel it refused, so every pending join fails.
				this.failPendingVoice(String(payload.d?.code ?? 'GATEWAY_ERROR'));
				break;
			case OP.Dispatch:
				this.onDispatch(payload);
				break;
		}
	}

	private onHello(intervalMs: number): void {
		this.heartbeatAcked = true;
		if (this.heartbeatTimer) clearInterval(this.heartbeatTimer);
		this.heartbeatTimer = setInterval(() => {
			if (!this.heartbeatAcked) {
				// Zombied connection: drop it and resume on a new socket.
				this.dropSocket();
				return;
			}
			this.heartbeatAcked = false;
			this.sendHeartbeat();
		}, intervalMs);
		if (this._sessionId !== null && this.seq !== null) {
			this.send({op: OP.Resume, d: {token: this.token, session_id: this._sessionId, seq: this.seq}});
		} else {
			this.identify();
		}
	}

	private identify(): void {
		this.send({
			op: OP.Identify,
			d: {
				token: this.token,
				properties: {os: 'linux', browser: 'fluxer-music', device: 'fluxer-music'},
				ignored_events: ['TYPING_START', 'PRESENCE_UPDATE'],
			},
		});
	}

	private onInvalidSession(): void {
		this._sessionId = null;
		this.seq = null;
		this._ready = false;
		if (this.identifyTimer) clearTimeout(this.identifyTimer);
		const {minMs, maxMs} = this.invalidSessionWait;
		this.identifyTimer = setTimeout(
			() => {
				this.identifyTimer = null;
				this.identify();
			},
			minMs + Math.random() * (maxMs - minMs),
		);
	}

	private onDispatch(payload: Payload): void {
		const t = payload.t;
		if (!t) return;
		if (typeof payload.s === 'number') this.seq = payload.s;
		if (t === 'READY') {
			this._sessionId = payload.d.session_id;
			this._userId = payload.d.user.id;
			this.markReady();
		} else if (t === 'RESUMED') {
			this.markReady();
		} else if (t === 'VOICE_SERVER_UPDATE') {
			this.onVoiceServerUpdate(payload.d);
		}
		this.emit('dispatch', t, payload.d);
		if (t === 'READY') this.emit('ready');
	}

	private markReady(): void {
		this._ready = true;
		this.attempt = 0;
		this.settleWaiters();
	}

	private onVoiceServerUpdate(d: any): void {
		const pending = this.pendingVoice.find((p) => p.channelId === d.channel_id);
		if (!pending) return;
		this.removePending(pending);
		// The bot takes part in neither P2P calls nor end-to-end encrypted ones:
		// release the connection the grant opened.
		const refusal = d.p2p ? 'P2P_GRANT' : d.e2ee_key !== undefined && d.e2ee_key !== null ? 'E2EE' : null;
		if (refusal) {
			if (typeof d.connection_id === 'string') {
				this.send({
					op: OP.VoiceStateUpdate,
					d: {guild_id: d.guild_id ?? null, channel_id: null, connection_id: d.connection_id},
				});
			}
			pending.reject(new VoiceJoinError(refusal));
			return;
		}
		pending.resolve({
			token: d.token,
			endpoint: d.endpoint,
			connectionId: d.connection_id,
			channelId: d.channel_id,
			guildId: d.guild_id,
		});
	}

	private onClose(code: number): void {
		this.socket = null;
		this.stopHeartbeat();
		if (this.identifyTimer) clearTimeout(this.identifyTimer);
		this.identifyTimer = null;
		this._ready = false;
		this.failPendingVoice('NOT_CONNECTED');
		if (this.stopped) return;
		if (FATAL_CLOSE_CODES.has(code)) {
			this.stopped = true;
			this.clearTimers();
			this.settleWaiters(new Error(`Gateway closed with fatal code ${code}`));
			this.emit('fatal', code);
			return;
		}
		if (code === CLOSE_INVALID_SEQUENCE) {
			this._sessionId = null;
			this.seq = null;
		}
		const delay = Math.min(this.backoff.maxMs, this.backoff.minMs * 2 ** this.attempt);
		this.attempt++;
		this.reconnectTimer = setTimeout(() => {
			this.reconnectTimer = null;
			if (!this.stopped) this.open();
		}, delay);
	}

	/** Closes the current socket from our side so the close handler reconnects. */
	private dropSocket(): void {
		const socket = this.socket;
		if (!socket) return;
		this.stopHeartbeat();
		try {
			socket.close(CLIENT_CLOSE_CODE);
		} catch {
			// already closing
		}
		// Do not wait for the server's close handshake before reconnecting.
		this.onClose(CLIENT_CLOSE_CODE);
	}

	private sendHeartbeat(): void {
		this.send({op: OP.Heartbeat, d: this.seq});
	}

	private send(payload: Payload): void {
		const socket = this.socket;
		if (socket && socket.readyState === 1) socket.send(JSON.stringify(payload));
	}

	private removePending(pending: PendingVoice): void {
		clearTimeout(pending.timer);
		this.pendingVoice = this.pendingVoice.filter((p) => p !== pending);
	}

	private failPendingVoice(code: string): void {
		const pending = this.pendingVoice;
		this.pendingVoice = [];
		for (const p of pending) {
			clearTimeout(p.timer);
			p.reject(new VoiceJoinError(code));
		}
	}

	private settleWaiters(err?: Error): void {
		const waiters = this.readyWaiters;
		this.readyWaiters = [];
		for (const w of waiters) {
			if (err) w.reject(err);
			else w.resolve();
		}
	}

	private stopHeartbeat(): void {
		if (this.heartbeatTimer) clearInterval(this.heartbeatTimer);
		this.heartbeatTimer = null;
	}

	private clearTimers(): void {
		this.stopHeartbeat();
		if (this.reconnectTimer) clearTimeout(this.reconnectTimer);
		if (this.identifyTimer) clearTimeout(this.identifyTimer);
		this.reconnectTimer = null;
		this.identifyTimer = null;
	}
}
