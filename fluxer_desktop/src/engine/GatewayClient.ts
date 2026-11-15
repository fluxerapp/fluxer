// SPDX-License-Identifier: AGPL-3.0-or-later

import {errorMessage} from '@electron/common/ErrorMessage';
import type {EngineClock, EngineTimer} from '@electron/engine/EngineClock';
import type {
	GatewayTransportConnection,
	GatewayTransportEvent,
	GatewayTransportFactory,
} from '@electron/engine/GatewayTransport';
import {ZstdStreamCodec} from '@electron/engine/ZstdStreamCodec';
import type {
	AccountEngineClientConfig,
	AccountEngineSubscriptionRelease,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';

const GatewayOp = Object.freeze({
	DISPATCH: 0,
	HEARTBEAT: 1,
	IDENTIFY: 2,
	RESUME: 6,
	RECONNECT: 7,
	INVALID_SESSION: 9,
	HELLO: 10,
	HEARTBEAT_ACK: 11,
	LAZY_REQUEST: 14,
} as const);

export const GatewayClose = Object.freeze({
	NORMAL: 1000,
	ABNORMAL: 1006,
	DECODE_ERROR: 4002,
	AUTHENTICATION_FAILED: 4004,
	INVALID_SEQ: 4007,
	RATE_LIMITED: 4008,
	SESSION_TIMEOUT: 4009,
	ZOMBIE: 4900,
	HANDOFF: 4901,
} as const);

const NON_RESUMABLE_SERVER_CLOSE_CODES: ReadonlySet<number> = new Set([
	GatewayClose.AUTHENTICATION_FAILED,
	GatewayClose.INVALID_SEQ,
	GatewayClose.SESSION_TIMEOUT,
	4010,
	4011,
	4012,
]);

const GATEWAY_CONNECT_TIMEOUT_MS = 30_000;
const GATEWAY_HELLO_TIMEOUT_MS = 30_000;
const GATEWAY_ZOMBIE_GRACE_MS = 5_000;
const GATEWAY_PROBE_DEADLINE_MS = 5_000;
const GATEWAY_PENDING_FRAMES_MAX = 16_384;
const GATEWAY_PENDING_BYTES_MAX = 16 * 1024 * 1024;
const INVISIBLE_PRESENCE = Object.freeze({status: 'invisible', since: 0, afk: false, activities: []});

export type GatewayCompressionMode = 'zstd-stream' | 'none';

export type GatewayClientExit =
	| {readonly kind: 'closed'; readonly code: number; readonly reason: string; readonly resumable: boolean}
	| {readonly kind: 'auth_failed'}
	| {readonly kind: 'rate_limited'}
	| {readonly kind: 'session_invalid'; readonly resumable: boolean}
	| {readonly kind: 'reconnect_requested'}
	| {readonly kind: 'zombie'}
	| {readonly kind: 'decode_error'}
	| {readonly kind: 'overflow'}
	| {readonly kind: 'timeout'};

export interface GatewayResumeState {
	readonly sessionId: string;
	readonly sequence: number;
}

interface GatewayClientHandlers {
	onDispatch(type: string, data: unknown, sequence: number | null, bytes: number): void;
	onExit(exit: GatewayClientExit): void;
}

export interface GatewayClientOptions {
	readonly gatewayUrl: string;
	readonly address: string | null;
	readonly proxy: string | null;
	readonly token: string;
	readonly config: AccountEngineClientConfig;
	readonly resume: GatewayResumeState | null;
	readonly compression: GatewayCompressionMode;
	readonly transport: GatewayTransportFactory;
	readonly clock: EngineClock;
	readonly handlers: GatewayClientHandlers;
}

interface GatewayFrame {
	readonly op: number;
	readonly d: unknown;
	readonly s: number | null;
	readonly t: string | null;
}

function parseFrame(text: string): GatewayFrame | null {
	let parsed: unknown;
	try {
		parsed = JSON.parse(text);
	} catch {
		return null;
	}
	if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
		return null;
	}
	const record = parsed as Record<string, unknown>;
	if (typeof record.op !== 'number') {
		return null;
	}
	return {
		op: record.op,
		d: record.d,
		s: typeof record.s === 'number' && Number.isSafeInteger(record.s) ? record.s : null,
		t: typeof record.t === 'string' ? record.t : null,
	};
}

function buildGatewayUrl(endpoint: string, apiVersion: number, compression: GatewayCompressionMode): string {
	const url = new URL(endpoint);
	url.searchParams.set('v', String(apiVersion));
	url.searchParams.set('encoding', 'json');
	url.searchParams.set('compress', compression);
	if (compression === 'zstd-stream') {
		url.searchParams.set('stream', '1');
	} else {
		url.searchParams.delete('stream');
	}
	return url.toString();
}

export class GatewayClient {
	private readonly options: GatewayClientOptions;
	private readonly clock: EngineClock;
	private readonly codec: ZstdStreamCodec | null;
	private connection: GatewayTransportConnection | null = null;
	private exited = false;
	private hello = false;
	private heartbeatIntervalMs = 0;
	private heartbeatTimer: EngineTimer | null = null;
	private ackDeadlineTimer: EngineTimer | null = null;
	private connectTimer: EngineTimer | null = null;
	private awaitingAck = false;
	private lastSequence: number | null;
	private sendChain: Promise<void> = Promise.resolve();
	private inboundChain: Promise<void> = Promise.resolve();
	private pendingFrames = 0;
	private pendingBytes = 0;

	constructor(options: GatewayClientOptions) {
		this.options = options;
		this.clock = options.clock;
		this.codec = options.compression === 'zstd-stream' ? new ZstdStreamCodec() : null;
		this.lastSequence = options.resume?.sequence ?? null;
	}

	start(): void {
		this.connectTimer = this.clock.setTimeout(() => this.timeOut(), GATEWAY_CONNECT_TIMEOUT_MS);
		try {
			const url = buildGatewayUrl(this.options.gatewayUrl, this.options.config.apiVersion, this.options.compression);
			this.connection = this.options.transport({
				url,
				address: this.options.address,
				proxy: this.options.proxy,
				onEvent: (event) => this.handleTransportEvent(event),
			});
		} catch (error) {
			this.finish({
				kind: 'closed',
				code: GatewayClose.ABNORMAL,
				reason: error instanceof Error ? error.message : 'Gateway connect failed',
				resumable: true,
			});
		}
	}

	close(code: number, reason: string): void {
		if (this.exited) {
			return;
		}
		this.exited = true;
		this.teardown(code, reason);
	}

	releaseSubscriptions(release: AccountEngineSubscriptionRelease): void {
		if (this.exited || !this.hello) {
			return;
		}
		this.send({op: GatewayOp.LAZY_REQUEST, d: {subscriptions: release}});
	}

	probe(): void {
		if (this.exited || !this.hello) {
			return;
		}
		if (!this.awaitingAck) {
			this.sendHeartbeat();
		}
		this.armAckDeadline(GATEWAY_PROBE_DEADLINE_MS);
	}

	private handleTransportEvent(event: GatewayTransportEvent): void {
		if (this.exited) {
			return;
		}
		switch (event.kind) {
			case 'open':
				this.clearTimer('connect');
				this.connectTimer = this.clock.setTimeout(() => this.timeOut(), GATEWAY_HELLO_TIMEOUT_MS);
				return;
			case 'text':
				if (this.pendingFrames === 0) {
					this.handleText(event.data);
				} else {
					this.enqueueInbound(() => Promise.resolve(event.data), event.data.length);
				}
				return;
			case 'binary': {
				const codec = this.codec;
				if (codec === null) {
					this.failDecode();
					return;
				}
				this.enqueueInbound(() => codec.decompress(event.data), event.data.length);
				return;
			}
			case 'close':
				this.finishFromServerClose(event.code, event.reason);
				return;
			case 'error':
				this.finish({kind: 'closed', code: GatewayClose.ABNORMAL, reason: event.message, resumable: true});
				return;
		}
	}

	private enqueueInbound(read: () => Promise<string>, bytes: number): void {
		this.pendingFrames += 1;
		this.pendingBytes += bytes;
		if (this.pendingFrames > GATEWAY_PENDING_FRAMES_MAX || this.pendingBytes > GATEWAY_PENDING_BYTES_MAX) {
			this.finishLocally({kind: 'overflow'}, GatewayClose.ZOMBIE, 'overflow');
			return;
		}
		this.inboundChain = this.inboundChain.then(async () => {
			if (this.exited) {
				return;
			}
			let text: string;
			try {
				text = await read();
			} catch {
				this.failDecode();
				return;
			} finally {
				this.pendingFrames -= 1;
				this.pendingBytes -= bytes;
			}
			this.handleText(text);
		});
	}

	private handleText(text: string): void {
		if (this.exited || text.length === 0) {
			return;
		}
		const frame = parseFrame(text);
		if (frame === null) {
			this.failDecode();
			return;
		}
		this.handleFrame(frame, text.length);
	}

	private handleFrame(frame: GatewayFrame, bytes: number): void {
		switch (frame.op) {
			case GatewayOp.HELLO:
				this.handleHello(frame.d);
				return;
			case GatewayOp.HEARTBEAT:
				this.sendHeartbeat();
				return;
			case GatewayOp.HEARTBEAT_ACK:
				this.awaitingAck = false;
				this.clearTimer('ack');
				return;
			case GatewayOp.RECONNECT:
				this.finishLocally({kind: 'reconnect_requested'}, GatewayClose.ZOMBIE, 'reconnect');
				return;
			case GatewayOp.INVALID_SESSION: {
				const resumable = frame.d === true;
				this.finishLocally(
					{kind: 'session_invalid', resumable},
					resumable ? GatewayClose.ZOMBIE : GatewayClose.NORMAL,
					'invalid session',
				);
				return;
			}
			case GatewayOp.DISPATCH:
				if (frame.s !== null) {
					this.lastSequence = frame.s;
				}
				if (frame.t !== null) {
					this.options.handlers.onDispatch(frame.t, frame.d, frame.s, bytes);
				}
				return;
			default:
				return;
		}
	}

	private handleHello(data: unknown): void {
		const interval = (data as {heartbeat_interval?: unknown} | null)?.heartbeat_interval;
		if (typeof interval !== 'number' || !Number.isFinite(interval) || interval <= 0) {
			this.failDecode();
			return;
		}
		this.clearTimer('connect');
		this.hello = true;
		this.heartbeatIntervalMs = interval;
		this.scheduleHeartbeat(Math.floor(interval * this.clock.random()));
		const resume = this.options.resume;
		if (resume !== null) {
			this.send({
				op: GatewayOp.RESUME,
				d: {token: this.options.token, session_id: resume.sessionId, seq: resume.sequence},
			});
			return;
		}
		this.send({
			op: GatewayOp.IDENTIFY,
			d: {
				token: this.options.token,
				properties: this.options.config.properties,
				presence: INVISIBLE_PRESENCE,
				flags: this.options.config.identifyFlags,
			},
		});
	}

	private scheduleHeartbeat(delayMs: number): void {
		this.clearTimer('heartbeat');
		this.heartbeatTimer = this.clock.setTimeout(() => {
			this.heartbeatTimer = null;
			if (this.exited) {
				return;
			}
			if (!this.awaitingAck) {
				this.sendHeartbeat();
				this.armAckDeadline(this.heartbeatIntervalMs + GATEWAY_ZOMBIE_GRACE_MS);
			}
			this.scheduleHeartbeat(this.heartbeatIntervalMs);
		}, delayMs);
	}

	private armAckDeadline(delayMs: number): void {
		if (this.ackDeadlineTimer !== null) {
			this.clock.clearTimeout(this.ackDeadlineTimer);
		}
		this.ackDeadlineTimer = this.clock.setTimeout(() => {
			this.ackDeadlineTimer = null;
			if (this.awaitingAck && !this.exited) {
				this.finishLocally({kind: 'zombie'}, GatewayClose.ZOMBIE, 'zombie');
			}
		}, delayMs);
	}

	private sendHeartbeat(): void {
		this.awaitingAck = true;
		this.send({op: GatewayOp.HEARTBEAT, d: this.lastSequence});
	}

	private send(payload: {op: number; d: unknown}): void {
		const text = JSON.stringify(payload);
		const codec = this.codec;
		this.sendChain = this.sendChain.then(async () => {
			if (this.exited || this.connection === null) {
				return;
			}
			let frame: string | Buffer = text;
			try {
				if (codec !== null) {
					frame = await codec.compress(text);
				}
			} catch {
				this.failDecode();
				return;
			}
			if (this.exited || this.connection === null) {
				return;
			}
			try {
				if (typeof frame === 'string') {
					this.connection.sendText(frame);
				} else {
					this.connection.sendBinary(frame);
				}
			} catch (error) {
				this.finishLocally(
					{kind: 'closed', code: GatewayClose.ABNORMAL, reason: errorMessage(error), resumable: true},
					GatewayClose.ZOMBIE,
					'send failed',
				);
			}
		});
	}

	private failDecode(): void {
		this.finishLocally({kind: 'decode_error'}, GatewayClose.DECODE_ERROR, 'decode error');
	}

	private timeOut(): void {
		this.connectTimer = null;
		this.finishLocally({kind: 'timeout'}, GatewayClose.ZOMBIE, 'timeout');
	}

	private finishFromServerClose(code: number, reason: string): void {
		if (code === GatewayClose.AUTHENTICATION_FAILED) {
			this.finish({kind: 'auth_failed'});
			return;
		}
		if (code === GatewayClose.RATE_LIMITED) {
			this.finish({kind: 'rate_limited'});
			return;
		}
		this.finish({kind: 'closed', code, reason, resumable: !NON_RESUMABLE_SERVER_CLOSE_CODES.has(code)});
	}

	private finishLocally(exit: GatewayClientExit, code: number, reason: string): void {
		if (this.exited) {
			return;
		}
		this.exited = true;
		this.teardown(code, reason);
		this.options.handlers.onExit(exit);
	}

	private finish(exit: GatewayClientExit): void {
		if (this.exited) {
			return;
		}
		this.exited = true;
		this.teardown(null, '');
		this.options.handlers.onExit(exit);
	}

	private teardown(code: number | null, reason: string): void {
		this.clearTimer('heartbeat');
		this.clearTimer('ack');
		this.clearTimer('connect');
		const connection = this.connection;
		this.connection = null;
		if (connection !== null && code !== null) {
			try {
				connection.close(code, reason);
			} catch {}
		}
		this.codec?.dispose();
	}

	private clearTimer(kind: 'heartbeat' | 'ack' | 'connect'): void {
		if (kind === 'heartbeat' && this.heartbeatTimer !== null) {
			this.clock.clearTimeout(this.heartbeatTimer);
			this.heartbeatTimer = null;
		} else if (kind === 'ack' && this.ackDeadlineTimer !== null) {
			this.clock.clearTimeout(this.ackDeadlineTimer);
			this.ackDeadlineTimer = null;
		} else if (kind === 'connect' && this.connectTimer !== null) {
			this.clock.clearTimeout(this.connectTimer);
			this.connectTimer = null;
		}
	}
}
