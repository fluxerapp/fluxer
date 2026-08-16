// SPDX-License-Identifier: AGPL-3.0-or-later

import GeoIP from '@app/features/app/state/GeoIP';
import Authentication from '@app/features/auth/state/Authentication';
import {takeFastConnect} from '@app/features/gateway/transport/FastConnect';
import {
	type CompressionType,
	GatewayCompression,
	isGatewayCompressionError,
} from '@app/features/gateway/transport/GatewayCompression';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {
	formatGatewayReadyTimings,
	type GatewayTimings,
	type RpcTimings,
} from '@app/features/gateway/transport/GatewayTimingsFormatter';
import AppStorage, {PRESERVED_RESET_STORAGE_KEYS} from '@app/features/platform/state/PersistentStorage';
import {Logger, LogLevel} from '@app/features/platform/utils/AppLogger';
import {ExponentialBackoff} from '@app/features/platform/utils/RetryScheduler';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {GatewayErrorCode} from '@fluxer/constants/src/GatewayConstants';
import {GatewayCloseCodes, GatewayOpcodes} from '@fluxer/constants/src/GatewayConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import EventEmitter from 'eventemitter3';

const GATEWAY_TIMEOUTS = {
	HeartbeatAck: 15000,
	ResumeResponse: 15000,
	ResumeWindow: 180000,
	MinReconnect: 1000,
	MaxReconnect: 60000,
	Hello: 20000,
} as const;
export const GatewayState = {
	Disconnected: 'DISCONNECTED',
	Connecting: 'CONNECTING',
	Connected: 'CONNECTED',
	Reconnecting: 'RECONNECTING',
} as const;

export type GatewayState = ValueOf<typeof GatewayState>;

export interface GatewayPayload {
	op: number;
	d?: unknown;
	s?: number;
	t?: string;
}

export interface GatewaySocketProperties {
	os: string;
	browser: string;
	device: string;
	locale: string;
	user_agent: string;
	browser_version: string;
	os_version: string;
	build_version: string;
	desktop_app_version?: string | null;
	desktop_app_channel?: string | null;
	desktop_arch?: string | null;
	desktop_os?: string | null;
	e2ee_capable?: boolean;
	latitude?: string;
	longitude?: string;
}

export interface GatewayPresence {
	status: string;
	afk: boolean;
	mobile: boolean;
	custom_status?: GatewayCustomStatusPayload | null;
}

export interface GatewayVoiceStateUpdateParams {
	guild_id: string | null;
	channel_id: string | null;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	viewer_stream_keys?: ReadonlyArray<string>;
	connection_id: string | null;
}

export interface GatewaySocketOptions {
	token: string;
	apiVersion: number;
	properties: GatewaySocketProperties;
	presence?: GatewayPresence;
	compression?: CompressionType;
	identifyFlags?: number;
	initialGuildId?: string | null;
}

interface GatewayResumeProbeOptions {
	accelerateReconnect?: boolean;
}

export interface GatewayErrorData {
	code: GatewayErrorCode;
	message: string;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parseGatewayPayload(json: string): GatewayPayload {
	const parsed: unknown = JSON.parse(json);
	if (!isRecord(parsed) || typeof parsed.op !== 'number' || !Number.isFinite(parsed.op)) {
		throw new Error('Gateway payload missing numeric opcode');
	}
	if (parsed.s !== undefined && typeof parsed.s !== 'number') {
		throw new Error('Gateway payload sequence must be numeric when present');
	}
	if (parsed.t !== undefined && typeof parsed.t !== 'string') {
		throw new Error('Gateway payload event type must be a string when present');
	}
	return {
		op: parsed.op,
		d: parsed.d,
		s: parsed.s,
		t: parsed.t,
	};
}

function isGatewayErrorData(value: unknown): value is GatewayErrorData {
	return isRecord(value) && typeof value.code === 'number' && typeof value.message === 'string';
}

export interface GatewaySocketEvents {
	connecting: () => void;
	connected: () => void;
	ready: (data: unknown) => void;
	resumed: (data: unknown) => void;
	disconnect: (event: {code: number; reason: string; wasClean: boolean}) => void;
	error: (error: Error | Event | CloseEvent) => void;
	fatalError: (error: Error) => void;
	gatewayError: (error: GatewayErrorData) => void;
	message: (payload: GatewayPayload) => void;
	dispatch: (type: string, data: unknown) => void;
	stateChange: (newState: GatewayState, oldState: GatewayState) => void;
	heartbeat: (sequence: number) => void;
	heartbeatAck: () => void;
	networkStatusChange: (online: boolean) => void;
}

type GatewaySocketEventArgs<K extends keyof GatewaySocketEvents> = GatewaySocketEvents[K] extends (
	...args: infer Args
) => void
	? Args
	: never;

export class GatewaySocket extends EventEmitter<GatewaySocketEvents> {
	private readonly log: Logger;
	private reconnectBackoff: ExponentialBackoff;
	private socket: WebSocket | null = null;
	private connectionState: GatewayState = GatewayState.Disconnected;
	private activeSessionId: string | null = null;
	private lastSequenceNumber = 0;
	private lastReconnectAt = 0;
	private connectStartedAt = 0;
	private heartbeatIntervalMs: number | null = null;
	private heartbeatTimeoutId: number | null = null;
	private heartbeatAckTimeoutId: number | null = null;
	private awaitingHeartbeatAck = false;
	private lastHeartbeatAckAt: number | null = null;
	private lastHeartbeatSentAt: number | null = null;
	private lastGatewayMessageAt: number | null = null;
	private helloTimeoutId: number | null = null;
	private resumeTimeoutId: number | null = null;
	private reconnectTimeoutId: number | null = null;
	private invalidSessionTimeoutId: number | null = null;
	private isUserInitiatedDisconnect = false;
	private shouldReconnectImmediately = false;
	private deferredEmitQueue: Array<() => void> = [];
	private deferredEmitTimeoutId: number | null = null;
	private payloadDecompressor: GatewayCompression | null = null;
	private compressionFallbackInProgress = false;

	constructor(
		private readonly gatewayUrlBase: string,
		private readonly options: GatewaySocketOptions,
		private readonly gatewayUrlWrapper?: (url: string) => string,
	) {
		super();
		this.log = new Logger('Gateway');
		this.reconnectBackoff = new ExponentialBackoff({
			minDelay: GATEWAY_TIMEOUTS.MinReconnect,
			maxDelay: GATEWAY_TIMEOUTS.MaxReconnect,
		});
	}

	private emitDeferred<K extends keyof GatewaySocketEvents>(event: K, ...args: GatewaySocketEventArgs<K>): void {
		this.deferredEmitQueue.push(() => {
			(this.emit as (event: K, ...args: GatewaySocketEventArgs<K>) => boolean)(event, ...args);
		});
		if (this.deferredEmitTimeoutId != null) return;
		this.deferredEmitTimeoutId = window.setTimeout(() => {
			this.deferredEmitTimeoutId = null;
			const queue = this.deferredEmitQueue;
			this.deferredEmitQueue = [];
			for (const emitFn of queue) {
				emitFn();
			}
		}, 0);
	}

	connect(): void {
		if (this.connectionState === GatewayState.Connecting || this.connectionState === GatewayState.Connected) {
			this.log.debug('Ignoring connect: already connecting or connected');
			return;
		}
		this.isUserInitiatedDisconnect = false;
		this.connectStartedAt = Date.now();
		this.updateState(GatewayState.Connecting);
		this.openSocket();
	}

	disconnect(code = 1000, reason = 'Client disconnecting', resumable = false): void {
		this.log.info(`Disconnect requested: [${code}] ${reason}, resumable=${resumable}`);
		this.isUserInitiatedDisconnect = !resumable;
		this.clearHelloTimeout();
		this.clearResumeTimeout();
		if (this.reconnectTimeoutId != null) {
			clearTimeout(this.reconnectTimeoutId);
			this.reconnectTimeoutId = null;
		}
		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}
		this.stopHeartbeat();
		if (this.socket && this.socket.readyState === WebSocket.OPEN) {
			try {
				this.socket.close(code, reason);
			} catch (error) {
				this.log.error('Error while closing WebSocket', error);
			}
		}
		if (resumable) {
			this.updateState(GatewayState.Reconnecting);
			this.scheduleReconnect();
		} else {
			this.updateState(GatewayState.Disconnected);
		}
	}

	simulateNetworkDisconnect(): void {
		if (!this.isConnected()) {
			this.log.warn('Cannot simulate network disconnect: not connected');
			return;
		}
		this.log.info('Simulating network disconnect with resumable close');
		this.disconnect(4000, 'Simulated network disconnect', true);
	}

	reset(shouldReconnect = true): void {
		this.log.info(`Resetting gateway connection (reconnect=${shouldReconnect})`);
		this.clearHelloTimeout();
		this.clearResumeTimeout();
		if (this.reconnectTimeoutId != null) {
			clearTimeout(this.reconnectTimeoutId);
			this.reconnectTimeoutId = null;
		}
		this.stopHeartbeat();
		this.clearSession();
		this.resetBackoffInternal();
		this.teardownSocket();
		this.updateState(GatewayState.Disconnected);
		if (shouldReconnect) {
			this.shouldReconnectImmediately = true;
			this.connect();
		}
	}

	handleNetworkStatusChange(online: boolean): void {
		this.log.info(`Network status: ${online ? 'online' : 'offline'}`);
		this.emitDeferred('networkStatusChange', online);
		if (!online) return;
		this.probeAfterResume('online', {accelerateReconnect: true});
	}

	probeAfterResume(reason: string, options: GatewayResumeProbeOptions = {}): void {
		this.log.info(`Probing gateway after resume: reason=${reason}, state=${this.connectionState}`);
		switch (this.connectionState) {
			case GatewayState.Connected:
				this.verifyConnectionFreshness(reason);
				return;
			case GatewayState.Reconnecting:
			case GatewayState.Disconnected: {
				if (this.connectionState === GatewayState.Disconnected && this.isUserInitiatedDisconnect) {
					this.log.debug('Disconnect was user-initiated; not reconnecting on resume');
					return;
				}
				this.reconnectAfterResume(reason, options.accelerateReconnect === true);
				return;
			}
			case GatewayState.Connecting: {
				const connectingFor = this.connectStartedAt > 0 ? Date.now() - this.connectStartedAt : 0;
				if (connectingFor > GATEWAY_TIMEOUTS.Hello) {
					this.log.warn(
						`Stuck in CONNECTING for ${connectingFor}ms on resume (reason=${reason}), forcing fresh attempt`,
					);
					this.forceReconnect(`Stuck connecting on resume: ${reason}`);
				} else {
					this.log.debug(`In CONNECTING for ${connectingFor}ms on resume; allowing in-progress connect to continue`);
				}
				return;
			}
		}
	}

	private reconnectAfterResume(reason: string, accelerateReconnect: boolean): void {
		if (accelerateReconnect) {
			if (this.reconnectTimeoutId != null) {
				clearTimeout(this.reconnectTimeoutId);
				this.reconnectTimeoutId = null;
			}
			this.shouldReconnectImmediately = true;
			this.resetBackoffInternal();
			this.connect();
			return;
		}
		if (this.reconnectTimeoutId != null) {
			this.log.debug(`Reconnect already scheduled; preserving backoff on resume: reason=${reason}`);
			return;
		}
		this.log.debug(`Scheduling reconnect after resume using backoff: reason=${reason}`);
		this.updateState(GatewayState.Reconnecting);
		this.scheduleReconnect({allowImmediate: false});
	}

	forceReconnectFromResume(reason: string): void {
		this.log.warn(`Force reconnect requested from resume handler: ${reason}`);
		this.forceReconnect(reason);
	}

	private verifyConnectionFreshness(reason: string): void {
		if (!this.heartbeatIntervalMs) {
			this.log.debug('No heartbeat interval set; skipping freshness verification');
			return;
		}
		const now = Date.now();
		const lastSeenAt = this.lastGatewayActivityAt();
		if (lastSeenAt == null) {
			this.log.debug('No gateway activity history; skipping freshness verification');
			return;
		}
		const staleness = now - lastSeenAt;
		const staleThreshold = Math.max(Math.floor(this.heartbeatIntervalMs * 1.5), 30_000);
		if (staleness > staleThreshold) {
			this.log.warn(
				`Gateway appears stale on resume: ${staleness}ms since last gateway activity (threshold ${staleThreshold}ms, reason=${reason})`,
			);
			this.forceReconnect(`Stale connection on resume: ${reason}`);
			return;
		}
		if (this.socket?.readyState !== WebSocket.OPEN) {
			this.log.warn(
				`Connection state is Connected but socket readyState is ${this.socket?.readyState ?? 'null'}; forcing reconnect`,
			);
			this.forceReconnect(`Inconsistent socket state on resume: ${reason}`);
			return;
		}
		this.log.debug(`Probing gateway via immediate heartbeat (staleness=${staleness}ms, reason=${reason})`);
		this.kickHeartbeat();
	}

	private kickHeartbeat(): void {
		if (!this.heartbeatIntervalMs) return;
		if (this.heartbeatTimeoutId != null) {
			clearTimeout(this.heartbeatTimeoutId);
			this.heartbeatTimeoutId = null;
		}
		this.handleHeartbeatTick();
	}

	private forceReconnect(reason: string): void {
		this.log.warn(`Forcing zombie socket teardown: ${reason}`);
		this.clearHelloTimeout();
		this.clearResumeTimeout();
		if (this.reconnectTimeoutId != null) {
			clearTimeout(this.reconnectTimeoutId);
			this.reconnectTimeoutId = null;
		}
		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}
		this.stopHeartbeat();
		this.teardownSocket();
		this.emitDeferred('disconnect', {
			code: 4000,
			reason,
			wasClean: false,
		});
		this.isUserInitiatedDisconnect = false;
		this.shouldReconnectImmediately = true;
		this.resetBackoffInternal();
		this.updateState(GatewayState.Reconnecting);
		this.scheduleReconnect();
	}

	updatePresence(
		status: string,
		afk?: boolean,
		mobile?: boolean,
		customStatus?: GatewayCustomStatusPayload | null,
	): void {
		if (!this.isConnected()) return;
		this.sendPayload({
			op: GatewayOpcodes.PRESENCE_UPDATE,
			d: {
				status,
				...(afk !== undefined && {afk}),
				...(mobile !== undefined && {mobile}),
				...(customStatus !== undefined && {custom_status: customStatus}),
			},
		});
	}

	private buildVoiceStatePayload(
		params: GatewayVoiceStateUpdateParams,
		options: {useCurrentConnectionFallback: boolean},
	): GatewayPayload {
		const isMobileLayout = MobileLayout.isMobileLayout();
		const {latitude, longitude} = GeoIP;
		return {
			op: GatewayOpcodes.VOICE_STATE_UPDATE,
			d: {
				...params,
				connection_id: params.connection_id ?? (options.useCurrentConnectionFallback ? MediaEngine.connectionId : null),
				is_mobile: isMobileLayout,
				latitude: latitude ?? undefined,
				longitude: longitude ?? undefined,
			},
		};
	}

	updateVoiceState(params: GatewayVoiceStateUpdateParams): boolean {
		return this.sendPayload(this.buildVoiceStatePayload(params, {useCurrentConnectionFallback: true}));
	}

	updateVoiceStateExplicit(params: GatewayVoiceStateUpdateParams): boolean {
		return this.sendPayload(this.buildVoiceStatePayload(params, {useCurrentConnectionFallback: false}));
	}

	requestGuildMembers(params: {
		guildId?: string;
		guildIds?: Array<string>;
		query?: string;
		limit?: number;
		userIds?: Array<string>;
		presences?: boolean;
		nonce?: string;
	}): void {
		if (!this.isConnected()) return;
		const guildIds = params['guildIds'] ? [...new Set(params['guildIds'].filter((guildId) => guildId.length > 0))] : [];
		const guildPayload: {
			guild_id?: string;
			guild_ids?: Array<string>;
		} = {};
		if (guildIds.length > 0) {
			guildPayload.guild_ids = guildIds;
		} else if (params['guildId']) {
			guildPayload.guild_id = params['guildId'];
		}
		if (!guildPayload.guild_id && !guildPayload.guild_ids) {
			return;
		}
		this.sendPayload({
			op: GatewayOpcodes.REQUEST_GUILD_MEMBERS,
			d: {
				...guildPayload,
				...(params['query'] !== undefined && {query: params['query']}),
				...(params['limit'] !== undefined && {limit: params['limit']}),
				...(params['userIds'] !== undefined && {user_ids: [...new Set(params['userIds'])]}),
				...(params['presences'] !== undefined && {presences: params['presences']}),
				...(params['nonce'] !== undefined && {nonce: params['nonce']}),
			},
		});
	}

	updateGuildSubscriptions(params: {
		subscriptions: Record<
			string,
			{
				active?: boolean;
				member_list_channels?: Record<string, Array<[number, number]>>;
				typing?: boolean;
				members?: Array<string>;
				sync?: boolean;
			}
		>;
	}): void {
		if (!this.isConnected()) return;
		this.sendPayload({
			op: GatewayOpcodes.LAZY_REQUEST,
			d: params,
		});
	}

	requestGuildCounts(guildIds: Array<string>): void {
		if (!this.isConnected()) return;
		const uniqueGuildIds = [...new Set(guildIds.filter((id) => id.length > 0))];
		if (uniqueGuildIds.length === 0) return;
		this.sendPayload({
			op: GatewayOpcodes.REQUEST_GUILD_COUNTS,
			d: {guild_ids: uniqueGuildIds},
		});
	}

	requestChannelMemberCounts(params: {guildId: string; channelIds: Array<string>; nonce?: string}): void {
		if (!this.isConnected()) return;
		if (params.guildId.length === 0) return;
		const uniqueChannelIds = [...new Set(params.channelIds.filter((id) => id.length > 0))];
		if (uniqueChannelIds.length === 0) return;
		this.sendPayload({
			op: GatewayOpcodes.REQUEST_CHANNEL_MEMBER_COUNTS,
			d: {
				guild_id: params.guildId,
				channel_ids: uniqueChannelIds,
				...(params.nonce !== undefined && {nonce: params.nonce}),
			},
		});
	}

	setToken(token: string): void {
		this.options.token = token;
	}

	getState(): GatewayState {
		return this.connectionState;
	}

	getSessionId(): string | null {
		return this.activeSessionId;
	}

	getSequence(): number {
		return this.lastSequenceNumber;
	}

	isConnected(): boolean {
		return this.connectionState === GatewayState.Connected && this.socket?.readyState === WebSocket.OPEN;
	}

	isConnecting(): boolean {
		return this.connectionState === GatewayState.Connecting;
	}

	private openSocket(): void {
		this.teardownSocket();
		this.buildGatewayUrl()
			.then((url) => {
				const adopted = takeFastConnect(url);
				this.log.debug(`Opening WebSocket connection to ${url}`);
				try {
					this.socket = adopted ? adopted.ws : new WebSocket(url);
					const compression: CompressionType = this.options.compression ?? 'zstd-stream';
					if (compression !== 'none') {
						this.socket.binaryType = 'arraybuffer';
						this.payloadDecompressor = new GatewayCompression(compression, true);
						void this.payloadDecompressor.warmup();
					} else {
						this.socket.binaryType = 'blob';
						this.payloadDecompressor = null;
					}
					this.compressionFallbackInProgress = false;
					this.socket.addEventListener('open', this.handleSocketOpen);
					this.socket.addEventListener('message', this.handleSocketMessage);
					this.socket.addEventListener('close', this.handleSocketClose);
					this.socket.addEventListener('error', this.handleSocketError);
					this.startHelloTimeout();
					this.emitDeferred('connecting');
					if (adopted) {
						this.log.info(
							`Adopted fast connect socket opened ${Date.now() - adopted.state.startedAt}ms ago with ${adopted.state.messages.length} buffered message(s)`,
						);
						if (adopted.state.open || this.socket.readyState === WebSocket.OPEN) {
							this.handleSocketOpen(new Event('open'));
						}
						for (const message of adopted.state.messages) {
							void this.handleSocketMessage(message);
						}
					}
				} catch (error) {
					this.log.error('Failed to create WebSocket', error);
					this.handleConnectionFailure();
				}
			})
			.catch((error) => {
				this.log.error('Failed to build gateway URL', error);
				this.handleConnectionFailure();
			});
	}

	private teardownSocket(): void {
		if (this.payloadDecompressor) {
			this.payloadDecompressor.destroy();
			this.payloadDecompressor = null;
		}
		if (!this.socket) return;
		try {
			this.socket.removeEventListener('open', this.handleSocketOpen);
			this.socket.removeEventListener('message', this.handleSocketMessage);
			this.socket.removeEventListener('close', this.handleSocketClose);
			this.socket.removeEventListener('error', this.handleSocketError);
			if (this.socket.readyState === WebSocket.OPEN) {
				this.socket.close(1000, 'Disposing stale socket');
			}
		} catch (error) {
			this.log.error('Error while disposing socket', error);
		} finally {
			this.socket = null;
		}
	}

	private handleSocketOpen = (event: Event): void => {
		if (!this.isCurrentSocketEvent(event)) return;
		this.log.info('WebSocket connection established');
		this.emitDeferred('connected');
	};
	private handleSocketMessage = async (event: MessageEvent): Promise<void> => {
		try {
			if (!this.isCurrentSocketEvent(event)) return;
			const json = await this.extractPayload(event);
			if (!this.isCurrentSocketEvent(event)) return;
			if (!json) return;
			const payload = parseGatewayPayload(json);
			this.lastGatewayMessageAt = Date.now();
			this.log.debug('Gateway message received', payload);
			if (
				payload.op === GatewayOpcodes.DISPATCH &&
				typeof payload.s === 'number' &&
				payload.s > this.lastSequenceNumber
			) {
				this.lastSequenceNumber = payload.s;
			}
			this.routeGatewayPayload(payload);
			this.emitDeferred('message', payload);
		} catch (error) {
			const fatalError = error instanceof Error ? error : new Error(String(error));
			if (this.handleRecoverableCompressionDecodeError(fatalError)) {
				return;
			}
			this.log.fatal('Fatal gateway decode/parsing error', fatalError);
			this.disconnect(GatewayCloseCodes.DECODE_ERROR, 'Fatal message decode error', false);
			this.emit('fatalError', fatalError);
			throw fatalError;
		}
	};

	private isCurrentSocketEvent(event: Event): boolean {
		return event.target == null || event.target === this.socket;
	}

	private async extractPayload(event: MessageEvent): Promise<string | null> {
		if (event.data instanceof ArrayBuffer) {
			if (!this.payloadDecompressor) {
				throw new Error('Received binary data but no decompressor is configured');
			}
			const chunk = await this.payloadDecompressor.decompress(event.data);
			if (!chunk) {
				this.log.debug('Awaiting additional compressed chunks');
				return null;
			}
			return chunk;
		}
		if (event.data instanceof Blob) {
			return await event.data.text();
		}
		if (typeof event.data === 'string') {
			return event.data;
		}
		throw new Error(`Unsupported gateway payload type: ${typeof event.data}`);
	}

	private handleRecoverableCompressionDecodeError(error: Error): boolean {
		if (!isGatewayCompressionError(error)) {
			return false;
		}
		if (this.compressionFallbackInProgress) {
			this.log.debug('Ignoring compressed gateway message while reconnecting without compression');
			return true;
		}
		if ((this.options.compression ?? 'zstd-stream') === 'none') {
			return false;
		}
		this.compressionFallbackInProgress = true;
		this.options.compression = 'none';
		this.shouldReconnectImmediately = true;
		this.log.warn('Gateway compression decode failed; reconnecting with compression disabled', error);
		if (this.socket) {
			this.socket.removeEventListener('message', this.handleSocketMessage);
		}
		this.disconnect(GatewayCloseCodes.DECODE_ERROR, 'Retrying without compression', true);
		return true;
	}

	private handleSocketClose = (event: CloseEvent): void => {
		if (!this.isCurrentSocketEvent(event)) return;
		this.log.warn(`WebSocket closed [${event.code}] ${event.reason || ''}`);
		this.clearHelloTimeout();
		this.stopHeartbeat();
		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}
		const resumeWasPending = this.resumeTimeoutId != null;
		this.clearResumeTimeout();
		this.emitDeferred('disconnect', {
			code: event.code,
			reason: event.reason,
			wasClean: event.wasClean,
		});
		if (event.code === GatewayCloseCodes.AUTHENTICATION_FAILED) {
			this.handleAuthFailure();
			return;
		}
		if (
			event.code === GatewayCloseCodes.INVALID_SEQ ||
			event.code === GatewayCloseCodes.SESSION_TIMEOUT ||
			resumeWasPending
		) {
			this.log.info(`Resume failed or session invalidated [${event.code}], clearing session to re-identify`);
			this.clearSession();
		}
		if (!this.isUserInitiatedDisconnect) {
			this.handleConnectionFailure();
		} else {
			this.updateState(GatewayState.Disconnected);
		}
	};
	private handleSocketError = (event: Event): void => {
		if (!this.isCurrentSocketEvent(event)) return;
		this.log.error('WebSocket error', event);
		this.emitDeferred('error', event);
		this.handleConnectionFailure();
	};

	private routeGatewayPayload(payload: GatewayPayload): void {
		switch (payload.op) {
			case GatewayOpcodes.DISPATCH:
				this.handleDispatchPayload(payload);
				break;
			case GatewayOpcodes.HEARTBEAT:
				this.log.debug('Heartbeat requested by server');
				this.sendHeartbeat(true);
				break;
			case GatewayOpcodes.HEARTBEAT_ACK:
				this.handleHeartbeatAck();
				break;
			case GatewayOpcodes.HELLO:
				this.handleHelloPayload(payload);
				break;
			case GatewayOpcodes.INVALID_SESSION:
				this.handleInvalidSessionPayload(payload);
				break;
			case GatewayOpcodes.RECONNECT:
				this.log.info('Server requested reconnect');
				this.shouldReconnectImmediately = true;
				this.disconnect(4000, 'Server requested reconnect', true);
				break;
			case GatewayOpcodes.GATEWAY_ERROR: {
				if (!isGatewayErrorData(payload.d)) {
					this.log.warn('Gateway error payload had invalid shape');
					break;
				}
				const errorData = payload.d;
				this.log.warn(`Gateway error received [${errorData.code}] ${errorData.message}`);
				this.emitDeferred('gatewayError', errorData);
				break;
			}
		}
	}

	private handleDispatchPayload(payload: GatewayPayload): void {
		if (!payload.t) return;
		switch (payload.t) {
			case 'READY': {
				const data = payload.d as {
					session_id: string;
				};
				this.activeSessionId = data.session_id;
				this.resetHeartbeatHistory();
				this.resetBackoffInternal();
				this.updateState(GatewayState.Connected);
				const readyMs = this.connectStartedAt > 0 ? Date.now() - this.connectStartedAt : 0;
				this.log.info(`Gateway READY, session=${this.activeSessionId}, ready_ms=${readyMs}`);
				this.logDispatchTimings('READY', payload.d);
				this.emitDeferred('ready', payload.d);
				break;
			}
			case 'RESUMED':
				this.clearResumeTimeout();
				this.resetHeartbeatHistory();
				this.updateState(GatewayState.Connected);
				this.resetBackoffInternal();
				this.log.info('Gateway session resumed');
				this.logDispatchTimings('RESUMED', payload.d);
				this.emitDeferred('resumed', payload.d);
				break;
		}
		this.emitDeferred('dispatch', payload.t, payload.d);
	}

	private logDispatchTimings(eventName: 'READY' | 'RESUMED', data: unknown): void {
		if (!this.log.isLevelEnabled(LogLevel.Debug) || !isRecord(data)) {
			return;
		}
		const timed = data as {_timings?: RpcTimings; _timings_gw?: GatewayTimings};
		const tree = formatGatewayReadyTimings(timed._timings_gw, timed._timings);
		if (tree) {
			this.log.debug(`Gateway ${eventName} timings:\n${tree}`);
		}
	}

	private handleHelloPayload(payload: GatewayPayload): void {
		this.clearHelloTimeout();
		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}
		const helloData = payload.d as {
			heartbeat_interval: number;
		};
		this.startHeartbeat(helloData.heartbeat_interval);
		if (this.canResumeSession()) {
			this.sendResume();
		} else {
			this.sendIdentify();
		}
	}

	private handleInvalidSessionPayload(payload: GatewayPayload): void {
		const isResumable = payload.d as boolean;
		this.log.info(`Session invalidated (resumable=${isResumable})`);
		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}
		const delay = 2500 + Math.random() * 1000;
		this.invalidSessionTimeoutId = window.setTimeout(() => {
			this.invalidSessionTimeoutId = null;
			if (!isResumable) {
				this.clearSession();
			}
			this.shouldReconnectImmediately = true;
			this.disconnect(4000, `Invalid session (resumable=${isResumable})`, true);
		}, delay);
	}

	private sendIdentify(): void {
		this.log.info('Sending IDENTIFY to gateway');
		const flags = this.options.identifyFlags ?? 0;
		this.sendPayload({
			op: GatewayOpcodes.IDENTIFY,
			d: {
				token: this.options.token,
				properties: this.options.properties,
				...(this.options.presence && {presence: this.options.presence}),
				flags,
				...(this.options.initialGuildId ? {initial_guild_id: this.options.initialGuildId} : {}),
			},
		});
	}

	private sendResume(): void {
		if (!this.activeSessionId) {
			this.log.warn('Cannot RESUME without an active session, falling back to IDENTIFY');
			this.sendIdentify();
			return;
		}
		this.log.info(`Sending RESUME for session ${this.activeSessionId}`);
		this.sendPayload({
			op: GatewayOpcodes.RESUME,
			d: {
				token: this.options.token,
				session_id: this.activeSessionId,
				seq: this.lastSequenceNumber,
			},
		});
		this.startResumeTimeout();
	}

	private startHeartbeat(intervalMs: number): void {
		this.stopHeartbeat();
		this.heartbeatIntervalMs = intervalMs;
		const initialDelay = this.computeNextHeartbeatDelay();
		this.scheduleHeartbeat(initialDelay);
		this.log.debug(`Heartbeat scheduled (interval=${intervalMs}ms, next=${initialDelay}ms)`);
	}

	private computeNextHeartbeatDelay(): number {
		if (!this.heartbeatIntervalMs || this.heartbeatIntervalMs <= 0) {
			return 1000;
		}
		const base = Math.max(1000, Math.floor(this.heartbeatIntervalMs * 0.8));
		const jitter = Math.min(1500, Math.floor(this.heartbeatIntervalMs * 0.05));
		return base + Math.floor(Math.random() * (jitter + 1));
	}

	private scheduleHeartbeat(delayMs?: number): void {
		if (!this.heartbeatIntervalMs) return;
		const delay = delayMs ?? this.computeNextHeartbeatDelay();
		if (this.heartbeatTimeoutId != null) {
			clearTimeout(this.heartbeatTimeoutId);
		}
		this.heartbeatTimeoutId = window.setTimeout(() => this.handleHeartbeatTick(), delay);
	}

	private handleHeartbeatTick(): void {
		this.heartbeatTimeoutId = null;
		this.sendHeartbeat();
		if (this.heartbeatIntervalMs) {
			this.scheduleHeartbeat();
		}
	}

	private heartbeatSkipThreshold(): number {
		if (!this.heartbeatIntervalMs || this.heartbeatIntervalMs <= 0) {
			return GATEWAY_TIMEOUTS.HeartbeatAck;
		}
		const derived = Math.floor(this.heartbeatIntervalMs * 0.75);
		return Math.max(500, Math.min(GATEWAY_TIMEOUTS.HeartbeatAck, derived));
	}

	private sendHeartbeat(serverRequested = false): void {
		if (this.awaitingHeartbeatAck && !serverRequested) {
			const now = Date.now();
			const elapsedSinceLastHeartbeat = this.lastHeartbeatSentAt ? now - this.lastHeartbeatSentAt : 0;
			const skipThreshold = this.heartbeatSkipThreshold();
			if (elapsedSinceLastHeartbeat < skipThreshold) {
				const retryDelay = Math.max(500, skipThreshold - elapsedSinceLastHeartbeat);
				this.log.debug(`Deferring heartbeat while awaiting ACK (retry in ${retryDelay}ms)`);
				this.scheduleHeartbeat(retryDelay);
				return;
			}
			if (elapsedSinceLastHeartbeat < GATEWAY_TIMEOUTS.HeartbeatAck) {
				const retryDelay = Math.max(500, GATEWAY_TIMEOUTS.HeartbeatAck - elapsedSinceLastHeartbeat);
				this.log.debug(`Still waiting for heartbeat ACK, delaying retry by ${retryDelay}ms`);
				this.scheduleHeartbeat(retryDelay);
				return;
			}
			this.log.warn('Heartbeat ACK not received, forcing reconnect');
			this.handleHeartbeatFailure();
			return;
		}
		const didSend = this.sendPayload({
			op: GatewayOpcodes.HEARTBEAT,
			d: this.lastSequenceNumber,
		});
		if (!didSend) {
			this.log.error('Failed to transmit heartbeat');
			this.handleHeartbeatFailure();
			return;
		}
		this.awaitingHeartbeatAck = true;
		this.lastHeartbeatSentAt = Date.now();
		this.emitDeferred('heartbeat', this.lastSequenceNumber);
		if (serverRequested && this.heartbeatAckTimeoutId != null) {
			clearTimeout(this.heartbeatAckTimeoutId);
		}
		this.startHeartbeatAckTimeout();
		if (serverRequested && this.heartbeatIntervalMs) {
			this.scheduleHeartbeat();
		}
		this.log.debug(`Heartbeat sent (seq=${this.lastSequenceNumber}${serverRequested ? ', serverRequested' : ''})`);
	}

	private startHeartbeatAckTimeout(): void {
		this.heartbeatAckTimeoutId = window.setTimeout(() => {
			if (!this.awaitingHeartbeatAck) return;
			this.log.warn('Heartbeat ACK timeout');
			this.handleHeartbeatFailure();
		}, GATEWAY_TIMEOUTS.HeartbeatAck);
	}

	private handleHeartbeatAck(): void {
		this.awaitingHeartbeatAck = false;
		this.lastHeartbeatAckAt = Date.now();
		if (this.heartbeatAckTimeoutId != null) {
			clearTimeout(this.heartbeatAckTimeoutId);
			this.heartbeatAckTimeoutId = null;
		}
		this.log.debug('Heartbeat acknowledgment received');
		this.emitDeferred('heartbeatAck');
	}

	private handleHeartbeatFailure(): void {
		this.log.warn('Heartbeat failed, reconnecting');
		this.shouldReconnectImmediately = true;
		this.disconnect(4000, 'Heartbeat ACK timeout', true);
	}

	private stopHeartbeat(): void {
		if (this.heartbeatTimeoutId != null) {
			clearTimeout(this.heartbeatTimeoutId);
			this.heartbeatTimeoutId = null;
		}
		if (this.heartbeatAckTimeoutId != null) {
			clearTimeout(this.heartbeatAckTimeoutId);
			this.heartbeatAckTimeoutId = null;
		}
		this.awaitingHeartbeatAck = false;
		this.heartbeatIntervalMs = null;
		this.log.debug('Heartbeat stopped');
	}

	private resetHeartbeatHistory(): void {
		this.lastHeartbeatAckAt = null;
		this.lastHeartbeatSentAt = null;
	}

	private handleConnectionFailure(): void {
		if (this.isUserInitiatedDisconnect) {
			this.updateState(GatewayState.Disconnected);
			return;
		}
		this.updateState(GatewayState.Reconnecting);
		this.scheduleReconnect();
	}

	private scheduleReconnect(options: {allowImmediate?: boolean} = {}): void {
		if (this.reconnectTimeoutId != null) {
			this.log.debug('Reconnect already scheduled, ignoring');
			return;
		}
		const allowImmediate = options.allowImmediate ?? true;
		const wasImmediate = allowImmediate && this.shouldReconnectImmediately;
		const delay = wasImmediate ? 0 : this.nextReconnectDelay();
		this.shouldReconnectImmediately = false;
		this.log.info(`Scheduling reconnect in ${delay}ms${wasImmediate ? ' (immediate)' : ''}`);
		this.reconnectTimeoutId = window.setTimeout(() => {
			this.reconnectTimeoutId = null;
			if (!this.canResumeSession()) {
				this.log.info('Session no longer resumable, clearing state');
				this.clearSession();
			}
			this.connect();
		}, delay);
	}

	private nextReconnectDelay(): number {
		const now = Date.now();
		const elapsed = now - this.lastReconnectAt;
		if (elapsed < GATEWAY_TIMEOUTS.MinReconnect) {
			this.log.debug(`Last reconnect ${elapsed}ms ago, enforcing minimum delay (${GATEWAY_TIMEOUTS.MinReconnect}ms)`);
			return GATEWAY_TIMEOUTS.MinReconnect;
		}
		this.lastReconnectAt = now;
		const delay = this.reconnectBackoff.next();
		this.log.debug(`Reconnect backoff attempt=${this.reconnectBackoff.getCurrentAttempts()} delay=${delay}ms`);
		return delay;
	}

	private resetBackoffInternal(): void {
		this.reconnectBackoff.reset();
	}

	private canResumeSession(): boolean {
		const now = Date.now();
		if (!this.activeSessionId) return false;
		const lastActivityAt = this.lastGatewayActivityAt();
		if (lastActivityAt != null) {
			return now - lastActivityAt <= GATEWAY_TIMEOUTS.ResumeWindow;
		}
		return true;
	}

	private lastGatewayActivityAt(): number | null {
		const lastHeartbeatAt = this.lastHeartbeatActivityAt();
		if (this.lastGatewayMessageAt == null) return lastHeartbeatAt;
		if (lastHeartbeatAt == null) return this.lastGatewayMessageAt;
		return Math.max(this.lastGatewayMessageAt, lastHeartbeatAt);
	}

	private lastHeartbeatActivityAt(): number | null {
		if (this.lastHeartbeatAckAt == null) return this.lastHeartbeatSentAt;
		if (this.lastHeartbeatSentAt == null) return this.lastHeartbeatAckAt;
		return Math.max(this.lastHeartbeatAckAt, this.lastHeartbeatSentAt);
	}

	private clearSession(): void {
		const hadSession = Boolean(this.activeSessionId);
		this.activeSessionId = null;
		this.lastSequenceNumber = 0;
		this.lastGatewayMessageAt = null;
		this.resetHeartbeatHistory();
		if (hadSession) {
			this.log.info('Gateway session cleared');
		}
	}

	private startHelloTimeout(): void {
		this.clearHelloTimeout();
		this.helloTimeoutId = window.setTimeout(() => {
			this.log.warn('HELLO not received in time');
			this.disconnect(4000, 'Hello timeout', true);
		}, GATEWAY_TIMEOUTS.Hello);
	}

	private clearHelloTimeout(): void {
		if (this.helloTimeoutId != null) {
			clearTimeout(this.helloTimeoutId);
			this.helloTimeoutId = null;
		}
	}

	private startResumeTimeout(): void {
		this.clearResumeTimeout();
		this.resumeTimeoutId = window.setTimeout(() => {
			this.resumeTimeoutId = null;
			this.log.warn('RESUMED not received in time, closing connection');
			this.clearSession();
			this.disconnect(4000, 'Resume response timeout', false);
			this.isUserInitiatedDisconnect = false;
			this.handleConnectionFailure();
		}, GATEWAY_TIMEOUTS.ResumeResponse);
	}

	private clearResumeTimeout(): void {
		if (this.resumeTimeoutId != null) {
			clearTimeout(this.resumeTimeoutId);
			this.resumeTimeoutId = null;
		}
	}

	private async buildGatewayUrl(): Promise<string> {
		const url = new URL(this.gatewayUrlBase);
		url.searchParams.set('v', this.options.apiVersion.toString());
		url.searchParams.set('encoding', 'json');
		const compression: CompressionType = this.options.compression ?? 'zstd-stream';
		url.searchParams.set('compress', compression);
		if (compression === 'zstd-stream') {
			url.searchParams.set('stream', '1');
		}
		const built = url.toString();
		return this.gatewayUrlWrapper ? this.gatewayUrlWrapper(built) : built;
	}

	private sendPayload(payload: GatewayPayload): boolean {
		if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
			this.log.warn('Attempted to send gateway payload while socket is not open');
			return false;
		}
		try {
			const data = JSON.stringify(payload);
			const codec = this.payloadDecompressor;
			if (codec?.canCompress()) {
				let compressed: Uint8Array;
				try {
					compressed = codec.compress(data);
				} catch (error) {
					this.log.error('Gateway outbound compression failed; recovering without compression', error);
					this.handleRecoverableCompressionDecodeError(error instanceof Error ? error : new Error(String(error)));
					return false;
				}
				this.socket.send(new Uint8Array(compressed));
			} else {
				this.socket.send(data);
			}
			this.log.debug('Gateway payload sent', payload);
			return true;
		} catch (error) {
			this.log.error('Error while sending gateway payload', error);
			return false;
		}
	}

	private updateState(nextState: GatewayState): void {
		if (this.connectionState === nextState) return;
		const previous = this.connectionState;
		this.connectionState = nextState;
		this.log.info(`Gateway state ${previous} -> ${nextState}`);
		this.emitDeferred('stateChange', nextState, previous);
	}

	private handleAuthFailure(): void {
		this.log.error('Authentication failed: clearing client state and logging out');
		this.updateState(GatewayState.Disconnected);
		AppStorage.clearExcept(PRESERVED_RESET_STORAGE_KEYS);
		LayerManager.closeAll();
		GatewayConnection.logout();
		Authentication.handleConnectionClosed({code: 4004});
	}
}
