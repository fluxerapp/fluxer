/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import EventEmitter from 'eventemitter3';
import type {GatewayErrorCode} from '~/Constants';
import {GatewayCloseCodes, GatewayOpcodes} from '~/Constants';
import AppStorage from '~/lib/AppStorage';
import type {GatewayCustomStatusPayload} from '~/lib/customStatus';
import {ExponentialBackoff} from '~/lib/ExponentialBackoff';
import {type CompressionType, GatewayDecompressor} from '~/lib/GatewayCompression';
import {Logger} from '~/lib/Logger';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';
import GeoIPStore from '~/stores/GeoIPStore';
import LayerManager from '~/stores/LayerManager';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

const GATEWAY_TIMEOUTS = {
	HeartbeatAck: 15000,
	ResumeWindow: 180000,
	MinReconnect: 1000,
	MaxReconnect: 10000,
	Hello: 20000,
} as const;

export const GatewayState = {
	Disconnected: 'DISCONNECTED',
	Connecting: 'CONNECTING',
	Connected: 'CONNECTED',
	Reconnecting: 'RECONNECTING',
} as const;
export type GatewayState = (typeof GatewayState)[keyof typeof GatewayState];

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
	build_timestamp: string;
	desktop_app_version?: string | null;
	desktop_app_channel?: string | null;
	desktop_arch?: string | null;
	desktop_os?: string | null;
	latitude?: string;
	longitude?: string;
}

export interface GatewayPresence {
	status: string;
	afk: boolean;
	mobile: boolean;
	custom_status?: GatewayCustomStatusPayload | null;
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

export interface GatewayErrorData {
	code: GatewayErrorCode;
	message: string;
}

export interface GatewaySocketEvents {
	connecting: () => void;
	connected: () => void;
	ready: (data: unknown) => void;
	resumed: () => void;
	disconnect: (event: {code: number; reason: string; wasClean: boolean}) => void;
	error: (error: Error | Event | CloseEvent) => void;
	gatewayError: (error: GatewayErrorData) => void;
	message: (payload: GatewayPayload) => void;
	dispatch: (type: string, data: unknown) => void;
	stateChange: (newState: GatewayState, oldState: GatewayState) => void;
	heartbeat: (sequence: number) => void;
	heartbeatAck: () => void;
	networkStatusChange: (online: boolean) => void;
}

export class GatewaySocket extends EventEmitter<GatewaySocketEvents> {
	private readonly log: Logger;
	private readonly reconnectBackoff: ExponentialBackoff;

	private socket: WebSocket | null = null;
	private connectionState: GatewayState = GatewayState.Disconnected;

	private activeSessionId: string | null = null;
	private lastSequenceNumber = 0;
	private lastReconnectAt = 0;

	private heartbeatIntervalMs: number | null = null;
	private heartbeatTimeoutId: number | null = null;
	private heartbeatAckTimeoutId: number | null = null;
	private awaitingHeartbeatAck = false;
	private lastHeartbeatAckAt: number | null = null;
	private lastHeartbeatSentAt: number | null = null;

	private helloTimeoutId: number | null = null;
	private reconnectTimeoutId: number | null = null;
	private invalidSessionTimeoutId: number | null = null;
	private isUserInitiatedDisconnect = false;
	private shouldReconnectImmediately = false;
	private payloadDecompressor: GatewayDecompressor | null = null;

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

	connect(): void {
		if (this.connectionState === GatewayState.Connecting || this.connectionState === GatewayState.Connected) {
			this.log.debug('Ignoring connect: already connecting or connected');
			return;
		}

		this.isUserInitiatedDisconnect = false;
		this.updateState(GatewayState.Connecting);
		this.openSocket();
	}

	disconnect(code = 1000, reason = 'Client disconnecting', resumable = false): void {
		this.log.info(`Disconnect requested: [${code}] ${reason}, resumable=${resumable}`);
		this.isUserInitiatedDisconnect = !resumable;

		this.clearHelloTimeout();
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
		this.emit('networkStatusChange', online);

		if (online) {
			if (this.connectionState === GatewayState.Disconnected || this.connectionState === GatewayState.Reconnecting) {
				this.shouldReconnectImmediately = true;
				this.connect();
			}
		} else if (this.connectionState === GatewayState.Connected) {
			this.disconnect(1000, 'Network offline', true);
		}
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

	updateVoiceState(params: {
		guild_id: string | null;
		channel_id: string | null;
		self_mute: boolean;
		self_deaf: boolean;
		self_video: boolean;
		self_stream: boolean;
		viewer_stream_key?: string | null;
		connection_id: string | null;
	}): void {
		const isMobileLayout = MobileLayoutStore.isMobileLayout();
		const {latitude, longitude} = GeoIPStore;

		this.sendPayload({
			op: GatewayOpcodes.VOICE_STATE_UPDATE,
			d: {
				...params,
				connection_id: params.connection_id || MediaEngineStore.connectionId,
				is_mobile: isMobileLayout,
				latitude: latitude ?? undefined,
				longitude: longitude ?? undefined,
			},
		});
	}

	requestGuildMembers(params: {
		guildId: string;
		query?: string;
		limit?: number;
		userIds?: Array<string>;
		presences?: boolean;
		nonce?: string;
	}): void {
		if (!this.isConnected()) return;

		this.sendPayload({
			op: GatewayOpcodes.REQUEST_GUILD_MEMBERS,
			d: {
				guild_id: params.guildId,
				...(params.query !== undefined && {query: params.query}),
				...(params.limit !== undefined && {limit: params.limit}),
				...(params.userIds !== undefined && {user_ids: [...new Set(params.userIds)]}),
				...(params.presences !== undefined && {presences: params.presences}),
				...(params.nonce !== undefined && {nonce: params.nonce}),
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

		const url = this.buildGatewayUrl();
		this.log.debug(`Opening WebSocket connection to ${url}`);

		try {
			this.socket = new WebSocket(url);

			const compression: CompressionType = this.options.compression ?? 'zstd-stream';
			if (compression !== 'none') {
				this.socket.binaryType = 'arraybuffer';
				this.payloadDecompressor = new GatewayDecompressor(compression);
			} else {
				this.socket.binaryType = 'blob';
				this.payloadDecompressor = null;
			}

			this.socket.addEventListener('open', this.handleSocketOpen);
			this.socket.addEventListener('message', this.handleSocketMessage);
			this.socket.addEventListener('close', this.handleSocketClose);
			this.socket.addEventListener('error', this.handleSocketError);

			this.startHelloTimeout();
			this.emit('connecting');
		} catch (error) {
			this.log.error('Failed to create WebSocket', error);
			this.handleConnectionFailure();
		}
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

	private handleSocketOpen = (): void => {
		this.log.info('WebSocket connection established');
		this.emit('connected');
	};

	private handleSocketMessage = async (event: MessageEvent): Promise<void> => {
		try {
			const json = await this.extractPayload(event);
			if (!json) return;

			const payload = JSON.parse(json) as GatewayPayload;

			this.log.debug('Gateway message received', payload);

			if (
				this.connectionState === GatewayState.Connected &&
				payload.op === GatewayOpcodes.DISPATCH &&
				typeof payload.s === 'number' &&
				payload.s > this.lastSequenceNumber
			) {
				this.lastSequenceNumber = payload.s;
			}

			this.routeGatewayPayload(payload);
			this.emit('message', payload);
		} catch (error) {
			this.log.error('Error while handling gateway message', error);

			if (this.options.compression && this.options.compression !== 'none') {
				this.log.warn(`Decompression failed (compression=${this.options.compression}), retrying without compression`);
				this.options.compression = 'none';

				if (this.payloadDecompressor) {
					this.payloadDecompressor.destroy();
					this.payloadDecompressor = null;
				}

				this.shouldReconnectImmediately = true;
				this.disconnect(GatewayCloseCodes.DECODE_ERROR, 'Message decode error', true);
				return;
			}

			this.disconnect(GatewayCloseCodes.DECODE_ERROR, 'Message decode error');
		}
	};

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

		return event.data;
	}

	private handleSocketClose = (event: CloseEvent): void => {
		this.log.warn(`WebSocket closed [${event.code}] ${event.reason || ''}`);
		this.clearHelloTimeout();
		this.stopHeartbeat();

		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}

		const compressionChanged = this.maybeAdjustCompression(event);
		if (compressionChanged) {
			this.shouldReconnectImmediately = true;
			this.resetBackoffInternal();
		}

		this.emit('disconnect', {
			code: event.code,
			reason: event.reason,
			wasClean: event.wasClean,
		});

		if (event.code === GatewayCloseCodes.AUTHENTICATION_FAILED) {
			this.handleAuthFailure();
			return;
		}

		if (!this.isUserInitiatedDisconnect) {
			this.handleConnectionFailure();
		} else {
			this.updateState(GatewayState.Disconnected);
		}
	};

	private handleSocketError = (event: Event): void => {
		this.log.error('WebSocket error', event);
		this.emit('error', event);

		if (this.connectionState !== GatewayState.Reconnecting) {
			this.handleConnectionFailure();
		}
	};

	private maybeAdjustCompression(event: CloseEvent): boolean {
		if (event.code !== GatewayCloseCodes.DECODE_ERROR) return false;
		const normalizedReason = (event.reason || '').toLowerCase();
		if (!normalizedReason.includes('encode failed') && !normalizedReason.includes('compression failed')) return false;

		const currentCompression = this.options.compression ?? 'none';
		if (currentCompression === 'zstd-stream') {
			this.log.warn('Disabling gateway compression due to encode failure');
			this.options.compression = 'none';
			return true;
		}

		return false;
	}

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
				const errorData = payload.d as GatewayErrorData;
				this.log.warn(`Gateway error received [${errorData.code}] ${errorData.message}`);
				this.emit('gatewayError', errorData);
				break;
			}
		}
	}

	private handleDispatchPayload(payload: GatewayPayload): void {
		if (!payload.t) return;

		switch (payload.t) {
			case 'READY': {
				const data = payload.d as {session_id: string};
				this.activeSessionId = data.session_id;
				this.resetBackoffInternal();
				this.updateState(GatewayState.Connected);
				this.log.info(`Gateway READY, session=${this.activeSessionId}`);
				this.emit('ready', payload.d);
				break;
			}

			case 'RESUMED':
				this.updateState(GatewayState.Connected);
				this.resetBackoffInternal();
				this.log.info('Gateway session resumed');
				this.emit('resumed');
				break;
		}

		this.emit('dispatch', payload.t, payload.d);
	}

	private handleHelloPayload(payload: GatewayPayload): void {
		this.clearHelloTimeout();

		if (this.invalidSessionTimeoutId != null) {
			clearTimeout(this.invalidSessionTimeoutId);
			this.invalidSessionTimeoutId = null;
		}

		const helloData = payload.d as {heartbeat_interval: number};
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

		if (isResumable) {
			this.invalidSessionTimeoutId = window.setTimeout(() => {
				this.invalidSessionTimeoutId = null;
				this.sendResume();
			}, delay);
		} else {
			this.clearSession();
			this.invalidSessionTimeoutId = window.setTimeout(() => {
				this.invalidSessionTimeoutId = null;
				this.sendIdentify();
			}, delay);
		}
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
		this.emit('heartbeat', this.lastSequenceNumber);

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
		this.emit('heartbeatAck');
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

	private handleConnectionFailure(): void {
		if (this.isUserInitiatedDisconnect) {
			this.updateState(GatewayState.Disconnected);
			return;
		}

		this.updateState(GatewayState.Reconnecting);
		this.scheduleReconnect();
	}

	private scheduleReconnect(): void {
		if (this.reconnectTimeoutId != null) {
			this.log.debug('Reconnect already scheduled, ignoring');
			return;
		}

		const delay = this.shouldReconnectImmediately ? 0 : this.nextReconnectDelay();
		const wasImmediate = this.shouldReconnectImmediately;
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

		if (this.lastHeartbeatAckAt != null) {
			return now - this.lastHeartbeatAckAt <= GATEWAY_TIMEOUTS.ResumeWindow;
		}

		if (this.lastHeartbeatSentAt != null) {
			return now - this.lastHeartbeatSentAt <= GATEWAY_TIMEOUTS.ResumeWindow;
		}

		return true;
	}

	private clearSession(): void {
		const hadSession = Boolean(this.activeSessionId);
		this.activeSessionId = null;
		this.lastSequenceNumber = 0;

		if (hadSession) {
			this.log.info('Gateway session cleared');
		}
	}

	private startHelloTimeout(): void {
		this.clearHelloTimeout();

		this.helloTimeoutId = window.setTimeout(() => {
			this.log.warn('HELLO not received in time');
			this.disconnect(4000, 'Hello timeout');
		}, GATEWAY_TIMEOUTS.Hello);
	}

	private clearHelloTimeout(): void {
		if (this.helloTimeoutId != null) {
			clearTimeout(this.helloTimeoutId);
			this.helloTimeoutId = null;
		}
	}

	private buildGatewayUrl(): string {
		const url = new URL(this.gatewayUrlBase);
		url.searchParams.set('v', this.options.apiVersion.toString());
		url.searchParams.set('encoding', 'json');
		const compression: CompressionType = this.options.compression ?? 'zstd-stream';
		url.searchParams.set('compress', compression);
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
			this.socket.send(data);
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
		this.emit('stateChange', nextState, previous);
	}

	private handleAuthFailure(): void {
		this.log.error('Authentication failed: clearing client state and logging out');
		this.updateState(GatewayState.Disconnected);

		AppStorage.clear();
		LayerManager.closeAll();
		ConnectionStore.logout();
		AuthenticationStore.handleConnectionClosed({code: 4004});
	}
}
