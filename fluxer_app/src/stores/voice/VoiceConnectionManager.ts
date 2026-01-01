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

import type {Room} from 'livekit-client';
import {Room as LiveKitRoom, RoomEvent} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import type {Subscription} from 'rxjs';
import {timer} from 'rxjs';
import {Logger} from '~/lib/Logger';
import {VoiceConnectionThrottle} from './VoiceConnectionThrottle';
import {VoiceReconnectManager} from './VoiceReconnectManager';

const logger = new Logger('VoiceConnectionManager');

const VOICE_SERVER_TIMEOUT_MS = 5000;

export interface VoiceServerUpdateData {
	token: string;
	endpoint: string;
	connection_id: string;
	guild_id?: string;
	channel_id?: string;
}

export interface VoiceConnectionState {
	room: Room | null;
	guildId: string | null;
	channelId: string | null;
	connecting: boolean;
	connected: boolean;
	reconnecting: boolean;
	voiceServerEndpoint: string | null;
	connectionId: string | null;
}

const initialConnectionState: VoiceConnectionState = {
	room: null,
	guildId: null,
	channelId: null,
	connecting: false,
	connected: false,
	reconnecting: false,
	voiceServerEndpoint: null,
	connectionId: null,
};

class VoiceConnectionManager {
	connectionState: VoiceConnectionState = initialConnectionState;
	private throttle = new VoiceConnectionThrottle();
	private reconnect = new VoiceReconnectManager();
	private voiceServerTimeoutSub: Subscription | null = null;
	private isLocalDisconnecting = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get room(): Room | null {
		return this.connectionState.room;
	}

	get guildId(): string | null {
		return this.connectionState.guildId;
	}

	get channelId(): string | null {
		return this.connectionState.channelId;
	}

	get connected(): boolean {
		return this.connectionState.connected;
	}

	get connecting(): boolean {
		return this.connectionState.connecting;
	}

	get reconnecting(): boolean {
		return this.connectionState.reconnecting;
	}

	get connectionId(): string | null {
		return this.connectionState.connectionId;
	}

	get voiceServerEndpoint(): string | null {
		return this.connectionState.voiceServerEndpoint;
	}

	get shouldAutoReconnect(): boolean {
		return this.reconnect.shouldAutoReconnect;
	}

	get reconnectAttempts(): number {
		return this.reconnect.reconnectAttempts;
	}

	get disconnecting(): boolean {
		return this.isLocalDisconnecting;
	}

	get lastConnectedChannel(): {guildId: string; channelId: string} | null {
		return this.reconnect.lastConnectedChannel;
	}

	startConnection(guildId: string | null, channelId: string): void {
		if (this.throttle.shouldThrottle()) {
			logger.warn('Connection throttled');
			return;
		}

		this.throttle.recordConnectRequest();
		this.throttle.incrementAttemptId();

		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				channelId,
				guildId,
				connecting: true,
				connected: false,
				reconnecting: false,
				connectionId: this.connectionState.connectionId,
			};
		});

		this.throttle.setInFlightConnect(true);
		this.scheduleVoiceServerTimeout(guildId, channelId);
		this.reconnect.setLastConnectedChannel(guildId, channelId);

		logger.info('Connection started', {guildId, channelId});
	}

	handleVoiceServerUpdate(
		raw: VoiceServerUpdateData,
		onRoomCreated: (room: Room, attemptId: number, guildId: string | null, channelId: string) => void,
	): void {
		const guildId = raw.guild_id ?? null;
		const endpoint = raw.endpoint ?? null;
		const token = raw.token ?? null;
		const connectionId = raw.connection_id ?? null;

		const {guildId: expectedGuildId, channelId, connected, room: existingRoom} = this.connectionState;
		const attemptId = this.throttle.connectAttemptId;

		logger.debug('handleVoiceServerUpdate called', {
			incomingGuildId: guildId,
			expectedGuildId,
			channelId,
			endpoint,
			hasToken: !!token,
			connectionId,
			attemptId,
		});

		if (expectedGuildId !== guildId || channelId == null) {
			logger.warn('Ignoring VOICE_SERVER_UPDATE: guild or channel mismatch', {
				expectedGuildId,
				incomingGuildId: guildId,
				channelId,
			});
			return;
		}

		if (!this.throttle.isLatestAttempt(attemptId)) {
			logger.warn('Ignoring VOICE_SERVER_UPDATE: not latest attempt', {attemptId});
			return;
		}

		this.clearVoiceServerTimeout();

		if (connected && existingRoom) {
			existingRoom.removeAllListeners();
			if (existingRoom.state === 'connected') {
				existingRoom.disconnect();
			}
		}

		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				connecting: true,
				connected: false,
				reconnecting: false,
				guildId,
				voiceServerEndpoint: endpoint,
				connectionId: connectionId ?? this.connectionState.connectionId,
			};
		});

		this.throttle.setInFlightConnect(true);

		const room = new LiveKitRoom({adaptiveStream: true, dynacast: true});

		onRoomCreated(room, attemptId, guildId, channelId);

		if (!endpoint || !token) {
			logger.error('Missing endpoint or token', {endpoint, hasToken: !!token});
			runInAction(() => {
				this.connectionState = {
					...this.connectionState,
					connecting: false,
					reconnecting: false,
				};
			});
			this.throttle.setInFlightConnect(false);
			return;
		}

		logger.info('Attempting to connect to LiveKit', {endpoint, guildId, channelId});

		room
			.connect(endpoint, token, {autoSubscribe: false})
			.then(() => {
				logger.info('LiveKit connection succeeded');
				if (!this.throttle.isLatestAttempt(attemptId)) {
					logger.warn('Connection succeeded but attempt is stale, disconnecting');
					try {
						room.removeAllListeners();
						room.disconnect();
					} catch {}
					return;
				}
				logger.info('Initializing voice connection');
				runInAction(() => {
					this.connectionState = {
						...this.connectionState,
						room,
					};
				});
			})
			.catch((error) => {
				logger.error('LiveKit connection failed', {error, endpoint});
				if (this.throttle.isLatestAttempt(attemptId)) {
					runInAction(() => {
						this.connectionState = {
							...this.connectionState,
							connecting: false,
							reconnecting: false,
						};
					});
					this.throttle.setInFlightConnect(false);
					this.reconnect.setReconnectState('error');
				}
			});
	}

	markConnected(): void {
		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				connected: true,
				connecting: false,
				reconnecting: false,
			};
		});
		this.throttle.setInFlightConnect(false);
		this.reconnect.resetOnConnection();
		logger.info('Connection established');
	}

	markDisconnected(reason: 'user' | 'error' | 'server' = 'user'): void {
		runInAction(() => {
			this.connectionState = {
				...initialConnectionState,
				connectionId: reason === 'user' ? null : this.connectionState.connectionId,
			};
		});
		this.throttle.setInFlightConnect(false);
		this.reconnect.setReconnectState(reason);
		logger.info('Connection terminated', {reason});
	}

	markReconnecting(): void {
		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				connecting: true,
				connected: false,
				reconnecting: true,
			};
		});
		logger.info('Connection reconnecting');
	}

	markReconnected(): void {
		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				connecting: false,
				connected: true,
				reconnecting: false,
			};
		});
		this.reconnect.resetOnConnection();
		logger.info('Connection reconnected');
	}

	disconnectFromVoiceChannel(reason: 'user' | 'error' | 'server' = 'user'): void {
		const {room} = this.connectionState;

		this.isLocalDisconnecting = reason === 'user';

		this.clearVoiceServerTimeout();

		if (room) {
			room.removeAllListeners();
			room.disconnect();
		}

		runInAction(() => {
			this.connectionState = {
				...initialConnectionState,
				connectionId: reason === 'user' ? null : this.connectionState.connectionId,
			};
		});

		this.reconnect.setReconnectState(reason);

		this.isLocalDisconnecting = false;
		logger.info('Disconnected from voice channel', {reason});
	}

	scheduleReconnect(callback: () => void): boolean {
		return this.reconnect.scheduleReconnect(callback);
	}

	markReconnectionAttempted(): void {
		this.reconnect.markAttempted();
	}

	resetReconnectState(): void {
		this.reconnect.reset();
	}

	updateChannelId(channelId: string): void {
		runInAction(() => {
			this.connectionState = {
				...this.connectionState,
				channelId,
				connected: false,
				connecting: true,
				reconnecting: false,
				room: null,
			};
		});
		logger.info('Channel updated', {channelId});
	}

	createGuardedHandler<T extends ReadonlyArray<unknown>>(
		attemptId: number,
		handler: (...args: T) => void,
	): (...args: T) => void {
		return (...args: T) => {
			if (!this.throttle.isLatestAttempt(attemptId)) {
				return;
			}
			handler(...args);
		};
	}

	bindConnectionEvents(
		room: Room,
		attemptId: number,
		handlers: {
			onConnected: () => void;
			onDisconnected: (reason?: unknown) => void;
			onReconnecting: () => void;
			onReconnected: () => void;
		},
	): void {
		room.on(RoomEvent.Connected, this.createGuardedHandler(attemptId, handlers.onConnected));
		room.on(RoomEvent.Disconnected, this.createGuardedHandler(attemptId, handlers.onDisconnected));
		room.on(RoomEvent.Reconnecting, this.createGuardedHandler(attemptId, handlers.onReconnecting));
		room.on(RoomEvent.Reconnected, this.createGuardedHandler(attemptId, handlers.onReconnected));
	}

	resetConnectionState(): void {
		runInAction(() => {
			this.connectionState = initialConnectionState;
		});
		this.isLocalDisconnecting = false;
		this.throttle.setInFlightConnect(false);
	}

	clearInFlightConnect(): void {
		this.throttle.setInFlightConnect(false);
	}

	abortConnection(): void {
		this.clearVoiceServerTimeout();

		runInAction(() => {
			this.connectionState = {
				...initialConnectionState,
				connectionId: null,
			};
		});

		this.isLocalDisconnecting = false;
		this.throttle.setInFlightConnect(false);
		logger.info('Connection aborted due to gateway error');
	}

	private scheduleVoiceServerTimeout(guildId: string | null, channelId: string): void {
		this.clearVoiceServerTimeout();
		this.voiceServerTimeoutSub = timer(VOICE_SERVER_TIMEOUT_MS).subscribe(() => {
			runInAction(() => {
				if (
					this.connectionState.guildId === guildId &&
					this.connectionState.channelId === channelId &&
					!this.connectionState.connected
				) {
					logger.warn('Voice server timeout', {guildId, channelId});
					this.connectionState = {
						...this.connectionState,
						connecting: false,
						connected: false,
						reconnecting: false,
						guildId: null,
						channelId: null,
					};
					this.throttle.setInFlightConnect(false);
					this.reconnect.setReconnectState('error');
				}
			});
		});
	}

	private clearVoiceServerTimeout(): void {
		this.voiceServerTimeoutSub?.unsubscribe();
		this.voiceServerTimeoutSub = null;
	}

	cleanup(): void {
		const {room} = this.connectionState;

		this.clearVoiceServerTimeout();

		if (room) {
			room.removeAllListeners();
			room.disconnect();
		}

		runInAction(() => {
			this.connectionState = initialConnectionState;
		});

		this.isLocalDisconnecting = false;
		this.throttle.reset();
		this.reconnect.cleanup();

		logger.info('Cleanup complete');
	}
}

export default new VoiceConnectionManager();
