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

import {getPreferredCompression} from '@app/lib/GatewayCompression';
import {type GatewayErrorData, GatewaySocket, type GatewaySocketProperties, GatewayState} from '@app/lib/GatewaySocket';
import {Logger} from '@app/lib/Logger';
import SessionManager from '@app/lib/SessionManager';
import FavoriteMemeStore from '@app/stores/FavoriteMemeStore';
import GeoIPStore from '@app/stores/GeoIPStore';
import GuildNSFWAgreeStore from '@app/stores/GuildNSFWAgreeStore';
import {
	createHandlerRegistry,
	type GatewayHandlerContext,
	type GatewayHandlerRegistry,
} from '@app/stores/gateway/handlers';
import InitializationStore from '@app/stores/InitializationStore';
import LayerManager from '@app/stores/LayerManager';
import LocalPresenceStore from '@app/stores/LocalPresenceStore';
import MemberSearchStore from '@app/stores/MemberSearchStore';
import MessageStore from '@app/stores/MessageStore';
import PermissionStore from '@app/stores/PermissionStore';
import QuickSwitcherStore from '@app/stores/QuickSwitcherStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import RuntimeCrashStore from '@app/stores/RuntimeCrashStore';
import SelectedGuildStore from '@app/stores/SelectedGuildStore';
import TypingStore from '@app/stores/TypingStore';
import MediaEngineStore from '@app/stores/voice/MediaEngineFacade';
import {getGatewayClientProperties} from '@app/utils/ClientInfoUtils';
import {DEFAULT_API_VERSION, FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {GatewayIdentifyFlags} from '@fluxer/constants/src/GatewayConstants';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('GatewayConnectionStore');

interface DesiredSession {
	token: string | null;
	userIdHint: string | null;
}

class GatewayConnectionStore {
	socket: GatewaySocket | null = null;

	isConnected: boolean = false;
	isConnecting: boolean = false;
	isReady: boolean = false;

	sessionId: string | null = null;

	private handlerRegistry: GatewayHandlerRegistry;

	private onlineListener: (() => void) | null = null;
	private offlineListener: (() => void) | null = null;
	private netInfoUnsubscribe: (() => void) | null = null;

	private generation: number = 0;
	private desired: DesiredSession | null = null;

	private pendingGuildSyncId: string | null = null;
	private syncedGuildSessions: Record<string, string | null> = {};
	private initialGuildIdAtIdentify: string | null = null;
	private isFatalCrashInProgress: boolean = false;

	constructor() {
		makeAutoObservable<
			this,
			| 'cleanupSocket'
			| 'handleGatewayDispatch'
			| 'handleFatalGatewaySocketError'
			| 'ensureGuildActiveAndSynced'
			| 'flushPendingGuildSync'
		>(
			this,
			{
				startSession: action.bound,
				logout: action.bound,
				handleConnectionOpen: action.bound,
				handleConnectionResumed: action.bound,
				handleConnectionClosed: action.bound,
				cleanupSocket: action.bound,
				handleGatewayDispatch: action.bound,
				handleFatalGatewaySocketError: action.bound,
				ensureGuildActiveAndSynced: action.bound,
				flushPendingGuildSync: action.bound,
				syncGuildIfNeeded: action.bound,
			},
			{autoBind: true},
		);

		this.handlerRegistry = createHandlerRegistry();

		this.setupPresenceSync();
		this.setupSelectedGuildSync();
	}

	private setupPresenceSync(): void {
		reaction(
			() => LocalPresenceStore.presenceFingerprint,
			() => {
				const presence = LocalPresenceStore.getPresence();
				this.socket?.updatePresence(presence.status, presence.afk, presence.mobile, presence.custom_status);
			},
		);
	}

	private setupSelectedGuildSync(): void {
		reaction(
			() => ({
				guildId: SelectedGuildStore.selectedGuildId,
				nonce: SelectedGuildStore.selectionNonce,
			}),
			({guildId}) => {
				if (!guildId || guildId === FAVORITES_GUILD_ID) {
					this.pendingGuildSyncId = null;
					return;
				}

				this.ensureGuildActiveAndSynced(guildId, {reason: 'select'});
			},
		);

		reaction(
			() => this.isReady,
			(ready) => {
				if (ready) {
					this.flushPendingGuildSync();
				}
			},
		);
	}

	private createHandlerContext(): GatewayHandlerContext {
		return {
			socket: this.socket,
			previousSessionId: null,
			setPreviousSessionId: (_id: string) => {},
			setReady: () => {
				runInAction(() => {
					this.isReady = true;
					this.isConnecting = false;
				});

				this.flushPendingGuildSync();
			},
		};
	}

	private removeNetworkListeners(): void {
		if (this.netInfoUnsubscribe) {
			this.netInfoUnsubscribe();
			this.netInfoUnsubscribe = null;
		}

		if (this.onlineListener) {
			window.removeEventListener('online', this.onlineListener);
			this.onlineListener = null;
		}

		if (this.offlineListener) {
			window.removeEventListener('offline', this.offlineListener);
			this.offlineListener = null;
		}
	}

	private cleanupSocket(): void {
		const socket = this.socket;
		this.socket = null;

		if (socket) {
			try {
				socket.disconnect(1000, 'Cleaning up socket', false);
			} catch (err) {
				logger.warn('Error while disconnecting socket during cleanup', err);
			}
		}

		this.removeNetworkListeners();
	}

	private createGatewaySocket(token: string, properties: GatewaySocketProperties, generation: number): GatewaySocket {
		const gatewayUrl = RuntimeConfigStore.gatewayEndpoint;
		const presence = LocalPresenceStore.getPresence();
		const compression = getPreferredCompression();

		logger.info(`Using gateway compression: ${compression}`);

		let identifyFlags = 0;
		identifyFlags |= GatewayIdentifyFlags.DEBOUNCE_MESSAGE_REACTIONS;
		const initialGuildId = SelectedGuildStore.selectedGuildId ?? null;
		this.initialGuildIdAtIdentify = initialGuildId;

		const socket = new GatewaySocket(gatewayUrl, {
			apiVersion: DEFAULT_API_VERSION,
			token,
			properties,
			presence: {
				status: presence.status,
				afk: presence.afk,
				mobile: presence.mobile,
				custom_status: presence.custom_status,
			},
			compression,
			identifyFlags,
			initialGuildId,
		});

		const isCurrent = (): boolean => this.socket === socket && this.generation === generation;

		socket.on('dispatch', (eventType: string, data: unknown) => {
			if (!isCurrent()) {
				return;
			}
			this.handleGatewayDispatch(eventType, data);
		});

		socket.on('gatewayError', (error: GatewayErrorData) => {
			if (!isCurrent()) {
				return;
			}
			this.handleGatewayError(error);
		});

		socket.on('fatalError', (error: Error) => {
			if (!isCurrent()) {
				return;
			}
			this.handleFatalGatewaySocketError(error);
		});

		this.removeNetworkListeners();

		this.onlineListener = () => {
			if (!isCurrent()) {
				return;
			}
			socket.handleNetworkStatusChange(true);
		};
		this.offlineListener = () => {
			if (!isCurrent()) {
				return;
			}
			socket.handleNetworkStatusChange(false);
		};
		window.addEventListener('online', this.onlineListener);
		window.addEventListener('offline', this.offlineListener);

		socket.on(
			'stateChange',
			action((newState: GatewayState) => {
				if (!isCurrent()) {
					return;
				}

				this.isConnected = newState === GatewayState.Connected;
				this.isConnecting = newState === GatewayState.Connecting || newState === GatewayState.Reconnecting;

				if (newState === GatewayState.Disconnected) {
					this.isReady = false;
					SessionManager.handleConnectionFailed();

					this.syncedGuildSessions = {};
				}
			}),
		);

		socket.on(
			'ready',
			action((data: unknown) => {
				if (!isCurrent()) {
					return;
				}
				const readyData = data as {session_id: string};
				this.handleConnectionOpen(readyData.session_id);
			}),
		);

		socket.on(
			'resumed',
			action(() => {
				if (!isCurrent()) {
					return;
				}
				this.handleConnectionResumed();
			}),
		);

		return socket;
	}

	private ensureGuildActiveAndSynced(guildId: string, options: {force?: boolean; reason?: string} = {}): void {
		if (!guildId || guildId === FAVORITES_GUILD_ID) {
			return;
		}

		const socket = this.socket;

		if (!socket || !this.isReady) {
			this.pendingGuildSyncId = guildId;
			return;
		}

		const sessionId = this.sessionId ?? null;
		const force = options.force ?? false;
		const alreadySyncedSession = this.syncedGuildSessions[guildId] ?? null;

		if (!force && alreadySyncedSession === sessionId) {
			return;
		}

		try {
			socket.updateGuildSubscriptions({
				subscriptions: {
					[guildId]: {
						active: true,
						sync: true,
					},
				},
			});

			this.syncedGuildSessions[guildId] = sessionId;
			this.pendingGuildSyncId = null;
		} catch (err) {
			logger.warn('Failed to update guild subscriptions; will retry when possible', err);
			this.pendingGuildSyncId = guildId;
		}
	}

	syncGuildIfNeeded(guildId: string, reason?: string): void {
		this.ensureGuildActiveAndSynced(guildId, {reason});
	}

	private flushPendingGuildSync(): void {
		const guildId = this.pendingGuildSyncId ?? SelectedGuildStore.selectedGuildId;
		if (!guildId || guildId === FAVORITES_GUILD_ID) {
			return;
		}

		this.ensureGuildActiveAndSynced(guildId, {reason: 'flush'});
	}

	async startSession(token?: string): Promise<void> {
		const userIdHint = SessionManager.userId ?? null;

		const desired: DesiredSession = {
			token: token ?? null,
			userIdHint,
		};

		if (this.isConnecting && this.desired) {
			const sameToken = this.desired.token === desired.token;
			const sameUser = this.desired.userIdHint === desired.userIdHint;

			if (sameToken && sameUser) {
				return;
			}
		}

		this.desired = desired;

		const generation = ++this.generation;

		runInAction(() => {
			this.isConnecting = true;
			this.isReady = false;
		});

		SessionManager.handleConnectionStarted();

		if (this.socket) {
			this.cleanupSocket();
		}

		InitializationStore.setConnecting();

		let gatewayToken: string | null = desired.token;

		try {
			if (!gatewayToken) {
				const stored = SessionManager.token;
				if (!stored) {
					runInAction(() => {
						this.isConnecting = false;
						this.isReady = false;
					});
					SessionManager.handleConnectionFailed();
					return;
				}
				gatewayToken = stored;
			}

			if (this.generation !== generation) {
				return;
			}

			let properties: GatewaySocketProperties;
			try {
				properties = await getGatewayClientProperties({
					latitude: GeoIPStore.latitude,
					longitude: GeoIPStore.longitude,
				});
			} catch (err) {
				logger.error('Failed to gather client metadata for gateway identification', err);
				runInAction(() => {
					this.isConnecting = false;
					this.isReady = false;
				});
				SessionManager.handleConnectionFailed();
				return;
			}

			if (this.generation !== generation) {
				return;
			}

			const socket = this.createGatewaySocket(gatewayToken, properties, generation);

			runInAction(() => {
				if (this.generation === generation) {
					this.socket = socket;
				}
			});

			if (this.socket) {
				this.socket.connect();
			}
		} catch (err) {
			logger.error('Failed to connect to gateway', err);
			runInAction(() => {
				this.isConnecting = false;
				this.isReady = false;
			});
			SessionManager.handleConnectionFailed();
		}
	}

	logout(): void {
		this.cleanupSocket();

		MessageStore.handleSessionInvalidated();
		FavoriteMemeStore.reset();
		GuildNSFWAgreeStore.reset();
		InitializationStore.reset();
		MemberSearchStore.handleLogout();

		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;

		this.sessionId = null;
		this.desired = null;

		this.pendingGuildSyncId = null;
		this.syncedGuildSessions = {};
	}

	handleConnectionOpen(sessionId: string): void {
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = true;
		this.sessionId = sessionId;
		this.markInitialGuildSynced(sessionId);

		SessionManager.handleConnectionReady();

		LocalPresenceStore.updatePresence();
		TypingStore.reset();
		QuickSwitcherStore.recomputeIfOpen();

		this.flushPendingGuildSync();
	}

	handleConnectionResumed(): void {
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = true;

		SessionManager.handleConnectionReady();
		this.markInitialGuildSynced(this.sessionId);

		LocalPresenceStore.updatePresence();
		TypingStore.reset();
		QuickSwitcherStore.recomputeIfOpen();

		this.flushPendingGuildSync();
	}

	handleConnectionClosed(code: number): void {
		SessionManager.handleConnectionClosed(code);

		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;

		LocalPresenceStore.updatePresence();
		PermissionStore.handleConnectionClose();

		this.syncedGuildSessions = {};

		if (code === 4004) {
			LayerManager.closeAll();
			MessageStore.handleConnectionClosed();
			this.cleanupSocket();
			this.sessionId = null;
		}
	}

	private markInitialGuildSynced(sessionId: string | null): void {
		const guildId = this.initialGuildIdAtIdentify;
		if (!guildId || !sessionId) {
			return;
		}

		this.syncedGuildSessions[guildId] = sessionId;
	}

	private handleGatewayError(error: GatewayErrorData): void {
		logger.warn(`Gateway error: [${error.code}] ${error.message}`);
		MediaEngineStore.handleGatewayError(error);
	}

	private handleFatalGatewaySocketError(error: Error): void {
		if (this.isFatalCrashInProgress) {
			return;
		}

		this.isFatalCrashInProgress = true;
		logger.fatal('Fatal gateway parsing failure, forcing crash cleanup', error);

		try {
			this.logout();
			LayerManager.closeAll();
			MediaEngineStore.cleanup();
		} catch (cleanupError) {
			logger.error('Failed to complete fatal crash cleanup', cleanupError);
		}

		RuntimeCrashStore.triggerFatalCrash(error);
	}

	private handleGatewayDispatch(eventType: string, data: unknown): void {
		const handler = this.handlerRegistry.get(eventType);
		if (!handler) {
			return;
		}
		const context = this.createHandlerContext();
		handler(data, context);
	}
}

export default new GatewayConnectionStore();
