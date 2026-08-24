// SPDX-License-Identifier: AGPL-3.0-or-later

import GeoIP from '@app/features/app/state/GeoIP';
import Initialization from '@app/features/app/state/Initialization';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {
	createHandlerRegistry,
	type GatewayGeoipPayload,
	type GatewayHandlerContext,
	type GatewayHandlerRegistry,
} from '@app/features/gateway/events/EventRouter';
import {getPreferredCompression} from '@app/features/gateway/transport/GatewayCompression';
import {
	type GatewaySessionRetirementReason,
	sendInvisiblePresenceForLocalSession,
} from '@app/features/gateway/transport/GatewaySessionRetirement';
import {
	type GatewayErrorData,
	GatewaySocket,
	type GatewaySocketProperties,
	GatewayState,
	type GatewayVoiceStateUpdateParams,
} from '@app/features/gateway/transport/GatewaySocket';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Permission from '@app/features/permissions/state/Permission';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getGatewayClientProperties} from '@app/features/platform/utils/ClientInfo';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import Presence from '@app/features/presence/state/Presence';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import LayerManager from '@app/features/ui/state/LayerManager';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {DEFAULT_API_VERSION, FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {GatewayIdentifyFlags} from '@fluxer/constants/src/GatewayConstants';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('GatewayConnection');

const CONNECTION_INTERRUPTION_GRACE_MS = 4000;

interface DesiredSession {
	token: string | null;
	userIdHint: string | null;
}

class GatewayConnection {
	socket: GatewaySocket | null = null;
	isConnected: boolean = false;
	isConnecting: boolean = false;
	isReady: boolean = false;
	sessionId: string | null = null;
	gatewayCountryCode: string | null = null;
	gatewayLatitude: string | null = null;
	gatewayLongitude: string | null = null;
	private handlerRegistry: GatewayHandlerRegistry;
	private onlineListener: (() => void) | null = null;
	private visibilityListener: (() => void) | null = null;
	private pageshowListener: ((event: PageTransitionEvent) => void) | null = null;
	private netInfoUnsubscribe: (() => void) | null = null;
	private generation: number = 0;
	private desired: DesiredSession | null = null;
	private pendingGuildSyncId: string | null = null;
	private syncedGuildSessions: Record<string, string | null> = {};
	private completedGuildSyncSessions: Record<string, string | null> = {};
	private initialGuildIdAtIdentify: string | null = null;
	private isFatalCrashInProgress: boolean = false;
	private connectionInterrupted: boolean = false;
	private connectionGraceTimer: number | null = null;
	private previousSessionId: string | null = null;

	constructor() {
		makeAutoObservable<
			this,
			| 'cleanupSocket'
			| 'handleGatewayDispatch'
			| 'handleFatalGatewaySocketError'
			| 'ensureGuildActiveAndSynced'
			| 'flushPendingGuildSync'
			| 'applyGatewayGeoip'
			| 'beginConnectionGrace'
			| 'clearConnectionGrace'
			| 'connectionGraceTimer'
			| 'previousSessionId'
		>(
			this,
			{
				startSession: action.bound,
				beginConnectionGrace: action.bound,
				clearConnectionGrace: action.bound,
				connectionGraceTimer: false,
				previousSessionId: false,
				setToken: action.bound,
				sendInvisiblePresenceForCurrentSession: action.bound,
				retireCurrentSession: action.bound,
				logout: action.bound,
				handleGatewayReady: action.bound,
				handleConnectionResumed: action.bound,
				handleConnectionClosed: action.bound,
				cleanupSocket: action.bound,
				handleGatewayDispatch: action.bound,
				handleFatalGatewaySocketError: action.bound,
				ensureGuildActiveAndSynced: action.bound,
				flushPendingGuildSync: action.bound,
				syncGuildIfNeeded: action.bound,
				markGuildSynced: action.bound,
				applyGatewayGeoip: action.bound,
			},
			{autoBind: true},
		);
		this.handlerRegistry = createHandlerRegistry();
		this.setupPresenceSync();
		this.setupSelectedGuildSync();
	}

	get isConnectionInterrupted(): boolean {
		return this.connectionInterrupted;
	}

	private beginConnectionGrace(): void {
		if (this.connectionGraceTimer != null || this.connectionInterrupted) {
			return;
		}
		this.connectionGraceTimer = window.setTimeout(
			action(() => {
				this.connectionGraceTimer = null;
				if (!this.isConnected) {
					this.connectionInterrupted = true;
				}
			}),
			CONNECTION_INTERRUPTION_GRACE_MS,
		);
	}

	private clearConnectionGrace(): void {
		if (this.connectionGraceTimer != null) {
			clearTimeout(this.connectionGraceTimer);
			this.connectionGraceTimer = null;
		}
	}

	private setupPresenceSync(): void {
		deferUntilModulesLoaded(() => {
			reaction(
				() => LocalPresence.presenceKey,
				() => {
					const presence = LocalPresence.getGatewayPresence();
					if (!presence) {
						return;
					}
					this.socket?.updatePresence(presence.status, presence.afk, presence.mobile, presence.custom_status);
				},
			);
		});
	}

	private setupSelectedGuildSync(): void {
		deferUntilModulesLoaded(() => {
			reaction(
				() => ({
					guildId: SelectedGuild.selectedGuildId,
					nonce: SelectedGuild.selectionNonce,
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
		});
	}

	private createHandlerContext(): GatewayHandlerContext {
		return {
			socket: this.socket,
			previousSessionId: this.previousSessionId,
			setPreviousSessionId: (id: string) => {
				this.previousSessionId = id;
			},
			setReady: () => {
				runInAction(() => {
					this.isReady = true;
					this.isConnecting = false;
				});
				this.flushPendingGuildSync();
			},
			setConnectionGeoip: (data: GatewayGeoipPayload) => {
				this.applyGatewayGeoip(data);
			},
			markGuildSynced: (guildId: string) => {
				this.markGuildSynced(guildId);
			},
		};
	}

	private applyGatewayGeoip(data: GatewayGeoipPayload): void {
		const countryCode = typeof data.country_code === 'string' ? data.country_code : null;
		const latitude = typeof data.latitude === 'string' ? data.latitude : null;
		const longitude = typeof data.longitude === 'string' ? data.longitude : null;
		if (countryCode !== null) {
			this.gatewayCountryCode = countryCode;
		}
		if (latitude !== null) {
			this.gatewayLatitude = latitude;
		}
		if (longitude !== null) {
			this.gatewayLongitude = longitude;
		}
		GeoIP.applyConnectionFallbackCoordinates({latitude, longitude});
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
		if (this.visibilityListener) {
			document.removeEventListener('visibilitychange', this.visibilityListener);
			this.visibilityListener = null;
		}
		if (this.pageshowListener) {
			window.removeEventListener('pageshow', this.pageshowListener);
			this.pageshowListener = null;
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
		const gatewayUrl = RuntimeConfig.gatewayEndpoint;
		LocalPresence.updatePresence();
		const presence = LocalPresence.getGatewayPresence();
		const compression = getPreferredCompression();
		logger.info(`Using gateway compression: ${compression}`);
		let identifyFlags = 0;
		identifyFlags |= GatewayIdentifyFlags.DEBOUNCE_MESSAGE_REACTIONS;
		const initialGuildId = SelectedGuild.selectedGuildId ?? null;
		this.initialGuildIdAtIdentify = initialGuildId;
		const socket = new GatewaySocket(gatewayUrl, {
			apiVersion: DEFAULT_API_VERSION,
			token,
			properties,
			...(presence && {
				presence: {
					status: presence.status,
					afk: presence.afk,
					mobile: presence.mobile,
					custom_status: presence.custom_status,
				},
			}),
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
		window.addEventListener('online', this.onlineListener);
		this.visibilityListener = () => {
			if (!isCurrent()) {
				return;
			}
			if (document.visibilityState !== 'visible') {
				return;
			}
			socket.probeAfterResume('visibilitychange', {accelerateReconnect: false});
		};
		document.addEventListener('visibilitychange', this.visibilityListener);
		this.pageshowListener = (event: PageTransitionEvent) => {
			if (!isCurrent()) {
				return;
			}
			if (event.persisted) {
				socket.forceReconnectFromResume('pageshow-bfcache');
				return;
			}
			socket.probeAfterResume('pageshow', {accelerateReconnect: false});
		};
		window.addEventListener('pageshow', this.pageshowListener);
		socket.on(
			'stateChange',
			action((newState: GatewayState, previousState: GatewayState) => {
				if (!isCurrent()) {
					return;
				}
				this.isConnected = newState === GatewayState.Connected;
				this.isConnecting = newState === GatewayState.Connecting || newState === GatewayState.Reconnecting;
				if (newState === GatewayState.Connected) {
					this.clearConnectionGrace();
					this.connectionInterrupted = false;
				} else if (newState === GatewayState.Disconnected) {
					this.isReady = false;
					SessionManager.handleConnectionFailed();
					this.syncedGuildSessions = {};
					this.completedGuildSyncSessions = {};
					this.clearConnectionGrace();
					this.connectionInterrupted = true;
				} else if (previousState === GatewayState.Connected) {
					this.beginConnectionGrace();
				}
			}),
		);
		socket.on(
			'ready',
			action((data: unknown) => {
				if (!isCurrent()) {
					return;
				}
				const readyData = data as {
					session_id: string;
				};
				this.handleGatewayReady(readyData.session_id);
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

	private ensureGuildActiveAndSynced(
		guildId: string,
		options: {
			force?: boolean;
			reason?: string;
		} = {},
	): void {
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

	syncGuildIfNeeded(guildId: string, reason?: string, force = false): void {
		this.ensureGuildActiveAndSynced(guildId, {reason, force});
	}

	hasCompletedGuildSync(guildId: string): boolean {
		const sessionId = this.sessionId;
		if (!sessionId) {
			return false;
		}
		return this.completedGuildSyncSessions[guildId] === sessionId;
	}

	markGuildSynced(guildId: string): void {
		if (!guildId) {
			return;
		}
		const sessionId = this.sessionId;
		if (!sessionId) {
			return;
		}
		this.syncedGuildSessions[guildId] = sessionId;
		this.completedGuildSyncSessions[guildId] = sessionId;
		if (this.pendingGuildSyncId === guildId) {
			this.pendingGuildSyncId = null;
		}
	}

	private flushPendingGuildSync(): void {
		const guildId = this.pendingGuildSyncId ?? SelectedGuild.selectedGuildId;
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
		LocalPresence.handleSessionChanging();
		runInAction(() => {
			this.isConnecting = true;
			this.isReady = false;
			this.clearConnectionGrace();
			this.connectionInterrupted = false;
		});
		SessionManager.handleConnectionStarted();
		if (this.socket) {
			this.cleanupSocket();
		}
		Initialization.setConnecting();
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
					latitude: GeoIP.latitude,
					longitude: GeoIP.longitude,
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

	setToken(token: string): void {
		this.desired = {
			token,
			userIdHint: SessionManager.userId ?? null,
		};
		this.socket?.setToken(token);
	}

	sendInvisiblePresenceForCurrentSession(reason: GatewaySessionRetirementReason): void {
		sendInvisiblePresenceForLocalSession(this.socket, LocalPresence.mobile, reason, logger);
		LocalPresence.handleSessionChanging({clearRestoredIntent: true});
	}

	retireCurrentSession(reason: GatewaySessionRetirementReason): void {
		this.sendInvisiblePresenceForCurrentSession(reason);
		this.logout();
	}

	sendTerminalVoiceDisconnect(params: GatewayVoiceStateUpdateParams, reason: string): boolean {
		const socket = this.socket;
		if (!socket) return false;
		const sent = socket.updateVoiceStateExplicit(params);
		if (!sent) {
			logger.warn('Terminal voice disconnect could not be sent because the gateway socket was not open', {reason});
		}
		socket.disconnect(1000, reason, false);
		return sent;
	}

	logout(): void {
		LocalPresence.handleSessionChanging({clearRestoredIntent: true});
		this.clearConnectionGrace();
		this.connectionInterrupted = false;
		this.cleanupSocket();
		Presence.handleSessionInvalidated();
		Messages.handleSessionInvalidated();
		FavoriteMemes.reset();
		GuildMatureContentAgree.reset();
		Initialization.reset();
		MemberSearch.handleLogout();
		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;
		this.sessionId = null;
		this.desired = null;
		this.gatewayCountryCode = null;
		this.gatewayLatitude = null;
		this.gatewayLongitude = null;
		this.pendingGuildSyncId = null;
		this.syncedGuildSessions = {};
		this.completedGuildSyncSessions = {};
	}

	handleGatewayReady(sessionId: string): void {
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = true;
		this.sessionId = sessionId;
		this.markInitialGuildSynced(sessionId);
		SessionManager.handleConnectionReady();
		LocalPresence.updatePresence();
		TypingIndicator.reset();
		QuickSwitcher.recomputeIfOpen();
		this.flushPendingGuildSync();
		GuildMembers.handleConnectionResumed();
	}

	handleConnectionResumed(): void {
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = true;
		SessionManager.handleConnectionReady();
		this.markInitialGuildSynced(this.sessionId);
		LocalPresence.updatePresence();
		TypingIndicator.reset();
		QuickSwitcher.recomputeIfOpen();
		this.flushPendingGuildSync();
		GuildMembers.handleConnectionResumed();
	}

	handleConnectionClosed(code: number): void {
		SessionManager.handleConnectionClosed(code);
		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;
		LocalPresence.updatePresence();
		Permission.handleConnectionClose();
		this.syncedGuildSessions = {};
		this.completedGuildSyncSessions = {};
		if (code === 4004) {
			LayerManager.closeAll();
			Messages.handleConnectionClosed();
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
		this.completedGuildSyncSessions[guildId] = sessionId;
	}

	private handleGatewayError(error: GatewayErrorData): void {
		logger.warn(`Gateway error: [${error.code}] ${error.message}`);
		MediaEngine.handleGatewayError(error);
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
			MediaEngine.cleanup();
		} catch (cleanupError) {
			logger.error('Failed to complete fatal crash cleanup', cleanupError);
		}
		RuntimeCrash.triggerFatalCrash(error);
	}

	private handleGatewayDispatch(eventType: string, data: unknown): void {
		const handler = this.handlerRegistry.get(eventType);
		if (!handler) {
			return;
		}
		const context = this.createHandlerContext();
		runInAction(() => {
			handler(data, context);
		});
	}
}

export default new GatewayConnection();
