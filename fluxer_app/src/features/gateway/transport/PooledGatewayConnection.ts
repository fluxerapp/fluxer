// SPDX-License-Identifier: AGPL-3.0-or-later

import GeoIP from '@app/features/app/state/GeoIP';
import Initialization from '@app/features/app/state/Initialization';
import InstanceSnapshotStore, {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import AccountAccess from '@app/features/auth/state/AccountAccess';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {scheduleAccountReadyWork} from '@app/features/gateway/events/AccountReadyWork';
import type {GatewayGeoipPayload} from '@app/features/gateway/events/EventRouter';
import {
	ForegroundDemotionHandover,
	type ForegroundDemotionRestore,
} from '@app/features/gateway/transport/ForegroundDemotionHandover';
import {
	ForegroundGatewayConnectionRecoverableError,
	ForegroundGatewayRecoveryCause,
	GatewayAuthenticationFailedError,
} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import {ForegroundGatewayDispatchProcessor} from '@app/features/gateway/transport/ForegroundGatewayDispatchProcessor';
import {ForegroundPromotionReplayBuffer} from '@app/features/gateway/transport/ForegroundPromotionReplayBuffer';
import type {CompressionType} from '@app/features/gateway/transport/GatewayCompression';
import {GatewayConnectionInterruption} from '@app/features/gateway/transport/GatewayConnectionInterruption';
import {GatewayConnectionRole} from '@app/features/gateway/transport/GatewayConnectionRole';
import {GatewayGuildSubscriptionSynchronizer} from '@app/features/gateway/transport/GatewayGuildSubscriptionSynchronizer';
import {
	GatewayReadinessWaiters,
	GatewayReadyTimeoutError,
} from '@app/features/gateway/transport/GatewayReadinessWaiters';
import {GatewayResumeListenerOwner} from '@app/features/gateway/transport/GatewayResumeListenerOwner';
import {
	type GatewaySessionRetirementReason,
	sendInvisiblePresenceForLocalSession,
} from '@app/features/gateway/transport/GatewaySessionRetirement';
import {
	type GatewayDispatchDelivery,
	type GatewayErrorData,
	GatewaySocket,
	type GatewaySocketProperties,
	GatewayState,
	type GatewayVoiceStateUpdateParams,
} from '@app/features/gateway/transport/GatewaySocket';
import {selectGuildActivationTarget} from '@app/features/gateway/transport/GuildActivationTarget';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import MemberSearch from '@app/features/member/state/MemberSearch';
import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import SessionManager from '@app/features/platform/state/AuthSession';
import {writePrebootNetworkHint} from '@app/features/platform/state/PrebootNetworkHandoff';
import {ResetClientStateReason, resetClientState} from '@app/features/platform/state/ResetClientState';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getGatewayClientProperties} from '@app/features/platform/utils/ClientInfo';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import Presence from '@app/features/presence/state/Presence';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {reportGatewayErrorToVoice, teardownVoiceForFatalGatewayCrash} from '@app/features/voice/VoiceGatewayLifecycle';
import {DEFAULT_API_VERSION} from '@fluxer/constants/src/AppConstants';
import {GatewayCloseCodes, GatewayIdentifyFlags} from '@fluxer/constants/src/GatewayConstants';
import {action, actionBound, makeAutoObservable, observableRef, runInAction} from 'mobx';

const logger = new Logger('GatewayConnection');

const FOREGROUND_SOCKET_EVENTS = [
	'dispatch',
	'disconnect',
	'gatewayError',
	'fatalError',
	'stateChange',
	'ready',
	'resumed',
] as const;

export {
	GATEWAY_FOREGROUND_READY_TIMEOUT_MS,
	GatewayReadyTimeoutError,
} from '@app/features/gateway/transport/GatewayReadinessWaiters';

import ChannelFrecency from '@app/features/channel/state/ChannelFrecency';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import ForumReadState from '@app/features/forum/state/ForumReadState';
import NavigationSideEffects from '@app/features/navigation/state/NavigationSideEffects';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import ThreadRoster from '@app/features/threads/state/ThreadRoster';
import ThreadSubscriptions from '@app/features/threads/state/ThreadSubscriptions';

export class GatewaySessionSupersededError extends Error {
	constructor() {
		super('Gateway session was superseded before it became ready');
		this.name = 'GatewaySessionSupersededError';
	}
}

export class GatewaySessionStartFailedError extends Error {
	constructor(status: GatewaySessionStartStatus) {
		super(`Gateway session did not start: ${status}`);
		this.name = 'GatewaySessionStartFailedError';
	}
}

export const GatewaySessionStartStatus = Object.freeze({
	STARTED: 'started',
	ALREADY_CURRENT: 'already-current',
	SUPERSEDED: 'superseded',
	MISSING_TOKEN: 'missing-token',
	FAILED: 'failed',
} as const);

export type GatewaySessionStartStatus = (typeof GatewaySessionStartStatus)[keyof typeof GatewaySessionStartStatus];

export type GatewaySessionStartResult =
	| {
			readonly status:
				| typeof GatewaySessionStartStatus.STARTED
				| typeof GatewaySessionStartStatus.ALREADY_CURRENT
				| typeof GatewaySessionStartStatus.SUPERSEDED
				| typeof GatewaySessionStartStatus.MISSING_TOKEN;
			readonly generation: number;
	  }
	| {
			readonly status: typeof GatewaySessionStartStatus.FAILED;
			readonly generation: number;
			readonly cause: Error;
	  };

interface DesiredSession {
	readonly accountKey: string | null;
	readonly token: string | null;
	readonly userIdHint: string | null;
}

interface GatewaySocketOwner {
	readonly socket: GatewaySocket;
	readonly accountKey: string | null;
}

interface GatewayAuthenticationFailure {
	readonly error: GatewayAuthenticationFailedError;
	readonly completion: Promise<void>;
}

export interface ForegroundSocketAdoptionMeta {
	readonly accountKey: string;
	readonly sessionId: string;
}

export function selectForegroundActivationGuildId(): string | null {
	const channelId = Navigation.channelId;
	const channel = channelId ? Channels.getChannel(channelId) : undefined;
	return selectGuildActivationTarget({
		selectedGuildId: SelectedGuild.selectedGuildId,
		openChannelGuildId: channel?.guildId ?? null,
	});
}

function normalizeGatewayConnectionError(error: unknown): Error {
	return error instanceof Error ? error : new Error(String(error));
}

export class PooledGatewayConnection {
	isConnected: boolean = false;
	connectionEpoch: number = 0;
	isConnecting: boolean = false;
	isReady: boolean = false;
	sessionId: string | null = null;
	gatewayCountryCode: string | null = null;
	gatewayLatitude: string | null = null;
	gatewayLongitude: string | null = null;
	private readonly dispatchProcessor: ForegroundGatewayDispatchProcessor;
	private readonly resumeListeners: GatewayResumeListenerOwner;
	private readonly demotionHandover: ForegroundDemotionHandover;
	private readonly promotionReplay: ForegroundPromotionReplayBuffer;
	private readonly readinessWaiters: GatewayReadinessWaiters;
	private readonly guildSubscriptions: GatewayGuildSubscriptionSynchronizer;
	private readonly connectionInterruption: GatewayConnectionInterruption;
	private socketOwner: GatewaySocketOwner | null = null;
	private generation: number = 0;
	private desired: DesiredSession | null = null;
	private authenticationFailure: GatewayAuthenticationFailure | null = null;
	private previousReadySessionId: string | null = null;
	private isFatalCrashInProgress: boolean = false;

	constructor() {
		this.resumeListeners = new GatewayResumeListenerOwner();
		this.dispatchProcessor = new ForegroundGatewayDispatchProcessor({
			getSocket: () => this.socket,
			getForegroundSocket: (accountKey) => this.foregroundSocketForAccount(accountKey),
			getAccountUserId: (accountKey) => SessionManager.getAccount(accountKey)?.userId ?? null,
			getPreviousSessionId: () => this.previousReadySessionId,
			setPreviousSessionId: (sessionId) => {
				this.previousReadySessionId = sessionId;
			},
			handleReady: (accountKey, isCurrent) => this.handleDispatchReady(accountKey, isCurrent),
			handleGeoip: (data) => this.applyGatewayGeoip(data),
			markGuildSynced: (guildId) => this.markGuildSynced(guildId),
		});
		this.demotionHandover = new ForegroundDemotionHandover({
			applySnapshotDispatch: (accountKey, expectedUserId, delivery) => {
				this.dispatchProcessor.applyForegroundSnapshot(accountKey, expectedUserId, delivery);
			},
			persistRetiringToken: (accountKey, token) => SessionManager.setRetiringAccountToken(accountKey, token),
			persistAccountToken: (accountKey, token) => SessionManager.setAccountToken(accountKey, token),
			getAccountToken: (accountKey) => SessionManager.getAccount(accountKey)?.token ?? null,
		});
		this.promotionReplay = new ForegroundPromotionReplayBuffer();
		this.readinessWaiters = new GatewayReadinessWaiters((generation, accountKey, error) => {
			this.handleReadyTimeout(generation, accountKey, error);
		});
		this.guildSubscriptions = new GatewayGuildSubscriptionSynchronizer({
			getSocket: () => this.socket,
			isReady: () => this.isReady,
			getSessionId: () => this.sessionId,
			getSelectedGuildId: () => selectForegroundActivationGuildId(),
		});
		this.connectionInterruption = new GatewayConnectionInterruption(() => this.isConnected);
		makeAutoObservable<
			this,
			| 'dispatchProcessor'
			| 'resumeListeners'
			| 'demotionHandover'
			| 'promotionReplay'
			| 'readinessWaiters'
			| 'guildSubscriptions'
			| 'connectionInterruption'
			| 'socketOwner'
			| 'generation'
			| 'desired'
			| 'authenticationFailure'
			| 'cleanupSocket'
			| 'handleGatewayDispatch'
			| 'handleGatewayAuthenticationFailed'
			| 'handleFatalGatewaySocketError'
			| 'handleDispatchReady'
			| 'handleReadyTimeout'
			| 'applyGatewayGeoip'
		>(
			this,
			{
				dispatchProcessor: false,
				resumeListeners: false,
				demotionHandover: false,
				promotionReplay: false,
				readinessWaiters: false,
				guildSubscriptions: false,
				connectionInterruption: false,
				socketOwner: observableRef,
				generation: false,
				desired: false,
				authenticationFailure: false,
				startSession: actionBound,
				cancelPendingSessionStart: actionBound,
				setToken: actionBound,
				reuseReadySession: actionBound,
				sendInvisiblePresenceForCurrentSession: actionBound,
				logout: actionBound,
				handleConnectionOpen: actionBound,
				handleConnectionResumed: actionBound,
				cleanupSocket: actionBound,
				handleGatewayDispatch: actionBound,
				handleGatewayAuthenticationFailed: actionBound,
				handleFatalGatewaySocketError: actionBound,
				handleDispatchReady: actionBound,
				handleReadyTimeout: actionBound,
				flushPendingGuildSync: actionBound,
				clearPendingGuildSync: actionBound,
				syncGuildIfNeeded: actionBound,
				markGuildSynced: actionBound,
				applyGatewayGeoip: actionBound,
			},
			{autoBind: true},
		);
	}

	get socket(): GatewaySocket | null {
		return this.socketOwner === null ? null : this.socketOwner.socket;
	}

	get foregroundAccountKey(): string | null {
		return this.socketOwner === null ? null : this.socketOwner.accountKey;
	}

	get isConnectionInterrupted(): boolean {
		return this.connectionInterruption.interrupted;
	}

	foregroundSocketForAccount(accountKey: string | null): GatewaySocket | null {
		const owner = this.socketOwner;
		if (owner === null || owner.accountKey !== accountKey) {
			return null;
		}
		return owner.socket;
	}

	isSocketReconnecting(accountKey: string): boolean {
		return this.foregroundSocketForAccount(accountKey)?.getState() === GatewayState.Reconnecting;
	}

	private handleDispatchReady(accountKey: string | null, isCurrent: () => boolean): boolean {
		if (!isCurrent()) {
			return false;
		}
		this.isReady = true;
		this.isConnecting = false;
		this.completeGatewayReady(accountKey);
		this.flushPendingGuildSync();
		return true;
	}

	private completeGatewayReady(accountKey: string | null): void {
		if (accountKey !== null) {
			AccountAccess.markGatewayReady(accountKey);
			this.recordPrebootNetworkHint(accountKey);
		}
		this.resolveReadyWaiters();
	}

	private recordPrebootNetworkHint(accountKey: string): void {
		const snapshot = RuntimeConfig.getSnapshotOrNull();
		if (snapshot === null || SessionManager.currentAccountKey !== accountKey) {
			return;
		}
		const instanceKey = runtimeInstanceKey(snapshot);
		writePrebootNetworkHint({
			accountKey,
			gatewayEndpoint: snapshot.gatewayEndpoint,
			discoveryUrl: instanceKey === null ? null : InstanceSnapshotStore.discoveryUrlFor(instanceKey),
		});
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
		GeoIP.applyConnectionFallbackGeo({countryCode, regionCode: null, latitude, longitude});
	}

	private cleanupSocket(): void {
		const owner = this.socketOwner;
		this.demotionHandover.clear();
		this.promotionReplay.clear();
		this.socketOwner = null;
		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;
		this.sessionId = null;
		if (owner !== null) {
			try {
				owner.socket.reset(false);
			} catch (err) {
				logger.warn('Error while resetting socket during cleanup', err);
			}
		}
		this.resumeListeners.dispose();
	}

	beginForegroundDemotionHandover(accountKey: string): boolean {
		this.demotionHandover.assertAvailable();
		const owner = this.socketOwner;
		const account = SessionManager.getAccount(accountKey);
		if (owner === null || owner.accountKey !== accountKey || account === null || !owner.socket.isConnected()) {
			return false;
		}
		this.generation += 1;
		for (const event of FOREGROUND_SOCKET_EVENTS) {
			owner.socket.removeAllListeners(event);
		}
		this.resumeListeners.dispose();
		this.demotionHandover.begin({
			accountKey,
			expectedUserId: account.userId,
			socket: owner.socket,
		});
		return true;
	}

	async consumeForegroundDemotionHandover(accountKey: string): Promise<boolean> {
		return this.demotionHandover.consume(accountKey);
	}

	async rollbackForegroundDemotionHandover(accountKey: string): Promise<void> {
		const rollback = await this.demotionHandover.rollback(accountKey);
		if (rollback === null) {
			return;
		}
		if (rollback.outcome === 'retire') {
			this.retireSession();
			return;
		}
		this.restoreForegroundDemotion(accountKey, rollback);
	}

	private restoreForegroundDemotion(accountKey: string, restore: ForegroundDemotionRestore): void {
		if (
			!restore.canRestore ||
			!restore.socket.isConnected() ||
			SessionManager.currentAccountKey !== accountKey ||
			this.foregroundSocketForAccount(accountKey) !== restore.socket
		) {
			this.retireSession();
			return;
		}
		const generation = ++this.generation;
		this.installSocketWiring(restore.socket, generation, accountKey);
		const sessionId = restore.socket.getSessionId();
		if (sessionId !== null && sessionId !== this.sessionId) {
			this.handleConnectionOpen(sessionId);
		}
		const isCurrent = (): boolean =>
			this.foregroundSocketForAccount(accountKey) === restore.socket && this.generation === generation;
		for (const delivery of restore.deliveries) {
			if (restore.socket.isDispatchActive(delivery.receipt)) {
				const replayDelivery =
					restore.persistedAuthToken !== null && delivery.type === 'AUTH_SESSION_CHANGE'
						? {...delivery, persistedAuthTokenAccountKey: accountKey}
						: delivery;
				this.handleGatewayDispatch(restore.socket, replayDelivery, accountKey, isCurrent);
			}
		}
	}

	detachForegroundSocket(): GatewaySocket | null {
		if (this.demotionHandover.isActive) {
			throw new Error('Foreground gateway socket cannot detach during an active demotion handover');
		}
		const owner = this.socketOwner;
		if (owner === null) {
			return null;
		}
		this.generation += 1;
		for (const event of FOREGROUND_SOCKET_EVENTS) {
			owner.socket.removeAllListeners(event);
		}
		this.resumeListeners.dispose();
		this.socketOwner = null;
		this.connectionInterruption.restore();
		this.rejectReadyWaiters(new GatewaySessionSupersededError());
		this.resetConnectionLocalState();
		this.guildSubscriptions.clearInitialGuild();
		this.previousReadySessionId = null;
		this.promotionReplay.clear();
		return owner.socket;
	}

	adoptForegroundSocket(socket: GatewaySocket, meta: ForegroundSocketAdoptionMeta): void {
		if (this.socketOwner !== null) {
			throw new Error('Foreground gateway socket adoption requires an empty owner');
		}
		if (SessionManager.currentAccountKey !== meta.accountKey) {
			throw new Error('Foreground gateway socket adoption does not match the active account');
		}
		if (socket.getSessionId() !== meta.sessionId || !socket.isConnected()) {
			throw new Error('Foreground gateway socket adoption requires its live session');
		}
		const token = SessionManager.token;
		if (token === null) {
			throw new Error('Foreground gateway socket adoption requires an active token');
		}
		LocalPresence.updatePresence();
		const presence = LocalPresence.getGatewayPresence();
		socket.setToken(token);
		socket.configureOwnership({
			role: GatewayConnectionRole.FOREGROUND,
			presence:
				presence === null
					? undefined
					: {
							status: presence.status,
							afk: presence.afk,
							mobile: presence.mobile,
							custom_status: presence.custom_status,
						},
			initialGuildId: null,
			isMobileLayout: () => MobileLayout.isMobileLayout(),
			geo: () => ({latitude: GeoIP.latitude, longitude: GeoIP.longitude}),
		});
		const generation = ++this.generation;
		this.socketOwner = {socket, accountKey: meta.accountKey};
		this.desired = {accountKey: meta.accountKey, token, userIdHint: SessionManager.userId};
		this.sessionId = meta.sessionId;
		this.previousReadySessionId = meta.sessionId;
		this.connectionEpoch += 1;
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = false;
		this.connectionInterruption.restore();
		this.guildSubscriptions.prepareAdoptedSession();
		this.promotionReplay.begin(meta.accountKey, socket);
		this.installSocketWiring(socket, generation, meta.accountKey);
	}

	preparePromotedForegroundReplay(): void {
		this.resetEphemeralForegroundStates();
		Initialization.setConnecting();
	}

	replayForegroundDispatch(accountKey: string, delivery: GatewayDispatchDelivery): void {
		const socket = this.foregroundSocketForAccount(accountKey);
		if (socket === null) {
			throw new Error('Cannot replay a gateway dispatch without its account-owned foreground socket');
		}
		const isCurrent = (): boolean => this.foregroundSocketForAccount(accountKey) === socket;
		if (!socket.isDispatchActive(delivery.receipt)) {
			throw new Error('Cannot replay an inactive gateway dispatch receipt');
		}
		this.handleGatewayDispatch(socket, delivery, accountKey, isCurrent);
		if (!socket.isConnected()) {
			throw new Error('Foreground gateway disconnected while replaying a dispatch');
		}
		if (socket.isDispatchActive(delivery.receipt)) {
			throw new Error('Foreground gateway dispatch replay did not complete its receipt');
		}
	}

	finalizePromotedForegroundSession(accountKey: string): void {
		const socket = this.foregroundSocketForAccount(accountKey);
		if (socket === null || !socket.isConnected()) {
			throw new Error('Cannot finalize a promoted foreground gateway without its live socket');
		}
		const sessionId = socket.getSessionId();
		if (sessionId === null || sessionId !== this.sessionId) {
			throw new Error('Cannot finalize a promoted foreground gateway with a mismatched session');
		}
		const isCurrent = (): boolean => this.foregroundSocketForAccount(accountKey) === socket;
		this.promotionReplay.drain(accountKey, socket, (delivery) => {
			if (!socket.isDispatchActive(delivery.receipt)) {
				throw new Error('Cannot finalize a promoted foreground gateway with an inactive live dispatch');
			}
			this.handleGatewayDispatch(socket, delivery, accountKey, isCurrent);
			if (!socket.isConnected() || socket.isDispatchActive(delivery.receipt)) {
				throw new Error('Promoted foreground gateway failed while draining live dispatches');
			}
		});
		SessionManager.handleConnectionStarted();
		this.handleConnectionOpen(sessionId);
		this.completeGatewayReady(accountKey);
		Initialization.setReady();
		Messages.handleGatewayReady();
		ForumPosts.handleGatewayReady();
		ForumReadState.handleGatewayReady();
		NavigationSideEffects.handleGatewayReady();
		const userId = SessionManager.userId;
		if (userId !== null) {
			scheduleAccountReadyWork(userId);
		}
		const presence = LocalPresence.getGatewayPresence();
		if (presence !== null) {
			socket.updatePresence(presence.status, presence.afk, presence.mobile, presence.custom_status);
		}
	}

	private createGatewaySocket(
		gatewayUrl: string,
		token: string,
		properties: GatewaySocketProperties,
		generation: number,
		accountKey: string | null,
	): GatewaySocket {
		LocalPresence.updatePresence();
		const presence = LocalPresence.getGatewayPresence();
		const compression: CompressionType = 'zstd-stream';
		logger.info(`Using gateway compression: ${compression}`);
		let identifyFlags = 0;
		identifyFlags |= GatewayIdentifyFlags.DEBOUNCE_MESSAGE_REACTIONS;
		identifyFlags |= GatewayIdentifyFlags.CHANNEL_THREADS;
		const initialGuildId = SelectedGuild.selectedGuildId ?? null;
		this.guildSubscriptions.recordInitialGuild(initialGuildId);
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
			isMobileLayout: () => MobileLayout.isMobileLayout(),
			geo: () => ({latitude: GeoIP.latitude, longitude: GeoIP.longitude}),
			role: GatewayConnectionRole.FOREGROUND,
		});
		this.installSocketWiring(socket, generation, accountKey);
		return socket;
	}

	private installSocketWiring(socket: GatewaySocket, generation: number, accountKey: string | null): void {
		const isCurrent = (): boolean => this.socket === socket && this.generation === generation;
		const whenCurrent =
			<A extends ReadonlyArray<unknown>>(handler: (...args: A) => void) =>
			(...args: A): void => {
				if (!isCurrent()) {
					return;
				}
				handler(...args);
			};
		socket.on(
			'dispatch',
			whenCurrent((delivery: GatewayDispatchDelivery) => {
				if (this.promotionReplay.capture(socket, accountKey, delivery)) {
					return;
				}
				this.handleGatewayDispatch(socket, delivery, accountKey, isCurrent);
			}),
		);
		socket.on(
			'disconnect',
			whenCurrent((event: {code: number}) => {
				if (event.code !== GatewayCloseCodes.AUTHENTICATION_FAILED) {
					return;
				}
				this.handleGatewayAuthenticationFailed();
			}),
		);
		socket.on(
			'gatewayError',
			whenCurrent((error: GatewayErrorData) => this.handleGatewayError(error)),
		);
		socket.on(
			'fatalError',
			whenCurrent((error: Error) => this.handleFatalGatewaySocketError(error)),
		);
		this.resumeListeners.install({socket, isCurrent});
		socket.on(
			'stateChange',
			action(
				whenCurrent((newState: GatewayState, previousState: GatewayState) =>
					this.handleSocketStateChange(newState, previousState),
				),
			),
		);
		socket.on(
			'ready',
			action(
				whenCurrent((data: unknown) => {
					const readyData = data as {
						session_id: string;
					};
					this.handleConnectionOpen(readyData.session_id);
				}),
			),
		);
		socket.on('resumed', action(whenCurrent(() => this.handleConnectionResumed())));
	}

	private handleSocketStateChange(newState: GatewayState, previousState: GatewayState): void {
		if (newState === GatewayState.Connected || previousState === GatewayState.Connected) {
			this.connectionEpoch += 1;
		}
		this.isConnected = newState === GatewayState.Connected;
		this.isConnecting = newState === GatewayState.Connecting || newState === GatewayState.Reconnecting;
		if (newState === GatewayState.Connected) {
			this.connectionInterruption.restore();
			return;
		}
		if (newState === GatewayState.Disconnected) {
			this.isReady = false;
			this.rejectReadyWaiters(
				new ForegroundGatewayConnectionRecoverableError(
					this.foregroundAccountKey,
					ForegroundGatewayRecoveryCause.TRANSPORT_DISCONNECTED,
					new Error('Gateway disconnected before it became ready'),
				),
			);
			SessionManager.handleConnectionFailed();
			this.guildSubscriptions.resetSessionTracking();
			this.connectionInterruption.disconnect();
			return;
		}
		if (previousState === GatewayState.Connected) {
			this.connectionInterruption.beginGracePeriod();
		}
		if (newState === GatewayState.Reconnecting && !this.isReady) {
			this.rejectReadyWaiters(
				new ForegroundGatewayConnectionRecoverableError(
					this.foregroundAccountKey,
					ForegroundGatewayRecoveryCause.TRANSPORT_DISCONNECTED,
					new Error('Gateway connection failed before it became ready'),
				),
			);
		}
	}

	syncGuildIfNeeded(guildId: string, reason?: string, force = false): void {
		this.guildSubscriptions.sync(guildId, {reason: reason ?? null, force});
	}

	hasCompletedGuildSync(guildId: string): boolean {
		return this.guildSubscriptions.hasCompleted(guildId);
	}

	markGuildSynced(guildId: string): void {
		this.guildSubscriptions.markCompleted(guildId);
	}

	clearPendingGuildSync(): void {
		this.guildSubscriptions.clearPending();
	}

	flushPendingGuildSync(): void {
		this.guildSubscriptions.flush();
	}

	async startSession(accountKey: string | null): Promise<GatewaySessionStartResult> {
		const userIdHint = SessionManager.userId ?? null;
		const storedToken = SessionManager.token;
		const desired: DesiredSession = {
			accountKey,
			token: storedToken,
			userIdHint,
		};
		if (this.socketOwner !== null && (this.isConnecting || this.isConnected) && this.desired) {
			const sameAccount = this.desired.accountKey === desired.accountKey;
			const sameToken = this.desired.token === desired.token;
			const sameUser = this.desired.userIdHint === desired.userIdHint;
			if (sameAccount && sameToken && sameUser) {
				return {status: GatewaySessionStartStatus.ALREADY_CURRENT, generation: this.generation};
			}
		}
		this.desired = desired;
		const generation = ++this.generation;
		this.rejectReadyWaiters(new GatewaySessionSupersededError());
		LocalPresence.handleSessionChanging();
		if (this.socket !== null) {
			this.cleanupSocket();
		}
		runInAction(() => {
			this.isConnecting = true;
			this.isReady = false;
			this.connectionInterruption.restore();
		});
		SessionManager.handleConnectionStarted();
		Initialization.setConnecting();
		try {
			if (!storedToken) {
				this.resetConnectionLocalState();
				SessionManager.handleConnectionFailed();
				return {status: GatewaySessionStartStatus.MISSING_TOKEN, generation};
			}
			if (this.generation !== generation) {
				return {status: GatewaySessionStartStatus.SUPERSEDED, generation};
			}
			let properties: GatewaySocketProperties;
			try {
				properties = await getGatewayClientProperties({
					latitude: GeoIP.latitude,
					longitude: GeoIP.longitude,
				});
			} catch (err) {
				logger.error('Failed to gather client metadata for gateway identification', err);
				return this.failForegroundSession(accountKey, generation, err);
			}
			if (this.generation !== generation) {
				return {status: GatewaySessionStartStatus.SUPERSEDED, generation};
			}
			const socket = this.createGatewaySocket(
				RuntimeConfig.gatewayEndpoint,
				storedToken,
				properties,
				generation,
				accountKey,
			);
			runInAction(() => {
				if (this.generation === generation) {
					this.socketOwner = {socket, accountKey};
				}
			});
			if (this.socket === socket) {
				socket.connect();
				return {status: GatewaySessionStartStatus.STARTED, generation};
			}
			return {status: GatewaySessionStartStatus.SUPERSEDED, generation};
		} catch (err) {
			logger.error('Failed to connect to gateway', err);
			return this.failForegroundSession(accountKey, generation, err);
		}
	}

	cancelPendingSessionStart(accountKey: string): boolean {
		if (!this.isConnecting || this.isReady || this.desired?.accountKey !== accountKey) {
			return false;
		}
		this.retireSession();
		SessionManager.handleConnectionFailed();
		return true;
	}

	private failForegroundSession(
		accountKey: string | null,
		generation: number,
		error: unknown,
	): GatewaySessionStartResult {
		if (this.generation !== generation) {
			return {status: GatewaySessionStartStatus.SUPERSEDED, generation};
		}
		const connectionError = normalizeGatewayConnectionError(error);
		this.cleanupSocket();
		this.resetConnectionLocalState();
		this.rejectReadyWaiters(connectionError);
		if (accountKey !== null) {
			AccountAccess.markGatewayUnavailable(accountKey, connectionError);
		}
		SessionManager.handleConnectionFailed();
		return {status: GatewaySessionStartStatus.FAILED, generation, cause: connectionError};
	}

	async startSessionAndWaitForReady(accountKey: string | null): Promise<void> {
		const result = await this.startSession(accountKey);
		if (result.status === GatewaySessionStartStatus.FAILED) {
			throw new ForegroundGatewayConnectionRecoverableError(
				accountKey,
				ForegroundGatewayRecoveryCause.TRANSPORT_START_FAILED,
				result.cause,
			);
		}
		if (result.status === GatewaySessionStartStatus.SUPERSEDED) {
			throw new GatewaySessionSupersededError();
		}
		if (
			result.status !== GatewaySessionStartStatus.STARTED &&
			result.status !== GatewaySessionStartStatus.ALREADY_CURRENT
		) {
			throw new GatewaySessionStartFailedError(result.status);
		}
		try {
			await this.waitForReady(result.generation, accountKey);
		} catch (error) {
			if (error instanceof GatewayAuthenticationFailedError) {
				await this.awaitGatewayAuthenticationFailure(error);
				throw error;
			}
			if (error instanceof GatewayReadyTimeoutError) {
				throw new ForegroundGatewayConnectionRecoverableError(
					accountKey,
					ForegroundGatewayRecoveryCause.READINESS_TIMEOUT,
					error,
				);
			}
			throw error;
		}
	}

	private async awaitGatewayAuthenticationFailure(error: GatewayAuthenticationFailedError): Promise<void> {
		const failure = this.authenticationFailure;
		if (failure === null || failure.error !== error) {
			throw new Error(`Gateway authentication failure for ${error.accountKey ?? 'no account'} has no completion`);
		}
		await failure.completion;
		if (this.authenticationFailure === failure) {
			this.authenticationFailure = null;
		}
	}

	private waitForReady(generation: number, accountKey: string | null): Promise<void> {
		if (this.isReady && this.generation === generation) {
			return Promise.resolve();
		}
		return this.readinessWaiters.wait(generation, accountKey);
	}

	private handleReadyTimeout(generation: number, accountKey: string | null, error: Error): void {
		if (generation !== this.generation) {
			return;
		}
		const socket = this.socket;
		const socketState = socket?.getState() ?? null;
		if (socketState === GatewayState.Reconnecting) {
			logger.warn('Gateway session is not ready yet, leaving the socket to its reconnect backoff', error);
			return;
		}
		if (socket !== null && socketState === GatewayState.Connecting) {
			logger.warn('Gateway session never became ready, reconnecting the socket with backoff', error);
			socket.disconnect(4000, 'Gateway session did not become ready', true);
			return;
		}
		logger.error('Gateway session never became ready, retiring the socket', error);
		if (accountKey !== null) {
			AccountAccess.markGatewayUnavailable(accountKey, error);
		}
		this.retireSession();
		SessionManager.handleConnectionFailed();
	}

	private resolveReadyWaiters(): void {
		if (!this.isReady) {
			return;
		}
		this.readinessWaiters.resolve(this.generation);
	}

	private rejectReadyWaiters(error: Error): void {
		this.readinessWaiters.rejectAll(error);
	}

	setToken(token: string): void {
		this.desired = {
			accountKey: this.foregroundAccountKey,
			token,
			userIdHint: SessionManager.userId ?? null,
		};
		this.socket?.setToken(token);
	}

	reuseReadySession(accountKey: string, token: string): void {
		if (
			this.foregroundAccountKey !== accountKey ||
			SessionManager.currentAccountKey !== accountKey ||
			this.socket === null ||
			!this.isReady
		) {
			throw new Error(`Cannot reuse a foreground gateway that is not ready for ${accountKey}`);
		}
		this.setToken(token);
		SessionManager.handleConnectionStarted();
		SessionManager.handleConnectionReady();
	}

	sendInvisiblePresenceForCurrentSession(reason: GatewaySessionRetirementReason): void {
		sendInvisiblePresenceForLocalSession(this.socket, LocalPresence.mobile, reason, logger);
		LocalPresence.handleSessionChanging({clearRestoredIntent: true});
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

	private resetEphemeralForegroundStates(): void {
		Presence.handleSessionInvalidated();
		Messages.handleSessionInvalidated();
		FavoriteMemes.reset();
		AttachmentUrlRefresher.reset();
		GuildMatureContentAgree.reset();
		ChannelFrecency.handleLogout();
		MemberSearch.handleLogout();
		ThreadPanel.closeCreate();
		ThreadGuilds.reset();
		ChannelThreads.handleGatewayReady([]);
		ThreadRoster.reset();
		ForumPosts.reset();
		ForumReadState.handleGatewayReady();
	}

	resetConnectionLocalState(): void {
		this.isConnected = false;
		this.isConnecting = false;
		this.isReady = false;
		this.sessionId = null;
		this.desired = null;
		this.gatewayCountryCode = null;
		this.gatewayLatitude = null;
		this.gatewayLongitude = null;
		this.guildSubscriptions.resetConnectionTracking();
	}

	private retireSession(): void {
		this.generation += 1;
		this.connectionInterruption.restore();
		this.cleanupSocket();
		this.rejectReadyWaiters(new GatewaySessionSupersededError());
		this.resetConnectionLocalState();
	}

	logout(): void {
		this.retireForeground();
		Initialization.reset();
	}

	retireForAccountSwitch(): void {
		this.retireForeground();
		Initialization.setLoading();
	}

	private retireForeground(): void {
		this.generation += 1;
		LocalPresence.handleSessionChanging({clearRestoredIntent: true});
		this.connectionInterruption.restore();
		this.cleanupSocket();
		this.rejectReadyWaiters(new GatewaySessionSupersededError());
		this.resetEphemeralForegroundStates();
		this.resetConnectionLocalState();
	}

	handleConnectionOpen(sessionId: string): void {
		this.sessionId = sessionId;
		this.completeLiveSession();
	}

	handleConnectionResumed(): void {
		this.completeLiveSession();
	}

	private completeLiveSession(): void {
		this.isConnected = true;
		this.isConnecting = false;
		this.isReady = true;
		this.guildSubscriptions.markInitialGuildCompleted(this.sessionId);
		SessionManager.handleConnectionReady();
		ThreadSubscriptions.handleConnectionReady();
		LocalPresence.updatePresence();
		TypingIndicator.reset();
		QuickSwitcher.recomputeIfOpen();
		this.flushPendingGuildSync();
		void import('@app/features/member/state/GuildMembers').then((module) => {
			module.default.handleConnectionResumed();
		});
		this.resolveReadyWaiters();
	}

	private handleGatewayError(error: GatewayErrorData): void {
		logger.warn(`Gateway error: [${error.code}] ${error.message}`);
		reportGatewayErrorToVoice(error);
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
			teardownVoiceForFatalGatewayCrash();
		} catch (cleanupError) {
			logger.error('Failed to complete fatal crash cleanup', cleanupError);
		}
		RuntimeCrash.triggerFatalCrash(error);
	}

	private handleGatewayDispatch(
		socket: GatewaySocket,
		delivery: GatewayDispatchDelivery,
		accountKey: string | null,
		isCurrent: () => boolean,
	): void {
		this.dispatchProcessor.dispatch(socket, delivery, accountKey, isCurrent);
	}

	private handleGatewayAuthenticationFailed(): void {
		logger.error('Authentication failed: clearing client state and logging out');
		const activeFailure = this.authenticationFailure;
		if (activeFailure !== null) {
			this.rejectReadyWaiters(activeFailure.error);
			return;
		}
		const error = new GatewayAuthenticationFailedError(this.foregroundAccountKey);
		this.rejectReadyWaiters(error);
		const completion = this.completeGatewayAuthenticationFailure(error);
		const failure = {error, completion} satisfies GatewayAuthenticationFailure;
		this.authenticationFailure = failure;
		void completion.then(() => {
			if (this.authenticationFailure === failure) {
				this.authenticationFailure = null;
			}
		});
	}

	private async completeGatewayAuthenticationFailure(error: GatewayAuthenticationFailedError): Promise<void> {
		const reset = resetClientState({reason: ResetClientStateReason.GATEWAY_AUTH_FAILURE, keepDrafts: true});
		try {
			LayerManager.closeAll();
			this.logout();
		} catch (error) {
			logger.error('Failed to retire the gateway after authentication failure', error);
		}
		try {
			await reset;
		} catch (error) {
			logger.error('Failed to clear client state after gateway authentication failure', error);
		}
		try {
			await Authentication.handleConnectionClosed({
				code: GatewayCloseCodes.AUTHENTICATION_FAILED,
				accountKey: error.accountKey,
			});
		} catch (error) {
			logger.error('Failed to complete gateway authentication failure logout', error);
		}
	}
}
