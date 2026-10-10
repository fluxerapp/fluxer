// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {
	parseSnapshotGatewayEvent,
	type SnapshotAuthSessionChangePayload,
} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {SNAPSHOT_SCHEMA_EPOCH} from '@app/features/gateway/snapshot/SnapshotEntities';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import type {StateSnapshotCapture, StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import BackgroundAccountPresence from '@app/features/gateway/transport/BackgroundAccountPresence';
import {sumMentionCounts} from '@app/features/gateway/transport/BackgroundGatewayAccounts';
import {
	type BackgroundDispatchHandoverEnd,
	BackgroundGatewayDispatchQueue,
} from '@app/features/gateway/transport/BackgroundGatewayDispatchQueue';
import {
	BackgroundGatewayReadyIdentityMismatchError,
	type BackgroundSnapshotReadySeed,
	buildBackgroundSnapshotReadySeed,
} from '@app/features/gateway/transport/BackgroundGatewaySnapshotHydration';
import {
	BackgroundReadStateCapacityError,
	BackgroundReadStateReducer,
} from '@app/features/gateway/transport/BackgroundReadStateReducer';
import {
	BackgroundMentionCountMode,
	type BackgroundMessageNotification,
	type BackgroundSnapshotSink,
} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {GatewayConnectionRole} from '@app/features/gateway/transport/GatewayConnectionRole';
import {
	type GatewayDispatchDelivery,
	type GatewayPresence,
	GatewaySocket,
	type GatewaySocketProperties,
	MAX_GATEWAY_AUTH_TOKEN_LENGTH,
} from '@app/features/gateway/transport/GatewaySocket';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Idle from '@app/features/ui/state/Idle';
import {DEFAULT_API_VERSION} from '@fluxer/constants/src/AppConstants';
import {GatewayCloseCodes, GatewayIdentifyFlags} from '@fluxer/constants/src/GatewayConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('BackgroundGatewaySession');

const BACKGROUND_SOCKET_EVENTS = ['dispatch', 'disconnect'] as const;

interface BackgroundGatewaySessionConfig {
	readonly account: Account;
	readonly gatewayEndpoint: string;
	readonly properties: GatewaySocketProperties;
	readonly onStopRequested: () => void;
}

interface BackgroundGatewayReadinessWait {
	readonly promise: Promise<boolean>;
	readonly resolve: (ready: boolean) => void;
	timer: number | null;
}

function createBackgroundGatewayReadinessWait(): BackgroundGatewayReadinessWait {
	let resolvePromise: ((ready: boolean) => void) | null = null;
	const promise = new Promise<boolean>((resolve) => {
		resolvePromise = resolve;
	});
	if (resolvePromise === null) {
		throw new Error('Background gateway readiness promise did not initialize synchronously');
	}
	return {promise, resolve: resolvePromise, timer: null};
}

export class BackgroundGatewaySession {
	mentionCount = 0;
	readonly accountKey: string;
	account: Account;
	private readonly gatewayEndpoint: string;
	private readonly properties: GatewaySocketProperties;
	private readonly onStopRequested: () => void;
	private readonly channelMentionCounts = new Map<string, number>();
	private readonly sink: BackgroundSnapshotSink;
	private readonly dispatchQueue: BackgroundGatewayDispatchQueue;
	private socket: GatewaySocket | null = null;
	private ready = false;
	private authMutationPending = false;
	private promotionReadinessWait: BackgroundGatewayReadinessWait | null = null;

	constructor(config: BackgroundGatewaySessionConfig) {
		this.account = config.account;
		this.accountKey = getAccountKey(config.account);
		this.gatewayEndpoint = config.gatewayEndpoint;
		this.properties = config.properties;
		this.onStopRequested = config.onStopRequested;
		this.sink = new BackgroundReadStateReducer({
			userId: config.account.userId,
			observeMentionCounts: (counts, mode) => this.observeMentionCounts(counts, mode),
			observeMessageNotification: (notification) => this.notifyMessage(notification),
		});
		this.dispatchQueue = new BackgroundGatewayDispatchQueue({
			isCurrentSocket: (socket) => this.socket === socket,
			persistHandoverAuthToken: (socket, delivery) => this.persistHandoverAuthToken(socket, delivery),
			processDispatch: (socket, delivery) => this.processDispatch(socket, delivery),
			handleQueueOverflow: (socket, delivery, error) => this.handleDispatchQueueOverflow(socket, delivery, error),
			handleQueueFailure: (error) => this.handleDispatchQueueFailure(error),
		});
		makeAutoObservable<
			this,
			| 'account'
			| 'gatewayEndpoint'
			| 'properties'
			| 'onStopRequested'
			| 'channelMentionCounts'
			| 'sink'
			| 'dispatchQueue'
			| 'socket'
			| 'ready'
			| 'authMutationPending'
			| 'promotionReadinessWait'
		>(
			this,
			{
				accountKey: false,
				account: false,
				gatewayEndpoint: false,
				properties: false,
				onStopRequested: false,
				channelMentionCounts: false,
				sink: false,
				dispatchQueue: false,
				socket: false,
				ready: false,
				authMutationPending: false,
				promotionReadinessWait: false,
			},
			{autoBind: true},
		);
	}

	connect(): void {
		if (this.socket !== null) {
			return;
		}
		const socket = new GatewaySocket(this.gatewayEndpoint, {
			apiVersion: DEFAULT_API_VERSION,
			token: this.account.token,
			properties: this.properties,
			presence: this.identifyPresence(),
			identifyFlags: GatewayIdentifyFlags.DEBOUNCE_MESSAGE_REACTIONS | GatewayIdentifyFlags.CHANNEL_THREADS,
			initialGuildId: null,
			isMobileLayout: () => false,
			geo: () => ({latitude: null, longitude: null}),
			role: GatewayConnectionRole.BACKGROUND,
		});
		this.socket = socket;
		this.ready = false;
		this.installSocketWiring(socket);
		socket.connect();
	}

	get isBackgroundConnectionReady(): boolean {
		const socket = this.socket;
		return (
			socket !== null &&
			this.ready &&
			!this.authMutationPending &&
			socket.isConnected() &&
			socket.getSessionId() !== null
		);
	}

	get backgroundSessionId(): string | null {
		return this.socket?.getSessionId() ?? null;
	}

	get hasPendingAuthMutation(): boolean {
		return this.authMutationPending;
	}

	waitForBackgroundConnectionReady(timeoutMs: number): Promise<boolean> {
		if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
			throw new Error(`Background gateway readiness timeout must be a positive integer, got ${timeoutMs}`);
		}
		if (this.isBackgroundConnectionReady) {
			return Promise.resolve(true);
		}
		if (this.socket === null) {
			return Promise.resolve(false);
		}
		const active = this.promotionReadinessWait;
		if (active !== null) {
			return active.promise;
		}
		const wait = createBackgroundGatewayReadinessWait();
		wait.timer = window.setTimeout(() => {
			wait.timer = null;
			this.settlePromotionReadiness(wait, false);
		}, timeoutMs);
		this.promotionReadinessWait = wait;
		return wait.promise;
	}

	beginBackgroundDispatchHandover(listener: (delivery: GatewayDispatchDelivery) => void): void {
		if (this.dispatchQueue.isHandoverActive) {
			throw new Error('Background gateway dispatch handover is already active');
		}
		if (!this.isBackgroundConnectionReady) {
			throw new Error('Background gateway dispatch handover requires a ready session');
		}
		this.dispatchQueue.beginHandover(listener);
	}

	endBackgroundDispatchHandover(request: BackgroundDispatchHandoverEnd): void {
		if (!this.dispatchQueue.isHandoverActive) {
			return;
		}
		const socket = this.socket;
		if (this.dispatchQueue.endHandover(request, socket) && socket !== null) {
			this.ready = false;
			this.sink.reset();
			socket.reset(true);
		}
	}

	waitForBackgroundDispatchIdle(): Promise<void> {
		return this.dispatchQueue.waitForIdle();
	}

	detachBackgroundSocket(): GatewaySocket | null {
		const socket = this.socket;
		if (socket === null) {
			return null;
		}
		this.dispatchQueue.assertDetachable();
		for (const event of BACKGROUND_SOCKET_EVENTS) {
			socket.removeAllListeners(event);
		}
		this.socket = null;
		this.ready = false;
		this.sink.reset();
		return socket;
	}

	captureMentionCounts(): Map<string, number> {
		return new Map(this.channelMentionCounts);
	}

	adoptSocket(socket: GatewaySocket, snapshot: StateSnapshotCapture, mentionCounts: ReadonlyMap<string, number>): void {
		if (this.socket !== null) {
			throw new Error('Background gateway socket adoption requires an empty session');
		}
		const sessionId = socket.getSessionId();
		if (
			sessionId === null ||
			!socket.isConnected() ||
			snapshot.cursor.sessionId !== sessionId ||
			snapshot.cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH
		) {
			throw new Error('Background gateway socket adoption requires a matching canonical session snapshot');
		}
		const presence = this.identifyPresence();
		socket.configureOwnership({
			role: GatewayConnectionRole.BACKGROUND,
			presence,
			initialGuildId: null,
			isMobileLayout: () => false,
			geo: () => ({latitude: null, longitude: null}),
		});
		this.socket = socket;
		this.ready = false;
		this.seedSinkFromSnapshot(snapshot.entries, mentionCounts);
		if (!this.ready) {
			this.socket = null;
			this.sink.reset();
			throw new Error('Background gateway socket adoption did not restore a READY session');
		}
		this.installSocketWiring(socket);
		socket.updatePresence(presence.status, presence.afk, presence.mobile, presence.custom_status);
	}

	stop(): void {
		this.settlePromotionReadiness(this.promotionReadinessWait, false);
		const socket = this.socket;
		this.socket = null;
		this.ready = false;
		this.dispatchQueue.stop();
		if (socket !== null) {
			for (const event of BACKGROUND_SOCKET_EVENTS) {
				socket.removeAllListeners(event);
			}
			try {
				socket.reset(false);
			} catch (error) {
				logger.warn('Failed to reset a background gateway socket cleanly', error);
			}
		}
		this.sink.reset();
	}

	refreshPresence(): void {
		const presence = this.identifyPresence();
		this.socket?.updatePresence(presence.status, presence.afk, presence.mobile);
	}

	probeAfterResume(): void {
		this.socket?.probeAfterResume('background-online', {accelerateReconnect: true});
	}

	private identifyPresence(): GatewayPresence {
		return {
			status: BackgroundAccountPresence.appearOffline ? StatusTypes.INVISIBLE : StatusTypes.ONLINE,
			afk: Idle.isAfk(),
			mobile: false,
		};
	}

	private installSocketWiring(socket: GatewaySocket): void {
		socket.on('dispatch', (delivery: GatewayDispatchDelivery) => {
			if (this.socket !== socket || !socket.isDispatchActive(delivery.receipt)) {
				return;
			}
			this.dispatchQueue.enqueue(socket, delivery);
		});
		socket.on('disconnect', (event: {code: number}) => {
			if (this.socket !== socket) {
				return;
			}
			this.ready = false;
			this.handleDisconnect(event.code);
		});
	}

	private handleDispatchQueueOverflow(socket: GatewaySocket, delivery: GatewayDispatchDelivery, error: Error): void {
		this.ready = false;
		this.sink.reset();
		socket.failDispatchProcessing(delivery.receipt, error);
	}

	private handleDispatchQueueFailure(error: unknown): void {
		if (error instanceof BackgroundReadStateCapacityError) {
			this.stopForReadStateCapacity(error);
			return;
		}
		logger.error('Background gateway dispatch queue failed', error);
		this.ready = false;
		this.sink.reset();
		this.socket?.reset(true);
	}

	private async processDispatch(socket: GatewaySocket, delivery: GatewayDispatchDelivery): Promise<void> {
		if (this.socket !== socket || !socket.isDispatchActive(delivery.receipt)) {
			return;
		}
		try {
			await this.handleDispatch(delivery);
		} catch (error) {
			if (this.socket === socket) {
				if (error instanceof BackgroundReadStateCapacityError) {
					this.stopForReadStateCapacity(error);
					return;
				}
				this.ready = false;
				this.sink.reset();
				socket.failDispatchProcessing(delivery.receipt, error);
			}
			return;
		}
		if (this.socket === socket && socket.isDispatchActive(delivery.receipt)) {
			socket.completeDispatchProcessing(delivery.receipt);
		}
	}

	private stopForReadStateCapacity(error: BackgroundReadStateCapacityError): void {
		logger.error('Stopping a background gateway session after read-state capacity exhaustion', error);
		this.stop();
		this.onStopRequested();
	}

	private async handleDispatch(delivery: GatewayDispatchDelivery): Promise<void> {
		const snapshotEvent = parseSnapshotGatewayEvent(delivery.type, delivery.data);
		if (delivery.type === 'READY') {
			if (snapshotEvent?.type !== 'READY') {
				throw new Error('Gateway READY dispatch did not produce a READY snapshot event');
			}
			this.handleReady(snapshotEvent.data);
			SnapshotSync.applyReady(this.accountKey, snapshotEvent.data, 'background');
			return;
		}
		if (delivery.type === 'RESUMED') {
			this.ready = true;
			this.settlePromotionReadiness(this.promotionReadinessWait, true);
		}
		if (delivery.type === 'AUTH_SESSION_CHANGE') {
			if (snapshotEvent?.type !== 'AUTH_SESSION_CHANGE') {
				throw new Error('Gateway AUTH_SESSION_CHANGE dispatch did not produce an authentication snapshot event');
			}
			if (delivery.persistedAuthTokenAccountKey !== this.accountKey) {
				await this.applyAuthSessionChange(snapshotEvent.data);
			}
			if (this.socket === null) {
				return;
			}
		}
		this.sink.applyDispatch(delivery.type, snapshotEvent?.data ?? delivery.data);
		if (snapshotEvent !== null && snapshotEvent.type !== 'READY') {
			SnapshotSync.applyDispatch(this.accountKey, snapshotEvent);
		}
	}

	private async persistHandoverAuthToken(
		socket: GatewaySocket,
		delivery: GatewayDispatchDelivery,
	): Promise<GatewayDispatchDelivery> {
		if (delivery.type !== 'AUTH_SESSION_CHANGE' || delivery.persistedAuthTokenAccountKey === this.accountKey) {
			return delivery;
		}
		const snapshotEvent = parseSnapshotGatewayEvent(delivery.type, delivery.data);
		if (snapshotEvent?.type !== 'AUTH_SESSION_CHANGE') {
			throw new Error('Gateway AUTH_SESSION_CHANGE dispatch did not produce an authentication snapshot event');
		}
		await this.applyAuthSessionChange(snapshotEvent.data);
		if (this.socket !== socket || !socket.isDispatchActive(delivery.receipt)) {
			return delivery;
		}
		return {...delivery, persistedAuthTokenAccountKey: this.accountKey};
	}

	private async applyAuthSessionChange(data: SnapshotAuthSessionChangePayload): Promise<void> {
		const newToken = data.new_token;
		if (newToken === undefined) {
			return;
		}
		if (typeof newToken !== 'string' || newToken.length === 0 || newToken.length > MAX_GATEWAY_AUTH_TOKEN_LENGTH) {
			throw new Error(`Background gateway received an invalid authentication token rotation for ${this.accountKey}`);
		}
		this.authMutationPending = true;
		try {
			const persisted = await SessionManager.setAccountToken(this.accountKey, newToken);
			if (!persisted) {
				throw new Error(`Background gateway token rotation was rejected for ${this.accountKey}`);
			}
			const updatedAccount = SessionManager.getAccount(this.accountKey);
			if (updatedAccount === null || updatedAccount.token !== newToken) {
				throw new Error(`Background gateway token rotation did not update ${this.accountKey}`);
			}
			this.account = updatedAccount;
			this.socket?.setToken(newToken);
		} finally {
			this.authMutationPending = false;
			if (this.isBackgroundConnectionReady) {
				this.settlePromotionReadiness(this.promotionReadinessWait, true);
			}
		}
	}

	private seedSinkFromSnapshot(entries: StateSnapshotEntries, mentionCounts: ReadonlyMap<string, number>): void {
		this.handleReady(buildBackgroundSnapshotReadySeed(entries, this.account.userId, mentionCounts));
	}

	private handleReady(data: ReadyPayload | BackgroundSnapshotReadySeed): void {
		const receivedUserId = data.user.id;
		if (receivedUserId !== this.account.userId) {
			const error = new BackgroundGatewayReadyIdentityMismatchError(this.account.userId, String(receivedUserId));
			logger.error('Stopping a background gateway session that identified as the wrong user', error);
			this.stop();
			this.onStopRequested();
			throw error;
		}
		this.sink.applyReady(data);
		this.ready = true;
		this.settlePromotionReadiness(this.promotionReadinessWait, true);
	}

	private settlePromotionReadiness(wait: BackgroundGatewayReadinessWait | null, ready: boolean): void {
		if (wait === null || this.promotionReadinessWait !== wait) {
			return;
		}
		this.promotionReadinessWait = null;
		if (wait.timer !== null) {
			clearTimeout(wait.timer);
			wait.timer = null;
		}
		wait.resolve(ready);
	}

	private handleDisconnect(code: number): void {
		if (code !== GatewayCloseCodes.AUTHENTICATION_FAILED) {
			return;
		}
		logger.warn('Background gateway authentication failed, stopping the session', this.accountKey);
		if (this.accountKey !== SessionManager.currentAccountKey) {
			SessionManager.markAccountInvalid(this.accountKey);
		}
		this.stop();
		this.onStopRequested();
	}

	private notifyMessage(notification: BackgroundMessageNotification): void {
		const accountKey = this.accountKey;
		void import('@app/features/notification/utils/BackgroundAccountNotifications')
			.then((module) => module.showBackgroundAccountNotification(accountKey, notification))
			.catch((error) => {
				logger.warn('Failed to show a background account notification', {accountKey, error});
			});
	}

	private observeMentionCounts(counts: ReadonlyMap<string, number>, mode: BackgroundMentionCountMode): void {
		if (mode === BackgroundMentionCountMode.REPLACE) {
			for (const channelId of [...this.channelMentionCounts.keys()]) {
				if (!counts.has(channelId)) {
					this.channelMentionCounts.delete(channelId);
				}
			}
		}
		for (const [channelId, count] of counts) {
			if (count > 0) {
				this.channelMentionCounts.set(channelId, count);
			} else {
				this.channelMentionCounts.delete(channelId);
			}
		}
		this.mentionCount = sumMentionCounts(this.channelMentionCounts.values());
	}
}
