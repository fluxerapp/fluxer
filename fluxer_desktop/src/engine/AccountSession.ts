// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {errorMessage} from '@electron/common/ErrorMessage';
import type {DispatchConsumer, DispatchPump} from '@electron/engine/DispatchPump';
import type {EngineClock} from '@electron/engine/EngineClock';
import {
	GatewayClient,
	type GatewayClientExit,
	GatewayClose,
	type GatewayCompressionMode,
	type GatewayResumeState,
} from '@electron/engine/GatewayClient';
import type {GatewayTransportFactory} from '@electron/engine/GatewayTransport';
import type {AdmittedGateway, EngineAccountRecord, EngineLogLevel} from '@electron/engine/HostProtocol';
import {SCHEDULER_LIMITS, type SchedulerClient, type SessionScheduler} from '@electron/engine/SessionScheduler';
import {
	ACCOUNT_ENGINE_LIMITS,
	type AccountEngineAccountSummary,
	AccountEngineCacheState,
	type AccountEngineClientConfig,
	AccountEngineConnectionState,
	type AccountEngineDemotion,
	AccountEngineErrorCode,
	type AccountEngineMention,
	type AccountEngineNotification,
	type AccountEnginePromotion,
	AccountEngineRequestError,
	type AccountEngineSnapshotCapture,
	type AccountEngineSubscriptionRelease,
	type AccountEngineUser,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {
	AccountEngineCore,
	AccountEngineCoreCounts,
	AccountEngineRowStore,
	CreateAccountEngineCore,
} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';

const INBOUND_QUEUE_MAX_BYTES = 32 * 1024 * 1024;
const NOTIFICATION_MAX_AGE_MS = 2 * 60 * 1000;
const NOTIFICATION_BURST = 5;
const NOTIFICATION_REFILL_MS = 1_000;
const INVALID_SESSION_MIN_DELAY_MS = 1_000;
const INVALID_SESSION_MAX_DELAY_MS = 5_000;
const DECODE_RETRY_DELAY_MS = 1_000;
const SUBSCRIPTION_RELEASE_SETTLE_MS = 250;

export interface AccountSessionDependencies {
	readonly clock: EngineClock;
	readonly scheduler: SessionScheduler;
	readonly pump: DispatchPump;
	readonly transport: GatewayTransportFactory;
	readonly store: AccountEngineRowStore;
	readonly createCore: CreateAccountEngineCore;
	readonly admit: (accountKey: string, url: string) => Promise<AdmittedGateway>;
	readonly config: () => AccountEngineClientConfig | null;
	readonly commitCache: () => void;
	readonly onSummaryChanged: (session: AccountSession) => void;
	readonly onNotification: (notification: AccountEngineNotification) => void;
	readonly onMention: (accountKey: string, mention: AccountEngineMention) => void;
	readonly onChannelRead: (accountKey: string, channelId: string) => void;
	readonly onAuthFailed: (accountKey: string, tokenTag: string) => void;
	readonly onTokenRotated: (accountKey: string, previousToken: string, token: string) => void;
	readonly log: (level: EngineLogLevel, message: string, detail?: unknown) => void;
}

interface InboundDispatch {
	readonly type: string;
	readonly data: unknown;
	readonly sequence: number | null;
	readonly bytes: number;
	readonly client: GatewayClient;
}

export interface PersistedAccountSummary extends AccountEngineAccountSummary {
	readonly mentionCounts?: Readonly<Record<string, number>>;
}

export function accountTokenTag(token: string): string {
	return createHash('sha256').update(token, 'utf8').digest('hex').slice(0, 32);
}

function estimateCaptureBytes(capture: AccountEngineSnapshotCapture): number {
	let bytes = 0;
	for (const [entity, rows] of Object.entries(capture.entries)) {
		bytes += entity.length;
		for (const [key, value] of Object.entries(rows)) {
			bytes += key.length + value.length;
		}
	}
	return bytes;
}

export class AccountSession implements SchedulerClient, DispatchConsumer {
	readonly accountKey: string;
	private record: EngineAccountRecord;
	private token: string;
	private readonly deps: AccountSessionDependencies;
	private readonly core: AccountEngineCore;
	private state: AccountEngineConnectionState = AccountEngineConnectionState.STARTING;
	private stateChangedAt: number;
	private nextRetryAt: number | null = null;
	private user: AccountEngineUser | null;
	private counts: AccountEngineCoreCounts = {
		mentionCount: 0,
		unreadChannelCount: 0,
		lastMessageAt: null,
		degraded: false,
	};
	private cache: AccountEngineCacheState = AccountEngineCacheState.EMPTY;
	private cacheUpdatedAt: number | null = null;
	private revision = 0;
	private client: GatewayClient | null = null;
	private connecting = false;
	private resume: GatewayResumeState | null = null;
	private handedOff: GatewayResumeState | null = null;
	private subscriptionRelease: AccountEngineSubscriptionRelease | null = null;
	private releaseSentAt: number | null = null;
	private admittedUrl: string | null = null;
	private compression: GatewayCompressionMode = 'zstd-stream';
	private attempt = 0;
	private queue: Array<InboundDispatch> = [];
	private queueHead = 0;
	private queueBytes = 0;
	private held = false;
	private stopped = false;
	private mutex: Promise<unknown> = Promise.resolve();
	private supersededToken: string | null = null;
	private failedTokenTag: string | null = null;
	private handshakeSucceeded = false;
	private handshakeStartedAt = 0;
	private invalidations = 0;
	private notificationTokens = NOTIFICATION_BURST;
	private notificationsRefilledAt: number;

	private readonly persistedMentionCounts: Readonly<Record<string, number>> | null;

	constructor(
		record: EngineAccountRecord,
		deps: AccountSessionDependencies,
		persisted: PersistedAccountSummary | null,
	) {
		this.accountKey = record.accountKey;
		this.record = record;
		this.token = record.token;
		this.deps = deps;
		this.stateChangedAt = deps.clock.now();
		this.notificationsRefilledAt = this.stateChangedAt;
		this.user = persisted?.user ?? null;
		this.persistedMentionCounts = persisted?.mentionCounts ?? null;
		if (persisted !== null) {
			this.cache = persisted.cache;
			this.cacheUpdatedAt = persisted.cacheUpdatedAt;
			this.counts = {
				mentionCount: persisted.mentionCount,
				unreadChannelCount: persisted.unreadChannelCount,
				lastMessageAt: persisted.lastMessageAt,
				degraded: persisted.degraded,
			};
		}
		this.core = deps.createCore({
			accountKey: record.accountKey,
			userId: record.userId,
			store: deps.store,
			onCountsChanged: (counts) => {
				this.counts = counts;
				this.touch();
			},
			onNotification: (notification) => {
				if (this.admitNotification(notification.message)) {
					this.deps.onNotification({...notification, accountKey: this.accountKey});
				}
			},
			onMention: (mention) => this.deps.onMention(this.accountKey, mention),
			onChannelRead: (channelId) => this.deps.onChannelRead(this.accountKey, channelId),
			onSelfUser: (user) => {
				if (JSON.stringify(user) !== JSON.stringify(this.user)) {
					this.user = user;
					this.touch();
				}
			},
			onTokenRotated: (token) => this.rotateToken(token),
			onInvalid: (error) => {
				this.invalidations += 1;
				this.deps.log('warn', 'Account engine cache invalidated', {accountKey: this.accountKey, error: error.message});
				this.cache = AccountEngineCacheState.STALE;
				this.touch();
			},
		});
	}

	get origin(): string {
		return this.record.instanceKey;
	}

	private get gatewayHost(): string {
		try {
			return new URL(this.record.gatewayEndpoint).host;
		} catch {
			return this.record.gatewayEndpoint;
		}
	}

	get priority(): number {
		return this.record.lastActive;
	}

	get resuming(): boolean {
		return this.resume !== null;
	}

	get connectionState(): AccountEngineConnectionState {
		return this.state;
	}

	get currentToken(): string {
		return this.token;
	}

	get resumeState(): GatewayResumeState | null {
		return this.resume;
	}

	get isHeld(): boolean {
		return this.held;
	}

	get authFailedTokenTag(): string | null {
		return this.state === AccountEngineConnectionState.AUTH_FAILED ? this.failedTokenTag : null;
	}

	start(options: {held: boolean}): void {
		this.held = options.held;
		try {
			if (this.core.seedFromStore(this.persistedMentionCounts)) {
				this.cache = AccountEngineCacheState.LIVE;
				this.cacheUpdatedAt = this.deps.store.readCursor(this.accountKey)?.updatedAt ?? null;
			}
		} catch (error) {
			this.deps.log('warn', 'Account engine could not seed from its cache', {
				accountKey: this.accountKey,
				error: String(error),
			});
			this.cache = AccountEngineCacheState.STALE;
		}
		if (!this.record.isValid) {
			this.setState(AccountEngineConnectionState.AUTH_FAILED);
			return;
		}
		if (this.held) {
			this.setState(AccountEngineConnectionState.FOREGROUND);
			return;
		}
		this.requestConnect();
	}

	serialize<T>(run: () => Promise<T> | T): Promise<T> {
		const next = this.mutex.then(run, run);
		this.mutex = next.catch(() => undefined);
		return next;
	}

	updateRecord(record: EngineAccountRecord): void {
		const previous = this.record;
		this.record = record;
		if (this.stopped) {
			return;
		}
		if (!record.isValid) {
			this.disconnect(GatewayClose.NORMAL, 'account invalid');
			this.resume = null;
			this.setState(AccountEngineConnectionState.AUTH_FAILED);
			return;
		}
		if (record.token === this.token) {
			this.supersededToken = null;
		}
		const tokenChanged = record.token !== this.token && record.token !== this.supersededToken;
		const endpointChanged = record.gatewayEndpoint !== previous.gatewayEndpoint;
		const recovered = this.state === AccountEngineConnectionState.AUTH_FAILED && (tokenChanged || !previous.isValid);
		if (tokenChanged) {
			this.token = record.token;
			this.supersededToken = null;
		}
		if (!tokenChanged && !endpointChanged && !recovered) {
			return;
		}
		this.resume = null;
		this.admittedUrl = null;
		if (this.held) {
			return;
		}
		this.disconnect(GatewayClose.NORMAL, 'account changed');
		this.attempt = 0;
		this.requestConnect();
	}

	configChanged(): void {
		if (this.state === AccountEngineConnectionState.STARTING && !this.held && !this.stopped && !this.connecting) {
			this.requestConnect();
		}
	}

	claimForeground(): void {
		if (this.held || this.stopped) {
			return;
		}
		this.deps.scheduler.cancel(this.accountKey);
		this.cancelRetryTimer();
		this.nextRetryAt = null;
		this.disconnect(GatewayClose.NORMAL, 'claimed by the renderer');
		this.resume = null;
		this.handedOff = null;
		this.subscriptionRelease = null;
		this.held = true;
		if (this.record.isValid) {
			this.setState(AccountEngineConnectionState.FOREGROUND);
		}
	}

	releaseHold(): void {
		if (!this.held || this.stopped) {
			return;
		}
		this.held = false;
		if (this.resume === null) {
			this.resume = this.handedOff;
		}
		this.handedOff = null;
		this.attempt = 0;
		if (this.record.isValid) {
			this.requestConnect();
		} else {
			this.setState(AccountEngineConnectionState.AUTH_FAILED);
		}
	}

	beginPromotion(): AccountEnginePromotion {
		if (this.stopped) {
			throw new AccountEngineRequestError(AccountEngineErrorCode.UNKNOWN_ACCOUNT, 'The account was removed');
		}
		this.deps.scheduler.cancel(this.accountKey);
		this.dropQueue();
		this.deps.commitCache();
		const session =
			this.resume !== null && this.admittedUrl !== null && this.subscriptionRelease === null
				? {sessionId: this.resume.sessionId, sequence: this.resume.sequence, gatewayUrl: this.admittedUrl}
				: null;
		this.handedOff = this.resume;
		this.resume = null;
		this.disconnect(session === null ? GatewayClose.NORMAL : GatewayClose.HANDOFF, 'handoff');
		this.held = true;
		this.setState(AccountEngineConnectionState.FOREGROUND);
		const capture = this.cache === AccountEngineCacheState.LIVE ? this.deps.store.capture(this.accountKey) : null;
		return {accountKey: this.accountKey, capture, mentionCounts: this.core.mentionCounts(), session};
	}

	settleSubscriptionRelease(): Promise<void> {
		const sentAt = this.releaseSentAt;
		this.releaseSentAt = null;
		const remainingMs = sentAt === null ? 0 : sentAt + SUBSCRIPTION_RELEASE_SETTLE_MS - this.deps.clock.now();
		if (remainingMs <= 0) {
			return Promise.resolve();
		}
		return new Promise((resolve) => {
			this.deps.clock.setTimeout(resolve, remainingMs);
		});
	}

	completePromotion(): void {
		this.handedOff = null;
	}

	abortPromotion(): void {
		if (!this.held || this.stopped) {
			return;
		}
		this.resume = this.handedOff;
		this.handedOff = null;
		this.held = false;
		this.attempt = 0;
		this.requestConnect();
	}

	async demote(demotion: AccountEngineDemotion): Promise<void> {
		if (this.stopped) {
			return;
		}
		this.handedOff = null;
		this.held = false;
		this.disconnect(GatewayClose.NORMAL, 'replaced by the renderer session');
		const capture = demotion.capture;
		if (capture !== null && estimateCaptureBytes(capture) <= ACCOUNT_ENGINE_LIMITS.demotionCaptureMaxBytes) {
			await new Promise((resolve) => setImmediate(resolve));
			if (this.stopped) {
				return;
			}
			try {
				this.core.replaceFromCapture(capture, demotion.mentionCounts);
				this.deps.commitCache();
				this.cache = AccountEngineCacheState.LIVE;
				this.cacheUpdatedAt = capture.cursor.updatedAt;
			} catch (error) {
				this.deps.log('warn', 'Account engine could not store a demotion capture', {
					accountKey: this.accountKey,
					error: String(error),
				});
				this.cache = AccountEngineCacheState.STALE;
			}
		}
		const session = demotion.session;
		this.resume = session === null ? null : {sessionId: session.sessionId, sequence: session.sequence};
		this.subscriptionRelease = session === null ? null : demotion.release;
		this.attempt = 0;
		if (this.record.isValid) {
			this.requestConnect();
		} else {
			this.setState(AccountEngineConnectionState.AUTH_FAILED);
		}
	}

	reconnect(): void {
		if (this.stopped || this.held || !this.record.isValid) {
			return;
		}
		this.attempt = 0;
		this.disconnect(GatewayClose.ZOMBIE, 'reconnect');
		this.requestConnect();
	}

	probe(): void {
		this.client?.probe();
	}

	retryNowIfWaiting(): void {
		if (this.state === AccountEngineConnectionState.BACKOFF || this.state === AccountEngineConnectionState.OFFLINE) {
			this.deps.scheduler.cancelRetry(this.accountKey);
			this.requestConnect();
		}
	}

	markOffline(): void {
		if (this.state === AccountEngineConnectionState.BACKOFF) {
			this.setState(AccountEngineConnectionState.OFFLINE);
		}
	}

	stop(code: number, reason: string, options: {dispose: boolean}): void {
		this.stopped = true;
		this.deps.scheduler.cancel(this.accountKey);
		this.dropQueue();
		this.deps.pump.forget(this);
		this.disconnect(code, reason);
		this.logGateway('Account engine gateway stopped', {reason});
		this.resume = null;
		this.setState(AccountEngineConnectionState.STOPPED);
		if (options.dispose) {
			this.core.dispose();
		}
	}

	mentionCounts(): Record<string, number> {
		return this.core.mentionCounts();
	}

	refreshUnread(): void {
		if (!this.stopped) {
			this.core.refreshUnread();
		}
	}

	setUnreadBadgeCustomization(enabled: boolean): void {
		if (!this.stopped) {
			this.core.setUnreadBadgeCustomization(enabled);
		}
	}

	activity(channelLimit: number) {
		return this.core.activity(channelLimit);
	}

	summary(): AccountEngineAccountSummary {
		return {
			accountKey: this.accountKey,
			instanceKey: this.record.instanceKey,
			userId: this.record.userId,
			user: this.user,
			state: this.state,
			stateChangedAt: this.stateChangedAt,
			nextRetryAt: this.nextRetryAt,
			mentionCount: this.counts.mentionCount,
			unreadChannelCount: this.counts.unreadChannelCount,
			lastMessageAt: this.counts.lastMessageAt,
			cache: this.cache,
			cacheUpdatedAt: this.cacheUpdatedAt,
			degraded: this.counts.degraded,
			revision: this.revision,
		};
	}

	hasPending(): boolean {
		return this.queueHead < this.queue.length;
	}

	processNext(): void {
		if (this.queueHead >= this.queue.length) {
			return;
		}
		const item = this.queue[this.queueHead];
		this.queueHead += 1;
		this.queueBytes -= item.bytes;
		if (this.queueHead >= this.queue.length) {
			this.queue = [];
			this.queueHead = 0;
			this.queueBytes = 0;
		}
		if (item.client !== this.client) {
			return;
		}
		this.applyInbound(item);
	}

	onSlotGranted(): void {
		void this.connect();
	}

	private applyInbound(item: InboundDispatch): void {
		try {
			if (item.type === 'READY') {
				const invalidations = this.invalidations;
				const {sessionId} = this.core.applyReady(item.data);
				this.resume = sessionId === null || item.sequence === null ? null : {sessionId, sequence: item.sequence};
				this.attempt = 0;
				this.handshakeSucceeded = true;
				if (this.invalidations === invalidations) {
					this.cache = AccountEngineCacheState.LIVE;
				}
				this.cacheUpdatedAt = this.deps.clock.now();
				this.setState(AccountEngineConnectionState.READY);
				this.logReady(false);
				return;
			}
			if (item.type === 'RESUMED') {
				this.attempt = 0;
				this.handshakeSucceeded = true;
				this.setState(AccountEngineConnectionState.READY);
				this.logReady(true);
			} else {
				this.core.applyDispatch(item.type, item.data);
			}
			if (this.resume !== null && item.sequence !== null) {
				this.resume = {sessionId: this.resume.sessionId, sequence: item.sequence};
			}
		} catch (error) {
			this.deps.log('warn', 'Account engine failed to reduce a dispatch', {
				accountKey: this.accountKey,
				type: item.type,
				error: errorMessage(error),
			});
			if (item.type === 'READY') {
				this.resume = null;
				this.disconnect(GatewayClose.NORMAL, 'ready rejected');
				this.scheduleBackoff(0);
			}
		}
	}

	private requestConnect(): void {
		if (this.stopped || this.held) {
			return;
		}
		if (this.deps.config() === null) {
			this.setState(AccountEngineConnectionState.STARTING);
			return;
		}
		this.cancelRetryTimer();
		this.nextRetryAt = null;
		if (this.state !== AccountEngineConnectionState.READY || this.client === null) {
			this.setState(
				this.resume !== null ? AccountEngineConnectionState.RESUMING : AccountEngineConnectionState.CONNECTING,
			);
		}
		this.deps.scheduler.requestSlot(this);
	}

	private async connect(): Promise<void> {
		const config = this.deps.config();
		if (this.stopped || this.held || config === null || this.client !== null || this.connecting) {
			this.deps.scheduler.releaseSlot(this.accountKey);
			return;
		}
		this.connecting = true;
		let admitted: AdmittedGateway;
		try {
			admitted = await this.deps.admit(this.accountKey, this.record.gatewayEndpoint);
		} catch (error) {
			this.connecting = false;
			this.deps.scheduler.releaseSlot(this.accountKey);
			this.deps.log('warn', 'Account engine gateway admission failed', {
				accountKey: this.accountKey,
				error: errorMessage(error),
			});
			if (!this.stopped && !this.held) {
				this.scheduleBackoff(0);
			}
			return;
		}
		this.connecting = false;
		if (this.stopped || this.held || this.client !== null) {
			this.deps.scheduler.releaseSlot(this.accountKey);
			return;
		}
		this.admittedUrl = admitted.url;
		this.handshakeSucceeded = false;
		const resume = this.resume;
		this.setState(resume !== null ? AccountEngineConnectionState.RESUMING : AccountEngineConnectionState.CONNECTING);
		let client: GatewayClient;
		try {
			client = this.createClient(admitted, resume, config);
		} catch (error) {
			this.deps.scheduler.releaseSlot(this.accountKey);
			this.deps.log('warn', 'Account engine could not open a gateway connection', {
				accountKey: this.accountKey,
				error: errorMessage(error),
			});
			this.scheduleBackoff(0);
			return;
		}
		this.client = client;
		this.handshakeStartedAt = this.deps.clock.now();
		client.start();
	}

	private createClient(
		admitted: AdmittedGateway,
		resume: GatewayResumeState | null,
		config: AccountEngineClientConfig,
	): GatewayClient {
		const client: GatewayClient = new GatewayClient({
			gatewayUrl: admitted.url,
			address: admitted.address,
			proxy: admitted.proxy,
			token: this.token,
			config,
			resume,
			compression: this.compression,
			transport: this.deps.transport,
			clock: this.deps.clock,
			handlers: {
				onDispatch: (type, data, sequence, bytes) => this.receive(client, type, data, sequence, bytes),
				onExit: (exit) => this.handleExit(client, exit),
			},
		});
		return client;
	}

	private receive(client: GatewayClient, type: string, data: unknown, sequence: number | null, bytes: number): void {
		if (client !== this.client) {
			return;
		}
		if (type === 'READY' || type === 'RESUMED') {
			this.deps.scheduler.releaseSlot(this.accountKey);
			const release = this.subscriptionRelease;
			this.subscriptionRelease = null;
			if (type === 'RESUMED' && release !== null) {
				client.releaseSubscriptions(release);
				this.releaseSentAt = this.deps.clock.now();
			}
		}
		if (this.queueBytes + bytes > INBOUND_QUEUE_MAX_BYTES && type !== 'READY') {
			this.deps.log('warn', 'Account engine inbound queue overflowed, resuming', {accountKey: this.accountKey});
			this.dropQueue();
			this.disconnect(GatewayClose.ZOMBIE, 'overflow');
			this.requestConnect();
			return;
		}
		this.queue.push({type, data, sequence, bytes, client});
		this.queueBytes += bytes;
		this.deps.pump.schedule(this);
	}

	private handleExit(client: GatewayClient, exit: GatewayClientExit): void {
		if (client !== this.client) {
			return;
		}
		this.client = null;
		this.deps.scheduler.releaseSlot(this.accountKey);
		this.dropQueue();
		if (this.stopped || this.held) {
			return;
		}
		this.logGateway('Account engine gateway disconnected', exit);
		switch (exit.kind) {
			case 'auth_failed':
				this.resume = null;
				this.failedTokenTag = accountTokenTag(this.token);
				this.setState(AccountEngineConnectionState.AUTH_FAILED);
				this.deps.onAuthFailed(this.accountKey, this.failedTokenTag);
				return;
			case 'rate_limited':
				this.scheduleBackoff(SCHEDULER_LIMITS.rateLimitFloorMs);
				return;
			case 'session_invalid': {
				if (!exit.resumable) {
					this.resume = null;
				}
				const delay =
					INVALID_SESSION_MIN_DELAY_MS +
					Math.floor(this.deps.clock.random() * (INVALID_SESSION_MAX_DELAY_MS - INVALID_SESSION_MIN_DELAY_MS));
				this.scheduleRetry(delay);
				return;
			}
			case 'reconnect_requested':
			case 'zombie':
				if (this.handshakeSucceeded) {
					this.requestConnect();
				} else {
					this.scheduleBackoff(SCHEDULER_LIMITS.backoffBaseMs);
				}
				return;
			case 'overflow':
				this.deps.log('warn', 'Account engine fell behind decoding, resuming', {accountKey: this.accountKey});
				this.scheduleBackoff(SCHEDULER_LIMITS.backoffBaseMs);
				return;
			case 'decode_error':
				this.compression = 'none';
				this.resume = null;
				this.scheduleRetry(DECODE_RETRY_DELAY_MS);
				return;
			case 'timeout':
				this.scheduleBackoff(0);
				return;
			case 'closed':
				if (!exit.resumable) {
					this.resume = null;
				}
				this.scheduleBackoff(0);
				return;
		}
	}

	private scheduleBackoff(floorMs: number): void {
		const delay = this.deps.scheduler.backoffDelay(this.attempt, floorMs);
		this.attempt += 1;
		this.scheduleRetry(delay);
	}

	private scheduleRetry(delayMs: number): void {
		if (this.stopped || this.held) {
			return;
		}
		this.cancelRetryTimer();
		this.nextRetryAt = this.deps.scheduler.scheduleRetry(this.accountKey, delayMs, () => {
			this.nextRetryAt = null;
			this.requestConnect();
		});
		this.setState(
			this.deps.scheduler.isOnline ? AccountEngineConnectionState.BACKOFF : AccountEngineConnectionState.OFFLINE,
		);
	}

	private cancelRetryTimer(): void {
		this.deps.scheduler.cancelRetry(this.accountKey);
	}

	private disconnect(code: number, reason: string): void {
		const client = this.client;
		this.client = null;
		this.dropQueue();
		this.deps.scheduler.releaseSlot(this.accountKey);
		if (client !== null && !this.stopped) {
			this.logGateway('Account engine gateway closed', {code, reason});
		}
		client?.close(code, reason);
	}

	private logReady(resumed: boolean): void {
		this.logGateway('Account engine gateway ready', {
			resumed,
			readyMs: this.deps.clock.now() - this.handshakeStartedAt,
		});
	}

	private logGateway(message: string, detail: object): void {
		this.deps.log('info', message, {accountKey: this.accountKey, host: this.gatewayHost, ...detail});
	}

	private dropQueue(): void {
		this.queue = [];
		this.queueHead = 0;
		this.queueBytes = 0;
	}

	private admitNotification(message: unknown): boolean {
		const now = this.deps.clock.now();
		const timestamp = (message as {timestamp?: unknown} | null)?.timestamp;
		const sentAt = typeof timestamp === 'string' ? Date.parse(timestamp) : Number.NaN;
		if (Number.isFinite(sentAt) && now - sentAt > NOTIFICATION_MAX_AGE_MS) {
			return false;
		}
		const refills = Math.floor((now - this.notificationsRefilledAt) / NOTIFICATION_REFILL_MS);
		if (refills > 0) {
			this.notificationTokens = Math.min(NOTIFICATION_BURST, this.notificationTokens + refills);
			this.notificationsRefilledAt += refills * NOTIFICATION_REFILL_MS;
		}
		if (this.notificationTokens === NOTIFICATION_BURST) {
			this.notificationsRefilledAt = now;
		}
		if (this.notificationTokens <= 0) {
			return false;
		}
		this.notificationTokens -= 1;
		return true;
	}

	private rotateToken(token: string): void {
		const previous = this.token;
		if (token === previous) {
			return;
		}
		this.token = token;
		this.supersededToken = previous;
		this.deps.onTokenRotated(this.accountKey, previous, token);
	}

	private setState(state: AccountEngineConnectionState): void {
		if (this.state === state) {
			return;
		}
		this.state = state;
		this.stateChangedAt = this.deps.clock.now();
		if (state === AccountEngineConnectionState.FOREGROUND) {
			this.logGateway('Account engine session moved to the foreground', {});
		}
		if (state !== AccountEngineConnectionState.BACKOFF && state !== AccountEngineConnectionState.OFFLINE) {
			this.nextRetryAt = null;
		}
		this.touch();
	}

	private touch(): void {
		this.revision += 1;
		this.deps.onSummaryChanged(this);
	}
}
