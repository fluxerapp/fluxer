// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountSession, accountTokenTag, type PersistedAccountSummary} from '@electron/engine/AccountSession';
import {CacheDatabase} from '@electron/engine/CacheDatabase';
import {DispatchPump} from '@electron/engine/DispatchPump';
import type {EngineClock, EngineTimer} from '@electron/engine/EngineClock';
import {GatewayClose} from '@electron/engine/GatewayClient';
import type {GatewayTransportFactory} from '@electron/engine/GatewayTransport';
import type {EngineMessagePort} from '@electron/engine/HostChannel';
import {
	ACCOUNT_ENGINE_HOST_PROTOCOL_VERSION,
	type AdmittedGateway,
	type EngineAccountRecord,
	type EngineInitMessage,
	type EngineLogLevel,
	type EnginePowerEvent,
	type EngineSessionCounts,
	type EngineToHostMessage,
	type HostToEngineMessage,
	parseEngineAccountRecord,
} from '@electron/engine/HostProtocol';
import {MentionLog} from '@electron/engine/MentionLog';
import {type AccountEngineHandlers, RendererPortServer} from '@electron/engine/RendererPortServer';
import {SessionScheduler} from '@electron/engine/SessionScheduler';
import {SqliteRowStore} from '@electron/engine/SqliteRowStore';
import {SummaryPublisher} from '@electron/engine/SummaryPublisher';
import {
	ACCOUNT_ENGINE_LIMITS,
	type AccountEngineAccountActivity,
	type AccountEngineAccountSummary,
	type AccountEngineClientConfig,
	AccountEngineConnectionState,
	AccountEngineErrorCode,
	type AccountEngineEvent,
	type AccountEngineMention,
	AccountEngineRequestError,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {CreateAccountEngineCore} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';

const HOST_REQUEST_TIMEOUT_MS = 30_000;
const FOREGROUND_HINT_HOLD_MS = 30_000;
const RENDERER_REATTACH_GRACE_MS = 30_000;
const HEALTH_INTERVAL_MS = 60_000;
const POWER_PROBE_STAGGER_MS = 50;
const CLIENT_CONFIG_META_KEY = 'client_config';
const UNREAD_BADGE_CUSTOMIZATION_META_KEY = 'unread_badge_customization';
const SEED_BATCH_SIZE = 4;

const SESSION_COUNTERS: Record<AccountEngineConnectionState, Exclude<keyof EngineSessionCounts, 'sessions'> | null> = {
	[AccountEngineConnectionState.STARTING]: 'connecting',
	[AccountEngineConnectionState.CONNECTING]: 'connecting',
	[AccountEngineConnectionState.RESUMING]: 'connecting',
	[AccountEngineConnectionState.READY]: 'ready',
	[AccountEngineConnectionState.BACKOFF]: 'backoff',
	[AccountEngineConnectionState.OFFLINE]: 'backoff',
	[AccountEngineConnectionState.FOREGROUND]: 'foreground',
	[AccountEngineConnectionState.AUTH_FAILED]: 'authFailed',
	[AccountEngineConnectionState.STOPPED]: null,
};

interface AccountEngineDependencies {
	readonly send: (message: EngineToHostMessage) => void;
	readonly clock: EngineClock;
	readonly transport: GatewayTransportFactory;
	readonly transportKind: 'native' | 'websocket';
	readonly transportError: string | null;
	readonly createCore: CreateAccountEngineCore;
	readonly snapshotSchemaEpoch: number;
	readonly exit: () => void;
	readonly memoryUsage: () => {rss: number; heapUsed: number};
}

interface RendererTokenRotation {
	readonly previousTokenTag: string;
	readonly token: string;
}

interface PendingHostRequest {
	readonly resolve: (value: HostToEngineMessage) => void;
	readonly reject: (error: Error) => void;
	readonly timer: EngineTimer;
}

function parseSummary(json: string): PersistedAccountSummary | null {
	try {
		const value = JSON.parse(json) as PersistedAccountSummary;
		return typeof value?.accountKey === 'string' ? value : null;
	} catch {
		return null;
	}
}

function parseClientConfig(json: string | null): AccountEngineClientConfig | null {
	if (json === null) {
		return null;
	}
	try {
		const value = JSON.parse(json) as AccountEngineClientConfig;
		if (
			typeof value?.apiVersion === 'number' &&
			typeof value.identifyFlags === 'number' &&
			typeof value.properties === 'object'
		) {
			return value;
		}
	} catch {}
	return null;
}

function parseAccountKeySet(json: string | null): Set<string> {
	if (json === null) {
		return new Set();
	}
	try {
		const value: unknown = JSON.parse(json);
		if (Array.isArray(value)) {
			return new Set(value.filter((entry): entry is string => typeof entry === 'string'));
		}
	} catch {}
	return new Set();
}

export class AccountEngine {
	private readonly deps: AccountEngineDependencies;
	private readonly clock: EngineClock;
	private readonly scheduler: SessionScheduler;
	private readonly pump = new DispatchPump();
	private readonly sessions = new Map<string, AccountSession>();
	private readonly pendingHost = new Map<number, PendingHostRequest>();
	private readonly persistedSummaries = new Map<string, PersistedAccountSummary>();
	private readonly rendererRotations = new Map<string, RendererTokenRotation>();
	private nextRequestId = 1;
	private cache: CacheDatabase | null = null;
	private store: SqliteRowStore | null = null;
	private mentionLog: MentionLog | null = null;
	private publisher: SummaryPublisher;
	private config: AccountEngineClientConfig | null = null;
	private renderer: RendererPortServer | null = null;
	private foregroundHint: string | null = null;
	private hintTimer: EngineTimer | null = null;
	private reattachTimer: EngineTimer | null = null;
	private readonly orphans = new Set<string>();
	private healthTimer: EngineTimer | null = null;
	private shuttingDown = false;
	private records = new Map<string, EngineAccountRecord>();
	private unreadBadgeCustomization = new Set<string>();

	constructor(deps: AccountEngineDependencies) {
		this.deps = deps;
		this.clock = deps.clock;
		this.scheduler = new SessionScheduler(deps.clock);
		this.publisher = this.createPublisher();
	}

	session(accountKey: string): AccountSession | null {
		return this.sessions.get(accountKey) ?? null;
	}

	handleHostMessage(message: HostToEngineMessage, port: EngineMessagePort | null): void {
		switch (message.type) {
			case 'init':
				this.init(message);
				return;
			case 'accounts':
				this.applyAccounts(message.accounts);
				return;
			case 'attachRenderer':
				if (port !== null) {
					this.attachRenderer(port);
				}
				return;
			case 'detachRenderer':
				this.detachRenderer();
				return;
			case 'admitResult':
			case 'httpResult':
				this.settleHostRequest(message.requestId, message);
				return;
			case 'tokenPersisted':
				this.tokenPersisted(message);
				return;
			case 'power':
				this.handlePower(message.event);
				return;
			case 'shutdown':
				this.shutdown();
				return;
		}
	}

	handlers(): AccountEngineHandlers {
		return {
			configure: (config) => this.configure(config),
			claimForeground: ({accountKey}) => this.claimForeground(accountKey),
			getSummaries: () => this.summaries(),
			getActivity: (params) => this.getActivity(params.accountKeys, params.channelLimit, params.mentionLimit),
			beginPromotion: ({accountKey}) => {
				const session = this.sessions.get(accountKey);
				if (session === undefined || this.store === null) {
					return null;
				}
				this.adoptForeground(accountKey);
				return session.serialize(async () => {
					await session.settleSubscriptionRelease();
					return session.beginPromotion();
				});
			},
			completePromotion: ({accountKey}) => {
				const session = this.requireSession(accountKey);
				return session.serialize(() => {
					session.completePromotion();
					return null;
				});
			},
			abortPromotion: ({accountKey}) => {
				const session = this.requireSession(accountKey);
				return session.serialize(() => {
					session.abortPromotion();
					return null;
				});
			},
			demote: (demotion) => {
				const session = this.requireSession(demotion.accountKey);
				return session.serialize(async () => {
					await session.demote(demotion);
					return null;
				});
			},
			ackChannel: (params) => this.ackChannel(params.accountKey, params.channelId, params.messageId),
			reconnect: ({accountKey}) => {
				this.requireSession(accountKey).reconnect();
				return null;
			},
			setUnreadBadgeCustomization: ({enabledAccountKeys}) => this.setUnreadBadgeCustomization(enabledAccountKeys),
		};
	}

	private createPublisher(): SummaryPublisher {
		return new SummaryPublisher({
			clock: this.clock,
			coalesceMs: ACCOUNT_ENGINE_LIMITS.summaryCoalesceMs,
			emit: (summaries) => {
				this.deps.send({type: 'summaries', summaries});
				this.emit({type: 'summaries', summaries});
			},
			persist: (summary) => {
				try {
					const session = this.sessions.get(summary.accountKey);
					const persisted: PersistedAccountSummary = {...summary, mentionCounts: session?.mentionCounts() ?? {}};
					this.cache?.writeSummary(summary.accountKey, JSON.stringify(persisted));
				} catch (error) {
					this.log('warn', 'Account engine could not persist a summary', String(error));
				}
			},
		});
	}

	private init(message: EngineInitMessage): void {
		if (this.cache !== null || message.protocol !== ACCOUNT_ENGINE_HOST_PROTOCOL_VERSION) {
			return;
		}
		const cache = new CacheDatabase(message.cachePath, this.deps.snapshotSchemaEpoch, this.clock);
		if (cache.openReport.reset !== 'none') {
			this.log('warn', `Account engine cache was reset (${cache.openReport.reset})`);
		}
		if (cache.openReport.memoryFallback !== null) {
			this.log('error', 'Account engine cache is in memory for this session', cache.openReport.memoryFallback);
		}
		this.cache = cache;
		this.store = new SqliteRowStore(cache);
		this.mentionLog = new MentionLog(cache);
		this.config = parseClientConfig(cache.getMeta(CLIENT_CONFIG_META_KEY));
		this.unreadBadgeCustomization = parseAccountKeySet(cache.getMeta(UNREAD_BADGE_CUSTOMIZATION_META_KEY));
		for (const json of cache.readSummaries().values()) {
			const summary = parseSummary(json);
			if (summary !== null) {
				this.persistedSummaries.set(summary.accountKey, summary);
			}
		}
		this.foregroundHint = message.foregroundHint;
		this.applyAccounts(message.accounts);
		this.deps.send({type: 'summaries', summaries: this.summaries()});
		if (message.accountsComplete !== false) {
			const known = new Set(this.records.keys());
			for (const accountKey of cache.knownAccountKeys()) {
				if (!known.has(accountKey)) {
					cache.purgeAccount(accountKey);
				}
			}
		}
		if (this.foregroundHint !== null) {
			this.hintTimer = this.clock.setTimeout(() => this.releaseForegroundHint(), FOREGROUND_HINT_HOLD_MS);
		}
		this.healthTimer = this.clock.setTimeout(() => this.reportHealth(), HEALTH_INTERVAL_MS);
		this.deps.send({type: 'ready', transport: this.deps.transportKind, transportError: this.deps.transportError});
	}

	private applyAccounts(input: ReadonlyArray<unknown>): void {
		if (this.cache === null) {
			return;
		}
		const next = new Map<string, EngineAccountRecord>();
		for (const value of input) {
			const record = parseEngineAccountRecord(value);
			if (record !== null) {
				next.set(record.accountKey, record);
			}
		}
		const removed: Array<string> = [];
		for (const accountKey of this.records.keys()) {
			if (!next.has(accountKey)) {
				removed.push(accountKey);
			}
		}
		this.records = next;
		for (const [accountKey, rotation] of this.rendererRotations) {
			const token = next.get(accountKey)?.token;
			if (token === undefined || token === rotation.token) {
				this.rendererRotations.delete(accountKey);
			}
		}
		for (const accountKey of removed) {
			this.removeAccount(accountKey);
		}
		const added: Array<AccountSession> = [];
		for (const record of next.values()) {
			const existing = this.sessions.get(record.accountKey);
			if (existing !== undefined) {
				void existing.serialize(() => existing.updateRecord(record));
				continue;
			}
			const session = this.createSession(record);
			session.setUnreadBadgeCustomization(this.unreadBadgeCustomization.has(record.accountKey));
			this.sessions.set(record.accountKey, session);
			this.publisher.seed(session.summary());
			added.push(session);
		}
		if (removed.length > 0) {
			this.deps.send({type: 'removed', accountKeys: removed});
			this.emit({type: 'removed', accountKeys: removed});
		}
		added.sort((left, right) => right.priority - left.priority);
		this.startGradually(added);
	}

	private startGradually(sessions: ReadonlyArray<AccountSession>): void {
		let index = 0;
		const step = (): void => {
			if (this.shuttingDown) {
				return;
			}
			const end = Math.min(sessions.length, index + SEED_BATCH_SIZE);
			for (; index < end; index += 1) {
				const session = sessions[index];
				if (this.sessions.get(session.accountKey) !== session) {
					continue;
				}
				session.start({held: session.accountKey === this.foregroundHint});
				this.publisher.update(session.summary());
			}
			if (index < sessions.length) {
				setImmediate(step);
			}
		};
		step();
	}

	private createSession(record: EngineAccountRecord): AccountSession {
		const store = this.store!;
		return new AccountSession(
			record,
			{
				clock: this.clock,
				scheduler: this.scheduler,
				pump: this.pump,
				transport: this.deps.transport,
				store,
				createCore: this.deps.createCore,
				admit: (accountKey, url) => this.admit(accountKey, url),
				config: () => this.config,
				commitCache: () => this.cache?.commit(),
				onSummaryChanged: (session) => {
					if (this.sessions.get(session.accountKey) === session) {
						this.publisher.update(session.summary());
					}
				},
				onNotification: (notification) => this.emit({type: 'notification', notification}),
				onMention: (accountKey, mention) => this.recordMention(accountKey, mention),
				onChannelRead: (accountKey, channelId) => this.emit({type: 'channel-read', accountKey, channelId}),
				onAuthFailed: (accountKey, tokenTag) => this.emit({type: 'auth-failed', accountKey, tokenTag}),
				onTokenRotated: (accountKey, previousToken, token) =>
					this.deps.send({type: 'tokenRotated', accountKey, previousToken, token}),
				log: (level, message, detail) => this.log(level, message, detail),
			},
			this.persistedSummaries.get(record.accountKey) ?? null,
		);
	}

	private removeAccount(accountKey: string): void {
		const session = this.sessions.get(accountKey);
		this.sessions.delete(accountKey);
		this.publisher.remove(accountKey);
		this.persistedSummaries.delete(accountKey);
		if (session !== undefined) {
			session.stop(GatewayClose.NORMAL, 'signed out', {dispose: true});
		}
		this.cache?.purgeAccount(accountKey);
		if (this.unreadBadgeCustomization.delete(accountKey)) {
			this.persistUnreadBadgeCustomization();
		}
	}

	private recordMention(accountKey: string, mention: AccountEngineMention): void {
		try {
			this.mentionLog?.record(accountKey, mention);
		} catch (error) {
			this.log('warn', 'Account engine could not record a mention', String(error));
		}
	}

	private configure(config: AccountEngineClientConfig): null {
		this.config = config;
		this.cache?.setMeta(CLIENT_CONFIG_META_KEY, JSON.stringify(config));
		for (const session of this.sessions.values()) {
			session.configChanged();
		}
		return null;
	}

	private setUnreadBadgeCustomization(enabledAccountKeys: ReadonlyArray<string>): null {
		this.unreadBadgeCustomization = new Set(enabledAccountKeys);
		this.persistUnreadBadgeCustomization();
		for (const session of this.sessions.values()) {
			session.setUnreadBadgeCustomization(this.unreadBadgeCustomization.has(session.accountKey));
		}
		return null;
	}

	private persistUnreadBadgeCustomization(): void {
		this.cache?.setMeta(UNREAD_BADGE_CUSTOMIZATION_META_KEY, JSON.stringify([...this.unreadBadgeCustomization]));
	}

	private claimForeground(accountKey: string | null): null {
		this.adoptForeground(accountKey);
		if (accountKey === null) {
			return null;
		}
		const session = this.sessions.get(accountKey);
		if (session === undefined) {
			this.foregroundHint = accountKey;
			return null;
		}
		void session.serialize(() => session.claimForeground());
		return null;
	}

	private adoptForeground(accountKey: string | null): void {
		if (this.reattachTimer !== null) {
			this.clock.clearTimeout(this.reattachTimer);
			this.reattachTimer = null;
		}
		if (accountKey !== this.foregroundHint) {
			this.releaseForegroundHint();
		} else if (this.hintTimer !== null) {
			this.clock.clearTimeout(this.hintTimer);
			this.hintTimer = null;
		}
		if (accountKey !== null) {
			this.orphans.delete(accountKey);
		}
		this.releaseOrphans();
	}

	private releaseOrphans(): void {
		const orphans = Array.from(this.orphans);
		this.orphans.clear();
		for (const accountKey of orphans) {
			const session = this.sessions.get(accountKey);
			if (session !== undefined && accountKey !== this.foregroundHint) {
				void session.serialize(() => session.releaseHold());
			}
		}
	}

	private collectOrphans(): void {
		for (const session of this.sessions.values()) {
			if (session.isHeld) {
				this.orphans.add(session.accountKey);
			}
		}
	}

	private releaseForegroundHint(): void {
		if (this.hintTimer !== null) {
			this.clock.clearTimeout(this.hintTimer);
			this.hintTimer = null;
		}
		const hint = this.foregroundHint;
		this.foregroundHint = null;
		if (hint !== null) {
			this.sessions.get(hint)?.releaseHold();
		}
	}

	private summaries(): Array<AccountEngineAccountSummary> {
		return Array.from(this.sessions.values(), (session) => session.summary());
	}

	private getActivity(
		accountKeys: ReadonlyArray<string> | null,
		channelLimit: number,
		mentionLimit: number,
	): Array<AccountEngineAccountActivity> {
		const keys = accountKeys ?? Array.from(this.sessions.keys());
		const result: Array<AccountEngineAccountActivity> = [];
		for (const accountKey of keys) {
			const session = this.sessions.get(accountKey);
			if (session === undefined) {
				continue;
			}
			try {
				const activity = session.activity(channelLimit);
				result.push({
					accountKey,
					channels: activity.channels,
					totalChannelCount: activity.totalChannelCount,
					mentions: this.mentionLog?.list(accountKey, mentionLimit) ?? [],
				});
			} catch (error) {
				this.log('warn', 'Account engine could not build activity', String(error));
			}
		}
		return result;
	}

	private async ackChannel(accountKey: string, channelId: string, messageId: string): Promise<boolean> {
		const session = this.requireSession(accountKey);
		const response = await this.hostRequest({
			type: 'http',
			requestId: 0,
			accountKey,
			method: 'POST',
			path: `/channels/${channelId}/messages/${messageId}/ack`,
			body: '{}',
		});
		if (response.type !== 'httpResult' || !response.ok) {
			return false;
		}
		if (this.sessions.get(accountKey) === session) {
			this.emit({type: 'channel-read', accountKey, channelId});
		}
		return true;
	}

	private async admit(accountKey: string, url: string): Promise<AdmittedGateway> {
		const response = await this.hostRequest({type: 'admit', requestId: 0, accountKey, url});
		if (response.type !== 'admitResult' || !response.ok || response.url === null) {
			throw new Error(response.type === 'admitResult' ? (response.error ?? 'Admission refused') : 'Admission refused');
		}
		return {url: response.url, address: response.address, proxy: response.proxy};
	}

	private hostRequest(message: Extract<EngineToHostMessage, {type: 'admit' | 'http'}>): Promise<HostToEngineMessage> {
		const requestId = this.nextRequestId++;
		return new Promise<HostToEngineMessage>((resolve, reject) => {
			const timer = this.clock.setTimeout(() => {
				this.pendingHost.delete(requestId);
				reject(new AccountEngineRequestError(AccountEngineErrorCode.TIMEOUT, 'The desktop host did not answer'));
			}, HOST_REQUEST_TIMEOUT_MS);
			this.pendingHost.set(requestId, {resolve, reject, timer});
			this.deps.send({...message, requestId});
		});
	}

	private settleHostRequest(requestId: number, message: HostToEngineMessage): void {
		const pending = this.pendingHost.get(requestId);
		if (pending === undefined) {
			return;
		}
		this.pendingHost.delete(requestId);
		this.clock.clearTimeout(pending.timer);
		pending.resolve(message);
	}

	private requireSession(accountKey: string): AccountSession {
		const session = this.sessions.get(accountKey);
		if (session === undefined) {
			throw new AccountEngineRequestError(AccountEngineErrorCode.UNKNOWN_ACCOUNT, 'Unknown account');
		}
		return session;
	}

	private attachRenderer(port: EngineMessagePort): void {
		this.renderer?.close();
		this.collectOrphans();
		this.scheduleHeldRelease();
		let server: RendererPortServer;
		server = new RendererPortServer(port, this.handlers(), () => {
			if (this.renderer === server) {
				this.renderer = null;
				this.scheduleHeldRelease();
			}
		});
		this.renderer = server;
		server.emit({type: 'summaries', summaries: this.summaries()});
		for (const session of this.sessions.values()) {
			const tokenTag = session.authFailedTokenTag;
			if (tokenTag !== null) {
				server.emit({type: 'auth-failed', accountKey: session.accountKey, tokenTag});
			}
		}
		for (const [accountKey, rotation] of this.rendererRotations) {
			server.emit({type: 'account-token-rotated', accountKey, ...rotation});
		}
	}

	private tokenPersisted(message: Extract<HostToEngineMessage, {type: 'tokenPersisted'}>): void {
		const {accountKey, previousToken, token, outcome} = message;
		if (outcome === 'persisted') {
			this.rendererRotations.delete(accountKey);
			this.emit({type: 'account-record-changed', accountKey});
			return;
		}
		if (outcome !== 'renderer' || this.sessions.get(accountKey)?.currentToken !== token) {
			return;
		}
		const pending = this.rendererRotations.get(accountKey);
		const rotation: RendererTokenRotation = {
			previousTokenTag:
				pending !== undefined && pending.token === previousToken
					? pending.previousTokenTag
					: accountTokenTag(previousToken),
			token,
		};
		this.rendererRotations.set(accountKey, rotation);
		this.emit({type: 'account-token-rotated', accountKey, ...rotation});
	}

	private detachRenderer(): void {
		const renderer = this.renderer;
		this.renderer = null;
		renderer?.close();
		this.collectOrphans();
		this.scheduleHeldRelease();
	}

	private scheduleHeldRelease(): void {
		if (this.reattachTimer !== null || this.shuttingDown || this.orphans.size === 0) {
			return;
		}
		this.reattachTimer = this.clock.setTimeout(() => {
			this.reattachTimer = null;
			if (this.foregroundHint !== null && this.orphans.has(this.foregroundHint)) {
				this.foregroundHint = null;
			}
			this.releaseOrphans();
		}, RENDERER_REATTACH_GRACE_MS);
	}

	private handlePower(event: EnginePowerEvent): void {
		switch (event) {
			case 'offline':
				this.scheduler.setOnline(false);
				for (const session of this.sessions.values()) {
					session.markOffline();
				}
				return;
			case 'online':
				this.scheduler.setOnline(true);
				this.probeAll();
				return;
			case 'resume':
			case 'unlock-screen':
				this.probeAll();
				return;
			case 'suspend':
			case 'lock-screen':
				return;
		}
	}

	private probeAll(): void {
		let index = 0;
		for (const session of this.sessions.values()) {
			const state = session.connectionState;
			if (state === AccountEngineConnectionState.READY) {
				this.clock.setTimeout(() => session.probe(), index * POWER_PROBE_STAGGER_MS);
				index += 1;
			} else if (state === AccountEngineConnectionState.BACKOFF || state === AccountEngineConnectionState.OFFLINE) {
				session.retryNowIfWaiting();
			}
		}
	}

	private reportHealth(): void {
		this.healthTimer = null;
		if (this.shuttingDown) {
			return;
		}
		const counts = {sessions: this.sessions.size, ready: 0, connecting: 0, backoff: 0, foreground: 0, authFailed: 0};
		for (const session of this.sessions.values()) {
			session.refreshUnread();
			const counter = SESSION_COUNTERS[session.connectionState];
			if (counter !== null) {
				counts[counter] += 1;
			}
		}
		const usage = this.deps.memoryUsage();
		this.deps.send({type: 'health', rss: usage.rss, heapUsed: usage.heapUsed, counts});
		this.healthTimer = this.clock.setTimeout(() => this.reportHealth(), HEALTH_INTERVAL_MS);
	}

	private shutdown(): void {
		if (this.shuttingDown) {
			return;
		}
		this.shuttingDown = true;
		for (const timer of [this.hintTimer, this.reattachTimer, this.healthTimer]) {
			if (timer !== null) {
				this.clock.clearTimeout(timer);
			}
		}
		for (const session of this.sessions.values()) {
			session.stop(GatewayClose.NORMAL, 'shutdown', {dispose: false});
		}
		this.scheduler.dispose();
		this.publisher.persistNow();
		this.renderer?.close();
		this.renderer = null;
		try {
			this.cache?.close();
		} catch (error) {
			this.log('warn', 'Account engine cache close failed', String(error));
		}
		this.deps.send({type: 'shutdownComplete'});
		this.deps.exit();
	}

	private emit(event: AccountEngineEvent): void {
		this.renderer?.emit(event);
	}

	private log(level: EngineLogLevel, message: string, detail?: unknown): void {
		let serialized: string | null = null;
		if (detail !== undefined) {
			try {
				serialized = typeof detail === 'string' ? detail : JSON.stringify(detail);
			} catch {
				serialized = String(detail);
			}
		}
		this.deps.send({type: 'log', level, message, detail: serialized});
	}
}
