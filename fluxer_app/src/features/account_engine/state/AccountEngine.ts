// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	accountEngineTokenTag,
	readAccountEngineErrorCode,
	resolveAccountEngineApi,
} from '@app/features/account_engine/state/AccountEngineClient';
import SessionManager from '@app/features/platform/state/AuthSession';
import type {Account} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import {desktopLegacyImportIsComplete} from '@app/features/platform/state/DesktopLegacyImportState';
import {flushAppStorageWrites} from '@app/features/platform/state/PersistentStorage';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getGatewayClientProperties} from '@app/features/platform/utils/ClientInfo';
import AdvancedSettings, {
	parseStoredBoolean,
	UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY,
} from '@app/features/user/state/AdvancedSettings';
import {DEFAULT_API_VERSION} from '@fluxer/constants/src/AppConstants';
import {GatewayIdentifyFlags} from '@fluxer/constants/src/GatewayConstants';
import {
	ACCOUNT_ENGINE_LIMITS,
	type AccountEngineAccountActivity,
	type AccountEngineAccountSummary,
	type AccountEngineClientConfig,
	AccountEngineErrorCode,
	type AccountEngineEvent,
	type AccountEngineNotification,
	type AccountEngineProvidedAccount,
	type AccountEngineRendererAPI,
	type AccountEngineRequestMethod,
	type AccountEngineRequests,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {
	action,
	compareStructural,
	computed,
	makeObservable,
	type ObservableMap,
	observable,
	reaction,
	runInAction,
} from 'mobx';

const logger = new Logger('AccountEngine');

const ACCOUNT_ENGINE_IDENTIFY_FLAGS =
	GatewayIdentifyFlags.DEBOUNCE_MESSAGE_REACTIONS |
	GatewayIdentifyFlags.CHANNEL_THREADS |
	GatewayIdentifyFlags.GUILD_VISITS;

interface AccountEngineHost {
	resolveApi(): AccountEngineRendererAPI | null;
	currentAccountKey(): string | null;
	expireAccount(accountKey: string, tokenTag: string): Promise<void>;
	reloadAccount(accountKey: string): Promise<void>;
	adoptRotatedToken(accountKey: string, previousTokenTag: string, token: string): Promise<void>;
	loadClientConfig(): Promise<AccountEngineClientConfig>;
	providedAccounts(): ReadonlyArray<AccountEngineProvidedAccount> | null;
	unreadBadgeCustomizationInputs(): string;
	loadUnreadBadgeCustomization(): Promise<ReadonlyArray<string>>;
}

interface LoadAccountActivityOptions {
	readonly accountKeys?: ReadonlyArray<string>;
	readonly channelLimit: number;
	readonly mentionLimit: number;
}

type NotificationListener = (notification: AccountEngineNotification) => void;
type ChannelReadListener = (accountKey: string, channelId: string) => void;

function clampLimit(value: number, max: number): number {
	if (!Number.isFinite(value) || value <= 0) {
		return 0;
	}
	return Math.min(Math.floor(value), max);
}

function hasActivity(summary: AccountEngineAccountSummary): boolean {
	return summary.mentionCount > 0 || summary.unreadChannelCount > 0;
}

function compareActivity(left: AccountEngineAccountSummary, right: AccountEngineAccountSummary): number {
	const mentionOrder = Number(right.mentionCount > 0) - Number(left.mentionCount > 0);
	return mentionOrder !== 0 ? mentionOrder : (right.lastMessageAt ?? 0) - (left.lastMessageAt ?? 0);
}

class AccountEngineStore {
	private readonly summaryMap: ObservableMap<string, AccountEngineAccountSummary> = observable.map(undefined, {
		deep: false,
	});
	private api: AccountEngineRendererAPI | null | undefined = undefined;
	private down = false;
	private started = false;
	private configured: Promise<void> = Promise.resolve();
	private unsubscribe: (() => void) | null = null;
	private disposeAccountFeed: (() => void) | null = null;
	private disposeUnreadBadgeFeed: (() => void) | null = null;
	private unreadBadgeSyncGeneration = 0;
	private readonly notificationListeners = new Set<NotificationListener>();
	private readonly channelReadListeners = new Set<ChannelReadListener>();

	constructor(private readonly host: AccountEngineHost) {
		makeObservable<AccountEngineStore, 'down' | 'applySummaries' | 'removeSummaries' | 'replaceSummaries'>(this, {
			down: observable,
			available: computed,
			summaries: computed,
			otherAccountsMentionCount: computed,
			otherAccountsHaveUnread: computed,
			otherAccountsWithActivity: computed,
			applySummaries: action,
			removeSummaries: action,
			replaceSummaries: action,
		});
	}

	private get engineApi(): AccountEngineRendererAPI | null {
		if (this.api === undefined) {
			this.api = this.host.resolveApi();
		}
		return this.api;
	}

	get available(): boolean {
		return this.engineApi !== null && !this.down;
	}

	get summaries(): ReadonlyMap<string, AccountEngineAccountSummary> {
		return this.available ? this.summaryMap : new Map();
	}

	getSummary(accountKey: string): AccountEngineAccountSummary | null {
		return this.summaries.get(accountKey) ?? null;
	}

	getMentionCount(accountKey: string): number {
		if (accountKey === this.host.currentAccountKey()) {
			return 0;
		}
		return this.getSummary(accountKey)?.mentionCount ?? 0;
	}

	hasUnread(accountKey: string): boolean {
		if (accountKey === this.host.currentAccountKey()) {
			return false;
		}
		const summary = this.getSummary(accountKey);
		return summary !== null && hasActivity(summary);
	}

	get otherAccountsMentionCount(): number {
		let total = 0;
		for (const summary of this.otherSummaries()) {
			total += summary.mentionCount;
		}
		return total;
	}

	get otherAccountsWithMentionsCount(): number {
		return this.otherSummaries().filter((summary) => summary.mentionCount > 0).length;
	}

	get otherAccountsHaveUnread(): boolean {
		return this.otherSummaries().some(hasActivity);
	}

	get otherAccountsWithActivity(): ReadonlyArray<AccountEngineAccountSummary> {
		return this.otherSummaries().filter(hasActivity).sort(compareActivity);
	}

	start(): void {
		if (this.started) {
			return;
		}
		this.started = true;
		const api = this.engineApi;
		if (api === null) {
			return;
		}
		this.unsubscribe = api.onEvent((event) => this.handleEvent(event));
		this.disposeAccountFeed = reaction(
			() => this.host.providedAccounts(),
			(accounts) => {
				if (accounts === null) return;
				void api.provideAccounts(accounts).catch((error: unknown) => {
					logger.warn('Failed to hand the signed-in accounts to the account engine', error);
				});
			},
			{fireImmediately: true, equals: compareStructural},
		);
		this.disposeUnreadBadgeFeed = reaction(
			() => this.host.unreadBadgeCustomizationInputs(),
			() => void this.syncUnreadBadgeCustomization(),
		);
		this.resynchronize(false);
	}

	stop(): void {
		this.unsubscribe?.();
		this.unsubscribe = null;
		this.disposeAccountFeed?.();
		this.disposeAccountFeed = null;
		this.disposeUnreadBadgeFeed?.();
		this.disposeUnreadBadgeFeed = null;
		this.started = false;
	}

	whenConfigured(): Promise<void> {
		return this.configured;
	}

	async request<M extends AccountEngineRequestMethod>(
		method: M,
		params: AccountEngineRequests[M]['params'],
	): Promise<AccountEngineRequests[M]['result']> {
		const api = this.engineApi;
		if (api === null || this.down) {
			throw new Error(`Account engine is unavailable for ${method}`);
		}
		try {
			return await api.request(method, params);
		} catch (error) {
			if (readAccountEngineErrorCode(error) === AccountEngineErrorCode.UNAVAILABLE) {
				this.markDown();
			}
			throw error;
		}
	}

	async loadActivity(options: LoadAccountActivityOptions): Promise<ReadonlyArray<AccountEngineAccountActivity>> {
		if (!this.available) {
			return [];
		}
		const currentAccountKey = this.host.currentAccountKey();
		const accountKeys =
			options.accountKeys === undefined
				? null
				: options.accountKeys.filter((accountKey) => accountKey !== currentAccountKey);
		if (accountKeys !== null && accountKeys.length === 0) {
			return [];
		}
		try {
			const activity = await this.request('getActivity', {
				accountKeys,
				channelLimit: clampLimit(options.channelLimit, ACCOUNT_ENGINE_LIMITS.activityChannelLimitMax),
				mentionLimit: clampLimit(options.mentionLimit, ACCOUNT_ENGINE_LIMITS.activityMentionLimitMax),
			});
			const latestAccountKey = this.host.currentAccountKey();
			return activity.filter((entry) => entry.accountKey !== latestAccountKey);
		} catch (error) {
			logger.warn('Failed to load activity from other accounts', error);
			return [];
		}
	}

	async ackChannel(accountKey: string, channelId: string, messageId: string): Promise<boolean> {
		if (!this.available) {
			return false;
		}
		try {
			return await this.request('ackChannel', {accountKey, channelId, messageId});
		} catch (error) {
			logger.warn('Failed to mark a channel on another account as read', {accountKey, channelId, error});
			return false;
		}
	}

	async reconnect(accountKey: string): Promise<void> {
		if (!this.available) {
			return;
		}
		try {
			await this.request('reconnect', {accountKey});
		} catch (error) {
			logger.warn('Failed to reconnect another account', {accountKey, error});
		}
	}

	onNotification(listener: NotificationListener): () => void {
		this.notificationListeners.add(listener);
		return () => {
			this.notificationListeners.delete(listener);
		};
	}

	onChannelRead(listener: ChannelReadListener): () => void {
		this.channelReadListeners.add(listener);
		return () => {
			this.channelReadListeners.delete(listener);
		};
	}

	private otherSummaries(): Array<AccountEngineAccountSummary> {
		const currentAccountKey = this.host.currentAccountKey();
		const others: Array<AccountEngineAccountSummary> = [];
		for (const [accountKey, summary] of this.summaries) {
			if (accountKey !== currentAccountKey) {
				others.push(summary);
			}
		}
		return others;
	}

	private handleEvent(event: AccountEngineEvent): void {
		switch (event.type) {
			case 'summaries':
				this.applySummaries(event.summaries);
				return;
			case 'removed':
				this.removeSummaries(event.accountKeys);
				return;
			case 'notification':
				this.emit(this.notificationListeners, (listener) => listener(event.notification));
				return;
			case 'channel-read':
				this.emit(this.channelReadListeners, (listener) => listener(event.accountKey, event.channelId));
				return;
			case 'auth-failed':
				void this.host.expireAccount(event.accountKey, event.tokenTag).catch((error: unknown) => {
					logger.warn('Failed to mark an expired account from the account engine', {
						accountKey: event.accountKey,
						error,
					});
				});
				return;
			case 'account-record-changed':
				void this.host.reloadAccount(event.accountKey).catch((error: unknown) => {
					logger.warn('Failed to reload an account record changed by the account engine', {
						accountKey: event.accountKey,
						error,
					});
				});
				return;
			case 'account-token-rotated':
				void this.host
					.adoptRotatedToken(event.accountKey, event.previousTokenTag, event.token)
					.catch((error: unknown) => {
						logger.warn('Failed to save a token rotated by the account engine', {
							accountKey: event.accountKey,
							error,
						});
					});
				return;
			case 'restarted':
				runInAction(() => {
					this.down = false;
				});
				this.replaceSummaries([]);
				this.resynchronize(true);
				return;
		}
	}

	private emit<L>(listeners: ReadonlySet<L>, invoke: (listener: L) => void): void {
		for (const listener of Array.from(listeners)) {
			try {
				invoke(listener);
			} catch (error) {
				logger.error('Account engine listener threw', error);
			}
		}
	}

	private resynchronize(claimForeground: boolean): void {
		this.configured = this.configure();
		void this.configured.then(async () => {
			if (claimForeground) {
				try {
					await this.request('claimForeground', {accountKey: this.host.currentAccountKey()});
				} catch (error) {
					logger.warn('Failed to claim the foreground account after the account engine restarted', error);
				}
			}
			void this.syncUnreadBadgeCustomization();
			await this.refreshSummaries();
		});
	}

	private async configure(): Promise<void> {
		try {
			const config = await this.host.loadClientConfig();
			await this.request('configure', config);
		} catch (error) {
			logger.warn('Failed to configure the account engine', error);
		}
	}

	private async syncUnreadBadgeCustomization(): Promise<void> {
		const generation = ++this.unreadBadgeSyncGeneration;
		try {
			const enabledAccountKeys = await this.host.loadUnreadBadgeCustomization();
			if (generation !== this.unreadBadgeSyncGeneration) {
				return;
			}
			await this.request('setUnreadBadgeCustomization', {enabledAccountKeys});
		} catch (error) {
			logger.warn('Failed to hand the unread badge settings to the account engine', error);
		}
	}

	private async refreshSummaries(): Promise<void> {
		try {
			this.replaceSummaries(await this.request('getSummaries', null));
		} catch (error) {
			logger.warn('Failed to load account summaries from the account engine', error);
		}
	}

	private applySummaries(summaries: ReadonlyArray<AccountEngineAccountSummary>): void {
		for (const summary of summaries) {
			const existing = this.summaryMap.get(summary.accountKey);
			if (existing !== undefined && existing.revision > summary.revision) {
				continue;
			}
			this.summaryMap.set(summary.accountKey, summary);
		}
	}

	private removeSummaries(accountKeys: ReadonlyArray<string>): void {
		for (const accountKey of accountKeys) {
			this.summaryMap.delete(accountKey);
		}
	}

	private replaceSummaries(summaries: ReadonlyArray<AccountEngineAccountSummary>): void {
		const incoming = new Set(summaries.map((summary) => summary.accountKey));
		for (const accountKey of Array.from(this.summaryMap.keys())) {
			if (!incoming.has(accountKey)) {
				this.summaryMap.delete(accountKey);
			}
		}
		this.applySummaries(summaries);
	}

	private markDown(): void {
		if (this.down) {
			return;
		}
		logger.warn('Account engine reported itself unavailable, falling back to cold switches');
		runInAction(() => {
			this.down = true;
			this.summaryMap.clear();
		});
	}
}

function providedAccount(account: Account): AccountEngineProvidedAccount | null {
	const instance = account.instance;
	if (instance === undefined || !account.token) {
		return null;
	}
	return {
		storageKey: account.storageKey,
		record: {
			userId: account.userId,
			token: account.token,
			isValid: account.isValid,
			lastActive: account.lastActive + 0,
			userData:
				account.userData === undefined
					? null
					: {username: account.userData.username, globalName: account.userData.globalName ?? null},
			instance: {
				apiEndpoint: instance.apiEndpoint,
				gatewayEndpoint: instance.gatewayEndpoint,
				apiCodeVersion: instance.apiCodeVersion,
			},
		},
	};
}

async function loadUnreadBadgeCustomization(): Promise<ReadonlyArray<string>> {
	const currentAccountKey = SessionManager.currentAccountKey;
	const accounts = SessionManager.accounts;
	const backend = getPersistentStorageBackend();
	await flushAppStorageWrites();
	const enabled = await Promise.all(
		accounts.map(async (account) => {
			if (account.storageKey === currentAccountKey) {
				return AdvancedSettings.unreadBadgeCustomizationEnabled;
			}
			const stored = await backend.get(account.storageKey, UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY);
			return parseStoredBoolean(stored?.value ?? null);
		}),
	);
	return accounts.filter((_account, index) => enabled[index]).map((account) => account.storageKey);
}

const defaultHost: AccountEngineHost = {
	resolveApi: resolveAccountEngineApi,
	currentAccountKey: () => SessionManager.currentAccountKey,
	expireAccount: async (accountKey, tokenTag) => {
		const token = SessionManager.getAccount(accountKey)?.token;
		if (!token || (await accountEngineTokenTag(token)) !== tokenTag) {
			return;
		}
		SessionManager.markAccountInvalid(accountKey, token);
	},
	reloadAccount: (accountKey) => SessionManager.reloadStoredAccount(accountKey),
	adoptRotatedToken: async (accountKey, previousTokenTag, token) => {
		const previous = SessionManager.getAccount(accountKey)?.token;
		if (!previous || previous === token || (await accountEngineTokenTag(previous)) !== previousTokenTag) {
			return;
		}
		await SessionManager.adoptRotatedAccountToken(accountKey, previous, token);
	},
	loadClientConfig: async () => ({
		properties: {...(await getGatewayClientProperties())},
		apiVersion: DEFAULT_API_VERSION,
		identifyFlags: ACCOUNT_ENGINE_IDENTIFY_FLAGS,
	}),
	providedAccounts: () => {
		if (desktopLegacyImportIsComplete()) {
			return null;
		}
		const accounts: Array<AccountEngineProvidedAccount> = [];
		for (const account of SessionManager.accounts) {
			const provided = providedAccount(account);
			if (provided !== null) {
				accounts.push(provided);
			}
		}
		return accounts;
	},
	unreadBadgeCustomizationInputs: () =>
		JSON.stringify([
			SessionManager.currentAccountKey,
			AdvancedSettings.unreadBadgeCustomizationEnabled,
			SessionManager.accounts.map((account) => account.storageKey),
		]),
	loadUnreadBadgeCustomization,
};

export default new AccountEngineStore(defaultHost);
