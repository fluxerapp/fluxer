// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	AccountEngineAccountActivity,
	AccountEngineChannelActivity,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {compare as compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('OtherAccountsInboxState');

const OTHER_ACCOUNTS_INBOX_CHANNEL_LIMIT = 8;
const OTHER_ACCOUNTS_INBOX_MENTION_LIMIT = 3;

interface OtherAccountsInboxEngine {
	loadActivity(options: {
		channelLimit: number;
		mentionLimit: number;
	}): Promise<ReadonlyArray<AccountEngineAccountActivity>>;
	ackChannel(accountKey: string, channelId: string, messageId: string): Promise<boolean>;
}

interface OtherAccountsInboxStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
}

interface ReadOverride {
	readonly messageId: string;
	readonly settled: boolean;
}

function readOverrideKey(accountKey: string, channelId: string): string {
	return `${accountKey}|${channelId}`;
}

function hasUnreadActivity(channel: AccountEngineChannelActivity): boolean {
	return channel.mentionCount > 0 || channel.unread;
}

export class OtherAccountsInboxState {
	collapsed: boolean;
	activity: ReadonlyArray<AccountEngineAccountActivity> = [];
	loading = false;
	private readOverrides = new Map<string, ReadOverride>();
	private generation = 0;

	constructor(
		private readonly engine: OtherAccountsInboxEngine,
		private readonly storage: OtherAccountsInboxStorage,
		private readonly storageKey: string,
	) {
		this.collapsed = storage.getItem(storageKey) === 'true';
		makeAutoObservable<this, 'engine' | 'storage' | 'storageKey' | 'generation'>(
			this,
			{engine: false, storage: false, storageKey: false, generation: false},
			{autoBind: true},
		);
	}

	setCollapsed(collapsed: boolean): void {
		this.collapsed = collapsed;
		this.storage.setItem(this.storageKey, collapsed ? 'true' : 'false');
	}

	toggleCollapsed(): void {
		this.setCollapsed(!this.collapsed);
	}

	async load(): Promise<void> {
		const generation = ++this.generation;
		this.loading = true;
		const activity = await this.engine.loadActivity({
			channelLimit: OTHER_ACCOUNTS_INBOX_CHANNEL_LIMIT,
			mentionLimit: OTHER_ACCOUNTS_INBOX_MENTION_LIMIT,
		});
		if (generation !== this.generation) return;
		runInAction(() => {
			this.activity = activity;
			this.loading = false;
			this.pruneSettledOverrides(activity);
		});
	}

	reset(): void {
		this.generation += 1;
		this.activity = [];
		this.loading = false;
		this.readOverrides.clear();
	}

	private pruneSettledOverrides(activity: ReadonlyArray<AccountEngineAccountActivity>): void {
		const latest = new Map<string, string | null>();
		for (const account of activity) {
			for (const channel of account.channels) {
				latest.set(readOverrideKey(account.accountKey, channel.channelId), channel.lastMessageId);
			}
		}
		for (const [key, override] of this.readOverrides) {
			if (!override.settled) continue;
			const lastMessageId = latest.get(key);
			if (lastMessageId == null || compareSnowflakes(lastMessageId, override.messageId) > 0) {
				this.readOverrides.delete(key);
			}
		}
	}

	isMarkedRead(accountKey: string, channel: AccountEngineChannelActivity): boolean {
		const override = this.readOverrides.get(readOverrideKey(accountKey, channel.channelId));
		if (override == null) return false;
		return channel.lastMessageId == null || compareSnowflakes(channel.lastMessageId, override.messageId) <= 0;
	}

	visibleChannels(account: AccountEngineAccountActivity): Array<AccountEngineChannelActivity> {
		const channels = account.channels.filter(
			(channel) => hasUnreadActivity(channel) && !this.isMarkedRead(account.accountKey, channel),
		);
		const mentioned = channels.filter((channel) => channel.mentionCount > 0);
		const unread = channels.filter((channel) => channel.mentionCount === 0);
		return [...mentioned, ...unread];
	}

	get visibleAccounts(): Array<AccountEngineAccountActivity> {
		return this.activity.filter((account) => this.visibleChannels(account).length > 0 || account.mentions.length > 0);
	}

	async markAsRead(accountKey: string, channel: AccountEngineChannelActivity): Promise<boolean> {
		const messageId = channel.lastMessageId;
		if (messageId == null) return false;
		const key = readOverrideKey(accountKey, channel.channelId);
		this.readOverrides.set(key, {messageId, settled: false});
		let acked = false;
		try {
			acked = await this.engine.ackChannel(accountKey, channel.channelId, messageId);
		} catch (error) {
			logger.warn(
				'Failed to acknowledge a channel on another account',
				{accountKey, channelId: channel.channelId},
				error,
			);
			acked = false;
		}
		runInAction(() => {
			const current = this.readOverrides.get(key);
			if (current == null || current.messageId !== messageId) return;
			if (acked) {
				this.readOverrides.set(key, {messageId, settled: true});
			} else {
				this.readOverrides.delete(key);
			}
		});
		return acked;
	}
}
