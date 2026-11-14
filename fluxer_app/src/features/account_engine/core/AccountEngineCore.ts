// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {DiskBackedSnapshotReducer} from '@app/features/gateway/snapshot/DiskBackedSnapshotReducer';
import {parseSnapshotGatewayEvent} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {snapshotGuildMemberKey, snapshotUserKey} from '@app/features/gateway/snapshot/SnapshotEntities';
import {SnapshotReducer} from '@app/features/gateway/snapshot/SnapshotReducer';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import {SnapshotWriter} from '@app/features/gateway/snapshot/SnapshotWriter';
import {buildBackgroundSnapshotReadySeed} from '@app/features/gateway/transport/BackgroundGatewaySnapshotHydration';
import {
	type BackgroundChannelActivity,
	BackgroundReadStateReducer,
} from '@app/features/gateway/transport/BackgroundReadStateReducer';
import type {BackgroundMessageNotification} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {snowflakeTimestamp} from '@app/features/read_state/state/read_states/shared';
import {
	ACCOUNT_ENGINE_LIMITS,
	type AccountEngineAccountActivity,
	type AccountEngineChannelActivity,
	type AccountEngineMention,
	type AccountEngineSnapshotCapture,
	type AccountEngineUser,
	inlineMentionedUserNames,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {
	AccountEngineCore,
	AccountEngineCoreCounts,
	AccountEngineCoreOptions,
	AccountEngineRowStore,
} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';

const MAX_AUTH_TOKEN_LENGTH = 4096;

function asRecord(value: unknown): Record<string, unknown> | null {
	return isRecord(value) ? value : null;
}

function asString(value: unknown): string | null {
	return typeof value === 'string' && value.length > 0 ? value : null;
}

function nullableString(value: unknown): string | null {
	return typeof value === 'string' ? value : null;
}

function accountEngineUserFromWire(value: unknown): AccountEngineUser | null {
	const user = asRecord(value);
	const id = asString(user?.id);
	if (user === null || id === null) {
		return null;
	}
	return {
		id,
		username: nullableString(user.username) ?? '',
		globalName: asString(user.global_name),
		avatar: asString(user.avatar),
		discriminator: asString(user.discriminator),
	};
}

function parseRow(serialized: string | null): Record<string, unknown> | null {
	if (serialized === null) {
		return null;
	}
	try {
		return asRecord(JSON.parse(serialized));
	} catch {
		return null;
	}
}

const SEED_ENTITIES = [
	'channel',
	'guild',
	'unavailable_guild',
	'user_guild_settings',
	'read_state',
	'relationship',
	'user_settings',
	'account_metadata',
] as const;

interface SeedRowSource {
	row(entity: string, key: string): string | null;
	entity(entity: string): Record<string, string>;
}

function collectSeedEntries(source: SeedRowSource, userId: string): StateSnapshotEntries | null {
	const selfKey = snapshotUserKey(userId);
	const self = source.row('user', selfKey);
	if (self === null) {
		return null;
	}
	const entries: Record<string, Record<string, string>> = {user: {[selfKey]: self}};
	for (const entity of SEED_ENTITIES) {
		entries[entity] = source.entity(entity);
	}
	const members: Record<string, string> = {};
	for (const guildId of Object.keys(entries.guild)) {
		const memberKey = snapshotGuildMemberKey(guildId, userId);
		const member = source.row('guild_member', memberKey);
		if (member !== null) {
			members[memberKey] = member;
		}
	}
	entries.guild_member = members;
	return entries as StateSnapshotEntries;
}

function seedEntriesFromStore(
	store: AccountEngineRowStore,
	accountKey: string,
	userId: string,
): StateSnapshotEntries | null {
	return collectSeedEntries(
		{
			row: (entity, key) => store.get(accountKey, entity, key),
			entity: (entity) => {
				const rows: Record<string, string> = {};
				for (const row of store.entriesByPrefix(accountKey, entity, '')) {
					rows[row.key] = row.value;
				}
				return rows;
			},
		},
		userId,
	);
}

function seedEntriesFromCapture(capture: AccountEngineSnapshotCapture, userId: string): StateSnapshotEntries | null {
	return collectSeedEntries(
		{
			row: (entity, key) => capture.entries[entity]?.[key] ?? null,
			entity: (entity) => capture.entries[entity] ?? {},
		},
		userId,
	);
}

function sumReadStateMentions(store: AccountEngineRowStore, accountKey: string): number {
	let total = 0;
	for (const row of store.entriesByPrefix(accountKey, 'read_state', '')) {
		const count = parseRow(row.value)?.mentionCount;
		if (typeof count === 'number' && Number.isFinite(count) && count > 0) {
			total += Math.floor(count);
		}
	}
	return total;
}

function compareActivity(left: BackgroundChannelActivity, right: BackgroundChannelActivity): number {
	const leftMentioned = left.mentionCount > 0 ? 1 : 0;
	const rightMentioned = right.mentionCount > 0 ? 1 : 0;
	if (leftMentioned !== rightMentioned) {
		return rightMentioned - leftMentioned;
	}
	const leftId = left.lastMessageId;
	const rightId = right.lastMessageId;
	if (leftId === rightId) {
		return 0;
	}
	if (leftId === null) {
		return 1;
	}
	if (rightId === null) {
		return -1;
	}
	if (leftId.length !== rightId.length) {
		return rightId.length - leftId.length;
	}
	return leftId < rightId ? 1 : -1;
}

class EngineCore implements AccountEngineCore {
	private readonly options: AccountEngineCoreOptions;
	private readonly accountKey: string;
	private readonly readState: BackgroundReadStateReducer;
	private writer: SnapshotWriter | null = null;
	private snapshot: DiskBackedSnapshotReducer | null = null;
	private lastCounts: AccountEngineCoreCounts | null = null;
	private degradedMentionTotal: number | null = null;
	private disposed = false;

	constructor(options: AccountEngineCoreOptions) {
		this.options = options;
		this.accountKey = options.accountKey;
		this.readState = new BackgroundReadStateReducer({
			userId: options.userId,
			observeMentionCounts: () => {
				this.degradedMentionTotal = null;
			},
			observeMessageNotification: (notification) => this.emitNotification(notification),
			observeMention: (notification) => this.emitMention(notification),
		});
		this.openSnapshot();
	}

	seedFromStore(mentionCounts: Readonly<Record<string, number>> | null): boolean {
		if (!this.options.store.has(this.accountKey)) {
			return false;
		}
		const entries = seedEntriesFromStore(this.options.store, this.accountKey, this.options.userId);
		if (entries === null) {
			return false;
		}
		this.seedFromEntries(entries, mentionCounts);
		this.emitCountsIfChanged();
		return true;
	}

	applyReady(data: unknown): {readonly sessionId: string | null} {
		const event = parseSnapshotGatewayEvent('READY', data);
		if (event?.type !== 'READY') {
			throw new Error('READY payload did not parse as a READY event');
		}
		const ready = event.data as ReadyPayload;
		const readyUserId = asString(asRecord(ready.user)?.id);
		if (readyUserId !== this.options.userId) {
			throw new Error(`READY identified as ${String(readyUserId)}, expected ${this.options.userId}`);
		}
		if (this.snapshot === null) {
			this.openSnapshot();
		}
		this.snapshot?.applyReady(ready);
		this.readState.applyReady(ready);
		this.degradedMentionTotal = null;
		const self = accountEngineUserFromWire(ready.user);
		if (self !== null) {
			this.options.onSelfUser(self);
		}
		this.emitCountsIfChanged();
		return {sessionId: asString(ready.session_id)};
	}

	applyDispatch(type: string, data: unknown): void {
		const payload = asRecord(data);
		if (type === 'AUTH_SESSION_CHANGE') {
			this.handleAuthSessionChange(payload);
		}
		const event = parseSnapshotGatewayEvent(type, data);
		if (event !== null && event.type !== 'READY') {
			this.snapshot?.applyDispatch(event);
		}
		this.readState.applyDispatch(type, data);
		if (type === 'USER_UPDATE' && asString(payload?.id) === this.options.userId) {
			const self = accountEngineUserFromWire(payload);
			if (self !== null) {
				this.options.onSelfUser(self);
			}
		}
		if (type === 'MESSAGE_ACK') {
			const channelId = asString(payload?.channel_id);
			if (channelId !== null) {
				this.options.onChannelRead(channelId);
			}
		}
		if (this.readState.isDegraded && (type === 'MESSAGE_CREATE' || type === 'MESSAGE_ACK')) {
			this.degradedMentionTotal = null;
		}
		this.emitCountsIfChanged();
	}

	replaceFromCapture(
		capture: AccountEngineSnapshotCapture,
		mentionCounts: Readonly<Record<string, number>> | null,
	): void {
		this.closeSnapshot();
		this.options.store.replaceAccount(this.accountKey, capture);
		this.openSnapshot();
		const entries = seedEntriesFromCapture(capture, this.options.userId);
		if (entries === null) {
			throw new Error('The demotion capture has no row for its own user');
		}
		this.seedFromEntries(entries, mentionCounts);
		this.emitCountsIfChanged();
	}

	mentionCounts(): Record<string, number> {
		return Object.fromEntries(this.readState.mentionCountsSnapshot());
	}

	refreshUnread(): void {
		this.readState.refreshUnreadState();
		this.emitCountsIfChanged();
	}

	setUnreadBadgeCustomization(enabled: boolean): void {
		this.readState.setUnreadBadgeCustomizationEnabled(enabled);
		this.emitCountsIfChanged();
	}

	activity(channelLimit: number): Omit<AccountEngineAccountActivity, 'mentions' | 'accountKey'> {
		const all = Array.from(this.readState.channelActivity()).sort(compareActivity);
		const limit = Math.max(0, Math.floor(channelLimit));
		return {
			channels: all.slice(0, limit).map((entry) => this.describeChannel(entry)),
			totalChannelCount: all.length,
		};
	}

	dispose(): void {
		this.disposed = true;
		this.closeSnapshot();
	}

	private openSnapshot(): void {
		this.closeSnapshot();
		const onInvalid = (error: Error): void => this.invalidateSnapshot(error);
		const store = this.options.store;
		const writer = new SnapshotWriter({storageKey: this.accountKey, store, onInvalid});
		this.writer = writer;
		this.snapshot = new DiskBackedSnapshotReducer({storageKey: this.accountKey, store, writer, onInvalid});
	}

	private closeSnapshot(): void {
		this.snapshot?.dispose();
		this.writer?.dispose();
		this.snapshot = null;
		this.writer = null;
	}

	private invalidateSnapshot(error: Error): void {
		if (this.disposed) {
			return;
		}
		this.closeSnapshot();
		this.options.onInvalid(error);
	}

	private seedFromEntries(entries: StateSnapshotEntries, mentionCounts: Readonly<Record<string, number>> | null): void {
		const shell = new SnapshotReducer();
		shell.loadHeavyMapsFromEntries(entries);
		this.snapshot?.setShellState(shell.exportShellState());
		const overlay = new Map<string, number>();
		if (mentionCounts !== null) {
			for (const [channelId, count] of Object.entries(mentionCounts)) {
				if (Number.isSafeInteger(count) && count > 0) {
					overlay.set(channelId, count);
				}
			}
		} else {
			for (const [channelId, serialized] of Object.entries(entries.read_state ?? {})) {
				const count = parseRow(serialized)?.mentionCount;
				if (typeof count === 'number' && Number.isSafeInteger(count) && count > 0) {
					overlay.set(channelId, count);
				}
			}
		}
		this.readState.applyReady(buildBackgroundSnapshotReadySeed(entries, this.options.userId, overlay));
		this.degradedMentionTotal = null;
		const self = accountEngineUserFromWire(parseRow(entries.user?.[this.options.userId] ?? null));
		if (self !== null) {
			this.options.onSelfUser(self);
		}
	}

	private emitCountsIfChanged(): void {
		const totals = this.readState.totals();
		let mentionCount = totals.mentionCount;
		if (totals.degraded) {
			this.degradedMentionTotal ??= sumReadStateMentions(this.options.store, this.accountKey);
			mentionCount = this.degradedMentionTotal;
		}
		const next: AccountEngineCoreCounts = {
			mentionCount,
			unreadChannelCount: totals.degraded ? 0 : totals.unreadChannelCount,
			lastMessageAt: totals.lastMessageId === null ? null : snowflakeTimestamp(totals.lastMessageId),
			degraded: totals.degraded,
		};
		const previous = this.lastCounts;
		if (
			previous !== null &&
			previous.mentionCount === next.mentionCount &&
			previous.unreadChannelCount === next.unreadChannelCount &&
			previous.lastMessageAt === next.lastMessageAt &&
			previous.degraded === next.degraded
		) {
			return;
		}
		this.lastCounts = next;
		this.options.onCountsChanged(next);
	}

	private handleAuthSessionChange(payload: Record<string, unknown> | null): void {
		const token = payload?.new_token;
		if (token === undefined) {
			return;
		}
		if (typeof token !== 'string' || token.length === 0 || token.length > MAX_AUTH_TOKEN_LENGTH) {
			throw new Error('AUTH_SESSION_CHANGE carried an invalid token');
		}
		this.options.onTokenRotated(token);
	}

	private emitNotification(notification: BackgroundMessageNotification): void {
		this.options.onNotification({
			message: notification.message,
			guildName: notification.guildName,
			channelName: notification.channelName,
			channelType: notification.channelType,
			mentioned: notification.mentioned,
			gated: notification.gated,
		});
	}

	private emitMention(notification: BackgroundMessageNotification): void {
		const message = asRecord(notification.message);
		const messageId = asString(message?.id);
		const channelId = asString(message?.channel_id);
		const author = accountEngineUserFromWire(message?.author);
		if (message === null || messageId === null || channelId === null || author === null) {
			return;
		}
		const content = inlineMentionedUserNames(nullableString(message.content) ?? '', message.mentions);
		const mention: AccountEngineMention = {
			messageId,
			channelId,
			guildId: asString(message.guild_id),
			channelType: notification.channelType,
			channelName: notification.channelName,
			guildName: notification.guildName,
			author,
			content: notification.gated ? '' : content.slice(0, ACCOUNT_ENGINE_LIMITS.mentionContentMaxChars),
			gated: notification.gated,
			timestamp: nullableString(message.timestamp) ?? new Date(snowflakeTimestamp(messageId)).toISOString(),
		};
		this.options.onMention(mention);
	}

	private describeChannel(entry: BackgroundChannelActivity): AccountEngineChannelActivity {
		const store = this.options.store;
		const channel = parseRow(store.get(this.accountKey, 'channel', entry.channelId));
		const guildId = entry.guildId ?? asString(channel?.guild_id);
		const guild = guildId === null ? null : parseRow(store.get(this.accountKey, 'guild', guildId));
		const guildProperties = asRecord(guild?.properties) ?? guild;
		const recipients: Array<AccountEngineUser> = [];
		const recipientList = Array.isArray(channel?.recipients) ? channel.recipients : [];
		for (const recipient of recipientList) {
			if (recipients.length >= ACCOUNT_ENGINE_LIMITS.recipientsMax) {
				break;
			}
			const recipientId = typeof recipient === 'string' ? recipient : asString(asRecord(recipient)?.id);
			const user =
				recipientId === null
					? null
					: (accountEngineUserFromWire(parseRow(store.get(this.accountKey, 'user', recipientId))) ??
						accountEngineUserFromWire(recipient));
			if (user !== null && user.id !== this.options.userId) {
				recipients.push(user);
			}
		}
		const channelType = entry.type ?? (typeof channel?.type === 'number' ? channel.type : 0);
		return {
			channelId: entry.channelId,
			guildId,
			parentId: entry.parentId ?? asString(channel?.parent_id),
			type: channelType,
			name: entry.name ?? asString(channel?.name),
			guildName:
				(guildId === null ? null : this.readState.guildName(guildId)) ?? asString(guildProperties?.name) ?? null,
			guildIcon: asString(guildProperties?.icon),
			recipients,
			mentionCount: entry.mentionCount,
			unread: entry.unread,
			lastMessageId: entry.lastMessageId,
			ackMessageId: entry.ackMessageId,
		};
	}
}

export function createEngineCore(options: AccountEngineCoreOptions): AccountEngineCore {
	return new EngineCore(options);
}
