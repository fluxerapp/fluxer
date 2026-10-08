// SPDX-License-Identifier: AGPL-3.0-or-later

declare const SNAPSHOT_STORAGE_KEY_BRAND: unique symbol;
declare const SNAPSHOT_ROW_KEY_BRAND: unique symbol;
declare const SNAPSHOT_ROW_KEY_PREFIX_BRAND: unique symbol;
declare const SNAPSHOT_SINGLETON_KEY_BRAND: unique symbol;
declare const SNAPSHOT_GUILD_KEY_BRAND: unique symbol;
declare const SNAPSHOT_GUILD_ROLE_KEY_BRAND: unique symbol;
declare const SNAPSHOT_CHANNEL_KEY_BRAND: unique symbol;
declare const SNAPSHOT_READ_STATE_KEY_BRAND: unique symbol;
declare const SNAPSHOT_USER_GUILD_SETTINGS_KEY_BRAND: unique symbol;
declare const SNAPSHOT_GUILD_EMOJI_KEY_BRAND: unique symbol;
declare const SNAPSHOT_GUILD_STICKER_KEY_BRAND: unique symbol;
declare const SNAPSHOT_GUILD_MEMBER_KEY_BRAND: unique symbol;
declare const SNAPSHOT_RELATIONSHIP_KEY_BRAND: unique symbol;
declare const SNAPSHOT_USER_KEY_BRAND: unique symbol;
declare const SNAPSHOT_PRESENCE_KEY_BRAND: unique symbol;

export type SnapshotStorageKey = string & {readonly [SNAPSHOT_STORAGE_KEY_BRAND]: true};
export type SnapshotRowKey = string & {readonly [SNAPSHOT_ROW_KEY_BRAND]: true};
export type SnapshotRowKeyPrefix = string & {readonly [SNAPSHOT_ROW_KEY_PREFIX_BRAND]: true};
export type SnapshotSingletonKey = SnapshotRowKey & {readonly [SNAPSHOT_SINGLETON_KEY_BRAND]: true};
export type SnapshotGuildKey = SnapshotRowKey & {readonly [SNAPSHOT_GUILD_KEY_BRAND]: true};
export type SnapshotGuildRoleKey = SnapshotRowKey & {readonly [SNAPSHOT_GUILD_ROLE_KEY_BRAND]: true};
export type SnapshotChannelKey = SnapshotRowKey & {readonly [SNAPSHOT_CHANNEL_KEY_BRAND]: true};
export type SnapshotReadStateKey = SnapshotRowKey & {readonly [SNAPSHOT_READ_STATE_KEY_BRAND]: true};
export type SnapshotUserGuildSettingsKey = SnapshotRowKey & {
	readonly [SNAPSHOT_USER_GUILD_SETTINGS_KEY_BRAND]: true;
};
export type SnapshotGuildEmojiKey = SnapshotRowKey & {readonly [SNAPSHOT_GUILD_EMOJI_KEY_BRAND]: true};
export type SnapshotGuildStickerKey = SnapshotRowKey & {readonly [SNAPSHOT_GUILD_STICKER_KEY_BRAND]: true};
export type SnapshotGuildMemberKey = SnapshotRowKey & {readonly [SNAPSHOT_GUILD_MEMBER_KEY_BRAND]: true};
export type SnapshotRelationshipKey = SnapshotRowKey & {readonly [SNAPSHOT_RELATIONSHIP_KEY_BRAND]: true};
export type SnapshotUserKey = SnapshotRowKey & {readonly [SNAPSHOT_USER_KEY_BRAND]: true};
export type SnapshotPresenceKey = SnapshotRowKey & {readonly [SNAPSHOT_PRESENCE_KEY_BRAND]: true};

export const SNAPSHOT_SINGLETON_KEY = snapshotSingletonKey('@me');

export function snapshotRowKey(value: string): SnapshotRowKey {
	requireNonEmpty(value, 'snapshot row key');
	return value as SnapshotRowKey;
}

export function snapshotRowKeyPrefix(value: string): SnapshotRowKeyPrefix {
	requireNonEmpty(value, 'snapshot row key prefix');
	return value as SnapshotRowKeyPrefix;
}

export function snapshotSingletonKey(value: '@me'): SnapshotSingletonKey {
	return snapshotRowKey(value) as SnapshotSingletonKey;
}

export function snapshotGuildKey(guildId: string): SnapshotGuildKey {
	requireNonEmpty(guildId, 'guild snapshot guild id');
	return snapshotRowKey(guildId) as SnapshotGuildKey;
}

export function snapshotGuildRoleKey(guildId: string, roleId: string): SnapshotGuildRoleKey {
	requireCompositePart(guildId, 'guild role snapshot guild id');
	requireCompositePart(roleId, 'guild role snapshot role id');
	return snapshotRowKey(`${guildId}:${roleId}`) as SnapshotGuildRoleKey;
}

export function snapshotChannelKey(channelId: string): SnapshotChannelKey {
	requireNonEmpty(channelId, 'channel snapshot channel id');
	return snapshotRowKey(channelId) as SnapshotChannelKey;
}

export function snapshotReadStateKey(channelId: string): SnapshotReadStateKey {
	requireNonEmpty(channelId, 'read state snapshot channel id');
	return snapshotRowKey(channelId) as SnapshotReadStateKey;
}

export function snapshotUserGuildSettingsKey(guildId: string | null | undefined): SnapshotUserGuildSettingsKey {
	return snapshotRowKey(guildId ?? SNAPSHOT_SINGLETON_KEY) as SnapshotUserGuildSettingsKey;
}

export function snapshotGuildScopedPrefix(guildId: string): SnapshotRowKeyPrefix {
	requireCompositePart(guildId, 'guild-scoped snapshot prefix guild id');
	return snapshotRowKeyPrefix(`${guildId}:`);
}

export function snapshotGuildEmojiKey(guildId: string): SnapshotGuildEmojiKey {
	requireNonEmpty(guildId, 'guild emoji snapshot guild id');
	return snapshotRowKey(guildId) as SnapshotGuildEmojiKey;
}

export function snapshotGuildStickerKey(guildId: string): SnapshotGuildStickerKey {
	requireNonEmpty(guildId, 'guild sticker snapshot guild id');
	return snapshotRowKey(guildId) as SnapshotGuildStickerKey;
}

export function snapshotGuildMemberKey(guildId: string, userId: string): SnapshotGuildMemberKey {
	requireCompositePart(guildId, 'guild member snapshot guild id');
	requireCompositePart(userId, 'guild member snapshot user id');
	return snapshotRowKey(`${guildId}:${userId}`) as SnapshotGuildMemberKey;
}

export function snapshotRelationshipKey(userId: string): SnapshotRelationshipKey {
	requireNonEmpty(userId, 'relationship snapshot user id');
	return snapshotRowKey(userId) as SnapshotRelationshipKey;
}

export function snapshotUserKey(userId: string): SnapshotUserKey {
	requireNonEmpty(userId, 'user snapshot user id');
	return snapshotRowKey(userId) as SnapshotUserKey;
}

export function snapshotPresenceKey(userId: string): SnapshotPresenceKey {
	requireNonEmpty(userId, 'presence snapshot user id');
	return snapshotRowKey(userId) as SnapshotPresenceKey;
}

export function parseSnapshotGuildRoleKey(key: SnapshotGuildRoleKey): {guildId: string; roleId: string} {
	const {left, right} = splitSnapshotPairKey(key, 'guild role');
	return {guildId: left, roleId: right};
}

export function parseSnapshotGuildMemberKey(key: SnapshotGuildMemberKey): {guildId: string; userId: string} {
	const {left, right} = splitSnapshotPairKey(key, 'guild member');
	return {guildId: left, userId: right};
}

function splitSnapshotPairKey(key: SnapshotRowKey, label: string): {left: string; right: string} {
	const separatorIndex = key.indexOf(':');
	if (separatorIndex <= 0) {
		throw new Error(`Malformed ${label} snapshot key: ${key}`);
	}
	const right = key.slice(separatorIndex + 1);
	if (right.length === 0) {
		throw new Error(`Malformed ${label} snapshot key: ${key}`);
	}
	return {left: key.slice(0, separatorIndex), right};
}

function requireNonEmpty(value: string, label: string): void {
	if (value.length === 0) {
		throw new Error(`Snapshot ${label} must be non-empty`);
	}
}

function requireCompositePart(value: string, label: string): void {
	requireNonEmpty(value, label);
	if (value.includes(':')) {
		throw new Error(`Snapshot ${label} must not contain ':'`);
	}
}
