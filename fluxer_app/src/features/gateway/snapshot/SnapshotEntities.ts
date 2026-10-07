// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import type {GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import type {WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import type {RtcRegionResponse, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ConnectionListResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {GuildEmoji, GuildSticker} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole as WireGuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {
	UserPartial,
	UserPrivate,
	UserSettingsResponse,
	User as WireUser,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

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

export const SNAPSHOT_SCHEMA_EPOCH = 4;

export type SnapshotUserRow = WireUser | UserPrivate | UserPartial;

export interface SnapshotGuildRow {
	id: string;
	properties: Omit<WireGuild, 'roles'>;
	joined_at: string | null;
	member_count: number;
	online_count?: number;
	threads_active?: boolean;
}

export interface SnapshotUnavailableGuildRow {
	id: string;
	unavailable: boolean;
}

export interface SnapshotReadStateRow {
	ackMessageId: string | null;
	ackPinTimestamp: number;
	mentionCount: number;
	serverVersion: string | null;
	readStateKnown: boolean;
	lastMessageId: string | null;
	guildId: string | null;
}

export interface SnapshotPresenceRow {
	status: string | null;
	customStatus: GatewayCustomStatusPayload | null;
	afk: boolean;
	mobile: boolean;
	guildIds: Array<string>;
}

export interface SnapshotAccountMetadataRow {
	authSessionIdHash: string | null;
	staticClientSessionId: string | null;
	countryCode: string | null;
	pinnedDmIds: Array<string>;
	favoriteMemes: Array<FavoriteMemeWire>;
	rtcRegions: Array<RtcRegionResponse>;
	webAuthnCredentials: Array<WebAuthnCredential>;
	notes: Record<string, string>;
}

export interface SnapshotEntityRowMap {
	guild: SnapshotGuildRow;
	unavailable_guild: SnapshotUnavailableGuildRow;
	guild_role: WireGuildRole;
	channel: WireChannel;
	read_state: SnapshotReadStateRow;
	user_settings: UserSettingsResponse;
	account_metadata: SnapshotAccountMetadataRow;
	user_guild_settings: GatewayGuildSettings;
	guild_emoji: ReadonlyArray<GuildEmoji>;
	guild_sticker: ReadonlyArray<GuildSticker>;
	guild_member: GuildMemberData;
	relationship: RelationshipWire;
	user: SnapshotUserRow;
	user_connections: ConnectionListResponse;
	presence: SnapshotPresenceRow;
	voice_state: ReadonlyArray<VoiceState>;
}

export type SnapshotEntity = keyof SnapshotEntityRowMap;

export interface SnapshotEntityKeyMap {
	guild: SnapshotGuildKey;
	unavailable_guild: SnapshotGuildKey;
	guild_role: SnapshotGuildRoleKey;
	channel: SnapshotChannelKey;
	read_state: SnapshotReadStateKey;
	user_settings: SnapshotSingletonKey;
	account_metadata: SnapshotSingletonKey;
	user_guild_settings: SnapshotUserGuildSettingsKey;
	guild_emoji: SnapshotGuildEmojiKey;
	guild_sticker: SnapshotGuildStickerKey;
	guild_member: SnapshotGuildMemberKey;
	relationship: SnapshotRelationshipKey;
	user: SnapshotUserKey;
	user_connections: SnapshotSingletonKey;
	presence: SnapshotPresenceKey;
	voice_state: SnapshotGuildKey;
}

export type SnapshotSingletonEntity = 'user_settings' | 'account_metadata' | 'user_connections';

export interface SnapshotRowUpsertOpFor<E extends SnapshotEntity> {
	readonly kind: 'upsert';
	readonly entity: E;
	readonly key: SnapshotEntityKeyMap[E];
	readonly value: SnapshotEntityRowMap[E];
}

export type SnapshotRowUpsertOp<E extends SnapshotEntity = SnapshotEntity> = E extends SnapshotEntity
	? SnapshotRowUpsertOpFor<E>
	: never;

export interface SnapshotRowDeleteOpFor<E extends SnapshotEntity> {
	readonly kind: 'delete';
	readonly entity: E;
	readonly key: SnapshotEntityKeyMap[E];
}

export type SnapshotRowDeleteOp<E extends SnapshotEntity = SnapshotEntity> = E extends SnapshotEntity
	? SnapshotRowDeleteOpFor<E>
	: never;

export interface SnapshotRowDeleteByPrefixOpFor<E extends SnapshotEntity> {
	readonly kind: 'deleteByPrefix';
	readonly entity: E;
	readonly keyPrefix: SnapshotRowKeyPrefix;
}

export type SnapshotRowDeleteByPrefixOp<E extends SnapshotEntity = SnapshotEntity> = E extends SnapshotEntity
	? SnapshotRowDeleteByPrefixOpFor<E>
	: never;

export interface SnapshotRowReplaceEntityEntry<E extends SnapshotEntity = SnapshotEntity> {
	readonly key: SnapshotEntityKeyMap[E];
	readonly value: SnapshotEntityRowMap[E];
}

export interface SnapshotRowReplaceEntityOpFor<E extends SnapshotEntity> {
	readonly kind: 'replaceEntity';
	readonly entity: E;
	readonly entries: ReadonlyArray<SnapshotRowReplaceEntityEntry<E>>;
}

export type SnapshotRowReplaceEntityOp<E extends SnapshotEntity = SnapshotEntity> = E extends SnapshotEntity
	? SnapshotRowReplaceEntityOpFor<E>
	: never;

export type SnapshotRowOp =
	| SnapshotRowUpsertOp
	| SnapshotRowDeleteOp
	| SnapshotRowDeleteByPrefixOp
	| SnapshotRowReplaceEntityOp;

export type SnapshotEmit = (op: SnapshotRowOp) => void;

export function parseSnapshotEntity<E extends SnapshotEntity>(
	entries: StateSnapshotEntries,
	entity: E,
): Map<SnapshotEntityKeyMap[E], SnapshotEntityRowMap[E]> {
	const result = new Map<SnapshotEntityKeyMap[E], SnapshotEntityRowMap[E]>();
	const rows = entries[entity];
	if (!rows) {
		return result;
	}
	for (const [key, value] of Object.entries(rows)) {
		result.set(snapshotEntityKey(entity, key), JSON.parse(value) as SnapshotEntityRowMap[E]);
	}
	return result;
}

export function parseSnapshotSingleton<E extends SnapshotSingletonEntity>(
	entries: StateSnapshotEntries,
	entity: E,
): SnapshotEntityRowMap[E] | null {
	return parseSnapshotRow(entries, entity, SNAPSHOT_SINGLETON_KEY);
}

export function parseSnapshotRow<E extends SnapshotEntity>(
	entries: StateSnapshotEntries,
	entity: E,
	key: SnapshotEntityKeyMap[E],
): SnapshotEntityRowMap[E] | null {
	const serialized = entries[entity]?.[key];
	return serialized == null ? null : (JSON.parse(serialized) as SnapshotEntityRowMap[E]);
}

export function snapshotEntityKey<E extends SnapshotEntity>(entity: E, key: string): SnapshotEntityKeyMap[E] {
	switch (entity) {
		case 'guild':
		case 'unavailable_guild':
		case 'voice_state':
			return snapshotGuildKey(key) as SnapshotEntityKeyMap[E];
		case 'guild_role': {
			const separatorIndex = key.indexOf(':');
			if (separatorIndex <= 0) {
				throw new Error(`Malformed guild role snapshot key: ${key}`);
			}
			return snapshotGuildRoleKey(
				key.slice(0, separatorIndex),
				key.slice(separatorIndex + 1),
			) as SnapshotEntityKeyMap[E];
		}
		case 'channel':
			return snapshotChannelKey(key) as SnapshotEntityKeyMap[E];
		case 'read_state':
			return snapshotReadStateKey(key) as SnapshotEntityKeyMap[E];
		case 'user_settings':
		case 'account_metadata':
		case 'user_connections':
			if (key !== SNAPSHOT_SINGLETON_KEY) {
				throw new Error(`Snapshot entity ${entity} must be keyed by ${SNAPSHOT_SINGLETON_KEY}`);
			}
			return SNAPSHOT_SINGLETON_KEY as SnapshotEntityKeyMap[E];
		case 'user_guild_settings':
			return snapshotUserGuildSettingsKey(key === SNAPSHOT_SINGLETON_KEY ? null : key) as SnapshotEntityKeyMap[E];
		case 'guild_emoji':
			return snapshotGuildEmojiKey(key) as SnapshotEntityKeyMap[E];
		case 'guild_sticker':
			return snapshotGuildStickerKey(key) as SnapshotEntityKeyMap[E];
		case 'guild_member': {
			const separatorIndex = key.indexOf(':');
			if (separatorIndex <= 0) {
				throw new Error(`Malformed guild member snapshot key: ${key}`);
			}
			return snapshotGuildMemberKey(
				key.slice(0, separatorIndex),
				key.slice(separatorIndex + 1),
			) as SnapshotEntityKeyMap[E];
		}
		case 'relationship':
			return snapshotRelationshipKey(key) as SnapshotEntityKeyMap[E];
		case 'user':
			return snapshotUserKey(key) as SnapshotEntityKeyMap[E];
		case 'presence':
			return snapshotPresenceKey(key) as SnapshotEntityKeyMap[E];
	}
}

export function accountMetadataFromReady(data: {
	auth_session_id_hash?: string;
	static_client_session_id?: string;
	country_code?: string;
	pinned_dms?: Array<string>;
	favorite_memes?: Array<FavoriteMemeWire>;
	rtc_regions?: Array<RtcRegionResponse>;
	webauthn_credentials?: Array<WebAuthnCredential>;
	notes?: Record<string, string>;
}): SnapshotAccountMetadataRow {
	return {
		authSessionIdHash: data.auth_session_id_hash ?? null,
		staticClientSessionId: data.static_client_session_id ?? null,
		countryCode: data.country_code ?? null,
		pinnedDmIds: [...(data.pinned_dms ?? [])],
		favoriteMemes: [...(data.favorite_memes ?? [])],
		rtcRegions: [...(data.rtc_regions ?? [])],
		webAuthnCredentials: [...(data.webauthn_credentials ?? [])],
		notes: {...(data.notes ?? {})},
	};
}
