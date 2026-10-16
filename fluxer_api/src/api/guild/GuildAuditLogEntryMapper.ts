// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {AuditLogChange, GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {THREAD_FEATURE_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {THREAD_PERMISSIONS, ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {AuditLogOptions, GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {isValidSnowflake} from '@fluxer/snowflake/src/Snowflake';

export interface StoredGuildAuditLogEntryResponse extends Omit<GuildAuditLogEntryResponse, 'changes'> {
	changes?: GuildAuditLogChange;
}

type NumericAuditLogOptionKey =
	| 'count'
	| 'delete_message_seconds'
	| 'integration_type'
	| 'members_removed'
	| 'type'
	| 'max_age'
	| 'max_uses'
	| 'uses';

export const GUILD_AUDIT_INTERNAL_CHANGE_KEYS: ReadonlySet<string> = new Set([
	'guild_id',
	'member_count',
	'banner_width',
	'banner_height',
	'splash_width',
	'splash_height',
	'embed_splash_width',
	'embed_splash_height',
]);

const NOOP_SKIPPABLE_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.STICKER_UPDATE,
]);

const USER_TARGET_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.MEMBER_KICK,
	AuditLogActionType.MEMBER_PRUNE,
	AuditLogActionType.MEMBER_BAN_ADD,
	AuditLogActionType.MEMBER_BAN_REMOVE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
	AuditLogActionType.BOT_ADD,
]);

const CREATOR_CHANGE_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.WEBHOOK_DELETE,
	AuditLogActionType.EMOJI_DELETE,
	AuditLogActionType.STICKER_DELETE,
]);

const OVERWRITE_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
]);

export const THREAD_SCOPED_AUDIT_OPTION = 'thread_scoped';

export const THREAD_AUDIT_LOG_ACTION_TYPES: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.THREAD_CREATE,
	AuditLogActionType.THREAD_UPDATE,
	AuditLogActionType.THREAD_DELETE,
]);

const CHANNEL_TARGET_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.CHANNEL_CREATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_DELETE,
	...OVERWRITE_ACTIONS,
]);

const THREAD_FEATURE_CHANNEL_TYPE_STRINGS: ReadonlySet<string> = new Set(
	[...THREAD_FEATURE_CHANNEL_TYPES].map((type) => type.toString()),
);

export function collectAuditLogChannelIds(log: GuildAuditLog): Array<string> {
	const ids: Array<string> = [];
	if (CHANNEL_TARGET_ACTIONS.has(log.actionType) && log.targetId) ids.push(log.targetId);
	const channelId = log.options.get('channel_id');
	if (channelId) ids.push(channelId);
	return ids;
}

export function isThreadScopedAuditLog(log: GuildAuditLog, gatedChannelIds: ReadonlySet<string>): boolean {
	if (THREAD_AUDIT_LOG_ACTION_TYPES.has(log.actionType) || log.options.has(THREAD_SCOPED_AUDIT_OPTION)) return true;
	if (
		(log.actionType === AuditLogActionType.CHANNEL_CREATE ||
			log.actionType === AuditLogActionType.CHANNEL_UPDATE ||
			log.actionType === AuditLogActionType.CHANNEL_DELETE) &&
		THREAD_FEATURE_CHANNEL_TYPE_STRINGS.has(log.options.get('type') ?? '')
	) {
		return true;
	}
	return collectAuditLogChannelIds(log).some((id) => gatedChannelIds.has(id));
}

const THREAD_PERMISSION_CHANGE_KEYS: ReadonlyMap<AuditLogActionType, ReadonlySet<string>> = new Map([
	[AuditLogActionType.ROLE_CREATE, new Set(['permissions'])],
	[AuditLogActionType.ROLE_UPDATE, new Set(['permissions', 'permissions_diff'])],
	[AuditLogActionType.ROLE_DELETE, new Set(['permissions'])],
	...[...OVERWRITE_ACTIONS].map((action) => [action, new Set(['allow', 'deny'])] as const),
]);

const THREAD_SURFACE_CHANGE_KEYS: ReadonlySet<string> = new Set([
	'thread_metadata',
	'applied_tags',
	'available_tags',
	'default_auto_archive_duration',
	'default_thread_rate_limit_per_user',
	'default_reaction_emoji',
	'default_sort_order',
	'default_forum_layout',
	'default_tag_setting',
	'message_count',
	'total_message_sent',
	'member_ids_preview',
	'member_count',
	'flags',
]);

const CHANNEL_ACTIONS: ReadonlySet<AuditLogActionType> = new Set([
	AuditLogActionType.CHANNEL_CREATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_DELETE,
]);

const THREAD_PERMISSION_NAMES: ReadonlySet<string> = new Set(Object.keys(ThreadPermissionFlags));

function stripThreadBitsValue(value: unknown): unknown {
	if (typeof value === 'string' && /^\d{1,20}$/.test(value)) return (BigInt(value) & ~THREAD_PERMISSIONS).toString();
	if (!isPermissionsDiff(value)) return value;
	const added = value.added.filter((name) => !THREAD_PERMISSION_NAMES.has(name));
	const removed = value.removed.filter((name) => !THREAD_PERMISSION_NAMES.has(name));
	if (added.length === value.added.length && removed.length === value.removed.length) return value;
	return added.length > 0 || removed.length > 0 ? {added, removed} : undefined;
}

function isPermissionsDiff(value: unknown): value is {added: Array<string>; removed: Array<string>} {
	return (
		typeof value === 'object' &&
		value !== null &&
		Array.isArray((value as {added?: unknown}).added) &&
		Array.isArray((value as {removed?: unknown}).removed)
	);
}

export function maskThreadAuditLog(log: GuildAuditLog): GuildAuditLog {
	if (!log.changes) return log;
	const permissionKeys = THREAD_PERMISSION_CHANGE_KEYS.get(log.actionType);
	const surfaceKeys = CHANNEL_ACTIONS.has(log.actionType) ? THREAD_SURFACE_CHANGE_KEYS : null;
	if (!permissionKeys && !surfaceKeys) return log;
	let changed = false;
	const masked: GuildAuditLogChange = [];
	for (const change of log.changes) {
		if (surfaceKeys?.has(change.key)) {
			changed = true;
			continue;
		}
		if (!permissionKeys?.has(change.key)) {
			masked.push(change);
			continue;
		}
		const next: AuditLogChange = {key: change.key};
		if ('old_value' in change) next.old_value = stripThreadBitsValue(change.old_value);
		if ('new_value' in change) next.new_value = stripThreadBitsValue(change.new_value);
		changed ||= next.old_value !== change.old_value || next.new_value !== change.new_value;
		if (next.old_value === next.new_value) {
			changed = true;
			continue;
		}
		masked.push(next);
	}
	if (!changed) return log;
	return new GuildAuditLog({...log.toRow(), changes: masked.length > 0 ? JSON.stringify(masked) : null});
}

const SNOWFLAKE_PATTERN = /^\d{1,20}$/;

export function isNoopGuildAuditLog(
	actionType: AuditLogActionType,
	changes: GuildAuditLogChange | null | undefined,
): boolean {
	if (!NOOP_SKIPPABLE_ACTIONS.has(actionType)) {
		return false;
	}
	if (!changes) {
		return true;
	}
	return !changes.some(
		(change) =>
			change.key !== 'ip' &&
			!(actionType === AuditLogActionType.GUILD_UPDATE && GUILD_AUDIT_INTERNAL_CHANGE_KEYS.has(change.key)),
	);
}

export function mapGuildAuditLogEntry(log: GuildAuditLog): StoredGuildAuditLogEntryResponse {
	return {
		id: log.logId.toString(),
		action_type: log.actionType as GuildAuditLogEntryResponse['action_type'],
		user_id: log.userId.toString(),
		target_id: log.targetId,
		reason: resolveEntryReason(log),
		options: buildAuditLogOptions(log.options),
		changes: scrubSensitiveChanges(log.changes),
	};
}

export function collectGuildAuditLogUserIds(log: GuildAuditLog): Array<UserID> {
	const userIds = new Set<UserID>([log.userId]);
	const addUserId = (value: unknown) => {
		const userId = parseUserId(value);
		if (userId !== null) {
			userIds.add(userId);
		}
	};
	if (USER_TARGET_ACTIONS.has(log.actionType)) {
		addUserId(log.targetId);
	}
	if (log.actionType === AuditLogActionType.GUILD_UPDATE) {
		addUserId(findChange(log.changes, 'owner_id')?.new_value);
	}
	if (log.actionType === AuditLogActionType.MEMBER_BAN_REMOVE) {
		addUserId(findChange(log.changes, 'moderator_id')?.old_value);
	}
	if (log.actionType === AuditLogActionType.INVITE_DELETE) {
		addUserId(log.options.get('inviter_id'));
	}
	if (CREATOR_CHANGE_ACTIONS.has(log.actionType)) {
		addUserId(findChange(log.changes, 'creator_id')?.old_value);
	}
	if (OVERWRITE_ACTIONS.has(log.actionType) && log.options.get('type') === '1') {
		addUserId(log.targetId);
	}
	return Array.from(userIds);
}

function resolveEntryReason(log: GuildAuditLog): string | undefined {
	const explicitReason = readNonBlankString(log.reason);
	if (explicitReason !== null) {
		return explicitReason;
	}
	if (log.actionType === AuditLogActionType.MEMBER_BAN_ADD) {
		return readNonBlankString(findChange(log.changes, 'reason')?.new_value) ?? undefined;
	}
	if (log.actionType === AuditLogActionType.MEMBER_UPDATE) {
		return readNonBlankString(log.options.get('timeout_reason')) ?? undefined;
	}
	return undefined;
}

function readNonBlankString(value: unknown): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : null;
}

function findChange(changes: GuildAuditLogChange | null, key: string): AuditLogChange | undefined {
	return changes?.find((change) => change.key === key);
}

function scrubSensitiveChanges(changes: GuildAuditLogChange | null | undefined): GuildAuditLogChange | undefined {
	if (!changes) {
		return undefined;
	}
	const scrubbed = changes.filter((change) => change.key !== 'ip');
	return scrubbed.length > 0 ? scrubbed : undefined;
}

function buildAuditLogOptions(options: Map<string, string>): AuditLogOptions | undefined {
	if (!options.size) {
		return undefined;
	}
	const mapped: AuditLogOptions = {};
	for (const [key, value] of options) {
		switch (key) {
			case 'channel_id':
				mapped.channel_id = value;
				break;
			case 'count':
				assignNumericOption(mapped, 'count', value);
				break;
			case 'delete_member_days':
				mapped.delete_member_days = value;
				break;
			case 'delete_message_days':
				if (!mapped.delete_member_days) {
					mapped.delete_member_days = value;
				}
				break;
			case 'delete_message_seconds':
				assignNumericOption(mapped, 'delete_message_seconds', value);
				break;
			case 'id':
				mapped.id = value;
				break;
			case 'integration_type':
				assignNumericOption(mapped, 'integration_type', value);
				break;
			case 'message_id':
				mapped.message_id = value;
				break;
			case 'members_removed':
				assignNumericOption(mapped, 'members_removed', value);
				break;
			case 'role_name':
				mapped.role_name = value;
				break;
			case 'type':
				assignNumericOption(mapped, 'type', value);
				break;
			case 'inviter_id':
				mapped.inviter_id = value;
				break;
			case 'max_age':
				assignNumericOption(mapped, 'max_age', value);
				break;
			case 'max_uses':
				assignNumericOption(mapped, 'max_uses', value);
				break;
			case 'uses':
				assignNumericOption(mapped, 'uses', value);
				break;
			case 'temporary':
				mapped.temporary = parseBooleanOption(value);
				break;
			default:
				break;
		}
	}
	return Object.keys(mapped).length === 0 ? undefined : mapped;
}

function parseBooleanOption(value: string): boolean {
	return value === 'true' || value === '1';
}

function assignNumericOption(target: AuditLogOptions, key: NumericAuditLogOptionKey, value: string): void {
	const parsed = Number(value);
	if (!Number.isNaN(parsed)) {
		target[key] = parsed;
	}
}

function parseUserId(value: unknown): UserID | null {
	if (typeof value !== 'string' || !SNOWFLAKE_PATTERN.test(value)) {
		return null;
	}
	const id = BigInt(value);
	return isValidSnowflake(id) ? createUserID(id) : null;
}
