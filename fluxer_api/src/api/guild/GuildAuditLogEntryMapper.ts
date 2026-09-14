// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {AuditLogChange, GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import type {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
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
		action_type: log.actionType,
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
