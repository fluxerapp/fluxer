// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogSentence,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	ROLE_COLOR_CHANGE_ROW,
	ROLE_COLOR_REMOVE_ROW,
	ROLE_COLOR_SET_ROW,
	ROLE_CREATE_SUMMARY,
	ROLE_DELETE_SUMMARY,
	ROLE_DELETED_PERMISSIONS_ROW,
	ROLE_EMOJI_CHANGE_ROW,
	ROLE_EMOJI_REMOVE_ROW,
	ROLE_EMOJI_SET_ROW,
	ROLE_HOIST_CREATE_ROW,
	ROLE_HOIST_POSITION_DOWN_ROW,
	ROLE_HOIST_POSITION_RESET_ROW,
	ROLE_HOIST_POSITION_SET_ROW,
	ROLE_HOIST_POSITION_UP_ROW,
	ROLE_HOIST_START_ROW,
	ROLE_HOIST_STOP_ROW,
	ROLE_ICON_CHANGE_ROW,
	ROLE_ICON_REMOVE_ROW,
	ROLE_ICON_SET_ROW,
	ROLE_MENTIONABLE_ALLOW_ROW,
	ROLE_MENTIONABLE_DISALLOW_ROW,
	ROLE_MOVE_DOWN_IN_MEMBER_LIST_SUMMARY,
	ROLE_MOVE_DOWN_IN_ROLE_LIST_SUMMARY,
	ROLE_MOVE_UP_IN_MEMBER_LIST_SUMMARY,
	ROLE_MOVE_UP_IN_ROLE_LIST_SUMMARY,
	ROLE_NAME_CHANGE_ROW,
	ROLE_PERMISSIONS_GRANT_ROW,
	ROLE_PERMISSIONS_REVOKE_ROW,
	ROLE_POSITION_DOWN_ROW,
	ROLE_POSITION_UP_ROW,
	ROLE_RENAME_SUMMARY,
	ROLE_RESET_MEMBER_LIST_POSITION_SUMMARY,
	ROLE_SET_MEMBER_LIST_POSITION_SUMMARY,
	ROLE_UPDATE_SUMMARY,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogRoleMessages';
import {
	actorPlaceholder,
	diffBitfields,
	permissionFlagsIn,
	readBitfield,
	readBoolean,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
	readStringArray,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

type ListDirection = 'up' | 'down';
type MemberListPositionChange = ListDirection | 'set' | 'reset';

interface ValueChange<T> {
	previous: T;
	next: T;
}

interface PermissionChanges {
	added: Array<bigint>;
	removed: Array<bigint>;
}

const MAX_ROLE_COLOR = 0xffffff;
const PERMISSION_FLAGS_BY_NAME: ReadonlyMap<string, bigint> = new Map(Object.entries(Permissions));

const ROLE_LIST_MOVE_SUMMARIES: Record<ListDirection, MessageDescriptor> = {
	up: ROLE_MOVE_UP_IN_ROLE_LIST_SUMMARY,
	down: ROLE_MOVE_DOWN_IN_ROLE_LIST_SUMMARY,
};
const ROLE_LIST_MOVE_ROWS: Record<ListDirection, MessageDescriptor> = {
	up: ROLE_POSITION_UP_ROW,
	down: ROLE_POSITION_DOWN_ROW,
};
const MEMBER_LIST_POSITION_SUMMARIES: Record<MemberListPositionChange, MessageDescriptor> = {
	set: ROLE_SET_MEMBER_LIST_POSITION_SUMMARY,
	reset: ROLE_RESET_MEMBER_LIST_POSITION_SUMMARY,
	up: ROLE_MOVE_UP_IN_MEMBER_LIST_SUMMARY,
	down: ROLE_MOVE_DOWN_IN_MEMBER_LIST_SUMMARY,
};
const MEMBER_LIST_POSITION_ROWS: Record<MemberListPositionChange, MessageDescriptor> = {
	set: ROLE_HOIST_POSITION_SET_ROW,
	reset: ROLE_HOIST_POSITION_RESET_ROW,
	up: ROLE_HOIST_POSITION_UP_ROW,
	down: ROLE_HOIST_POSITION_DOWN_ROW,
};

function row(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: AuditLogSentence['values'] = {},
): AuditLogDetailRow {
	return {id, tone, sentence: {descriptor, values}};
}

function summaryOnly(descriptor: MessageDescriptor, values: AuditLogSentence['values']): AuditLogDomainResult {
	return {summary: {descriptor, values}, rows: [], blocks: []};
}

function rolePlaceholder(entry: GuildAuditLogEntryResponse, recordedName: string | null): AuditLogPlaceholder {
	return {kind: 'role', id: readSnowflake(entry.target_id) ?? '', recordedName};
}

function nameValue(value: string): AuditLogPlaceholder {
	return {kind: 'name', value};
}

function colorValue(value: number): AuditLogPlaceholder {
	return {kind: 'color', value};
}

function permissionsValue(flags: Array<bigint>): AuditLogPlaceholder {
	return {kind: 'permissions', flags};
}

function readNewValue(entry: GuildAuditLogEntryResponse, key: string): unknown {
	return readChange(entry, key)?.newValue ?? null;
}

function readOldValue(entry: GuildAuditLogEntryResponse, key: string): unknown {
	return readChange(entry, key)?.oldValue ?? null;
}

function readColor(value: unknown): number | null {
	const color = readNumber(value);
	return color !== null && Number.isInteger(color) && color >= 0 && color <= MAX_ROLE_COLOR ? color : null;
}

function isNullableString(value: unknown): value is string | null {
	return value === null || typeof value === 'string';
}

function permissionFlagsNamed(value: unknown): Array<bigint> {
	const names = readStringArray(value) ?? [];
	return permissionFlagsIn(names.reduce((mask, name) => mask | (PERMISSION_FLAGS_BY_NAME.get(name) ?? 0n), 0n));
}

function readNameChange(entry: GuildAuditLogEntryResponse): ValueChange<string> | null {
	const change = readChange(entry, 'name');
	if (!change) return null;
	const previous = readString(change.oldValue);
	const next = readString(change.newValue);
	return previous !== null && next !== null && previous !== next ? {previous, next} : null;
}

function readPermissionsDiffChanges(entry: GuildAuditLogEntryResponse): PermissionChanges | null {
	const value = readNewValue(entry, 'permissions_diff');
	if (typeof value !== 'object' || value === null) return null;
	return {
		added: permissionFlagsNamed('added' in value ? value.added : null),
		removed: permissionFlagsNamed('removed' in value ? value.removed : null),
	};
}

function diffPermissionValues(oldValue: unknown, newValue: unknown): PermissionChanges | null {
	const previous = readBitfield(oldValue);
	const next = readBitfield(newValue);
	return previous !== null && next !== null ? diffBitfields(previous, next) : null;
}

function readPermissionChanges(entry: GuildAuditLogEntryResponse): PermissionChanges | null {
	const change = readChange(entry, 'permissions');
	const changes = change ? diffPermissionValues(change.oldValue, change.newValue) : readPermissionsDiffChanges(entry);
	return changes !== null && (changes.added.length > 0 || changes.removed.length > 0) ? changes : null;
}

function readColorChange(entry: GuildAuditLogEntryResponse): ValueChange<number> | null {
	const change = readChange(entry, 'color');
	if (!change) return null;
	const previous = readColor(change.oldValue);
	const next = readColor(change.newValue);
	return previous !== null && next !== null && previous !== next ? {previous, next} : null;
}

function readToggleChange(entry: GuildAuditLogEntryResponse, key: string): boolean | null {
	const change = readChange(entry, key);
	if (!change) return null;
	const previous = readBoolean(change.oldValue);
	const next = readBoolean(change.newValue);
	return previous !== null && next !== null && previous !== next ? next : null;
}

function readOptionalTextChange(entry: GuildAuditLogEntryResponse, key: string): ValueChange<string | null> | null {
	const change = readChange(entry, key);
	if (!change || !isNullableString(change.oldValue) || !isNullableString(change.newValue)) return null;
	const previous = readString(change.oldValue);
	const next = readString(change.newValue);
	return previous !== next ? {previous, next} : null;
}

function readRoleListMove(entry: GuildAuditLogEntryResponse): ListDirection | null {
	const change = readChange(entry, 'position');
	if (!change) return null;
	const previous = readNumber(change.oldValue);
	const next = readNumber(change.newValue);
	if (previous === null || next === null || previous === next) return null;
	return next > previous ? 'up' : 'down';
}

function readMemberListPositionChange(entry: GuildAuditLogEntryResponse): MemberListPositionChange | null {
	const change = readChange(entry, 'hoist_position');
	if (!change) return null;
	const previous = readNumber(change.oldValue);
	const next = readNumber(change.newValue);
	if ((change.oldValue !== null && previous === null) || (change.newValue !== null && next === null)) return null;
	if (previous === null) return next === null ? null : 'set';
	if (next === null) return 'reset';
	if (previous === next) return null;
	return next > previous ? 'up' : 'down';
}

function colorRow({previous, next}: ValueChange<number>): AuditLogDetailRow {
	if (previous === 0) return row('color', 'add', ROLE_COLOR_SET_ROW, {color: colorValue(next)});
	if (next === 0) return row('color', 'remove', ROLE_COLOR_REMOVE_ROW);
	return row('color', 'neutral', ROLE_COLOR_CHANGE_ROW, {oldColor: colorValue(previous), newColor: colorValue(next)});
}

function emojiRow({previous, next}: ValueChange<string | null>): AuditLogDetailRow {
	if (next === null) return row('unicode-emoji', 'remove', ROLE_EMOJI_REMOVE_ROW);
	if (previous === null) return row('unicode-emoji', 'add', ROLE_EMOJI_SET_ROW, {emoji: nameValue(next)});
	return row('unicode-emoji', 'neutral', ROLE_EMOJI_CHANGE_ROW, {
		oldEmoji: nameValue(previous),
		newEmoji: nameValue(next),
	});
}

function iconRow({previous, next}: ValueChange<string | null>): AuditLogDetailRow {
	if (next === null) return row('icon-hash', 'remove', ROLE_ICON_REMOVE_ROW);
	if (previous === null) return row('icon-hash', 'add', ROLE_ICON_SET_ROW);
	return row('icon-hash', 'neutral', ROLE_ICON_CHANGE_ROW);
}

export function presentRoleCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const permissions = permissionFlagsIn(readBitfield(readNewValue(entry, 'permissions')) ?? 0n);
	const color = readColor(readNewValue(entry, 'color'));
	const emoji = readString(readNewValue(entry, 'unicode_emoji'));
	const rows: Array<AuditLogDetailRow> = [];
	if (permissions.length > 0) {
		rows.push(
			row('permissions-granted', 'add', ROLE_PERMISSIONS_GRANT_ROW, {permissions: permissionsValue(permissions)}),
		);
	}
	if (color !== null && color > 0) {
		rows.push(row('color', 'neutral', ROLE_COLOR_SET_ROW, {color: colorValue(color)}));
	}
	if (readBoolean(readNewValue(entry, 'hoist')) === true) {
		rows.push(row('hoist', 'add', ROLE_HOIST_CREATE_ROW));
	}
	if (readBoolean(readNewValue(entry, 'mentionable')) === true) {
		rows.push(row('mentionable', 'add', ROLE_MENTIONABLE_ALLOW_ROW));
	}
	if (emoji !== null) {
		rows.push(row('unicode-emoji', 'add', ROLE_EMOJI_SET_ROW, {emoji: nameValue(emoji)}));
	}
	if (readString(readNewValue(entry, 'icon_hash')) !== null) {
		rows.push(row('icon-hash', 'add', ROLE_ICON_SET_ROW));
	}
	return {
		summary: {
			descriptor: ROLE_CREATE_SUMMARY,
			values: {
				actor: actorPlaceholder(entry),
				role: rolePlaceholder(entry, readString(readNewValue(entry, 'name'))),
			},
		},
		rows,
		blocks: [],
	};
}

export function presentRoleUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const name = readNameChange(entry);
	const permissions = readPermissionChanges(entry);
	const color = readColorChange(entry);
	const hoist = readToggleChange(entry, 'hoist');
	const mentionable = readToggleChange(entry, 'mentionable');
	const emoji = readOptionalTextChange(entry, 'unicode_emoji');
	const icon = readOptionalTextChange(entry, 'icon_hash');
	const roleListMove = readRoleListMove(entry);
	const memberListPosition = readMemberListPositionChange(entry);
	const actor = actorPlaceholder(entry);
	const role = rolePlaceholder(
		entry,
		readString(readNewValue(entry, 'name')) ?? readString(readOption(entry, 'role_name')),
	);
	const changeCount = [
		name,
		permissions,
		color,
		hoist,
		mentionable,
		emoji,
		icon,
		roleListMove,
		memberListPosition,
	].filter((change) => change !== null).length;

	if (changeCount === 1 && roleListMove !== null) {
		return summaryOnly(ROLE_LIST_MOVE_SUMMARIES[roleListMove], {actor, role});
	}
	if (changeCount === 1 && memberListPosition !== null) {
		return summaryOnly(MEMBER_LIST_POSITION_SUMMARIES[memberListPosition], {actor, role});
	}
	if (changeCount === 1 && name !== null) {
		return summaryOnly(ROLE_RENAME_SUMMARY, {actor, oldName: nameValue(name.previous), newName: nameValue(name.next)});
	}

	const rows: Array<AuditLogDetailRow> = [];
	if (name !== null) {
		rows.push(
			row('name', 'neutral', ROLE_NAME_CHANGE_ROW, {oldName: nameValue(name.previous), newName: nameValue(name.next)}),
		);
	}
	if (permissions !== null && permissions.added.length > 0) {
		rows.push(
			row('permissions-granted', 'add', ROLE_PERMISSIONS_GRANT_ROW, {permissions: permissionsValue(permissions.added)}),
		);
	}
	if (permissions !== null && permissions.removed.length > 0) {
		rows.push(
			row('permissions-revoked', 'remove', ROLE_PERMISSIONS_REVOKE_ROW, {
				permissions: permissionsValue(permissions.removed),
			}),
		);
	}
	if (color !== null) {
		rows.push(colorRow(color));
	}
	if (hoist !== null) {
		rows.push(hoist ? row('hoist', 'add', ROLE_HOIST_START_ROW) : row('hoist', 'remove', ROLE_HOIST_STOP_ROW));
	}
	if (mentionable !== null) {
		rows.push(
			mentionable
				? row('mentionable', 'add', ROLE_MENTIONABLE_ALLOW_ROW)
				: row('mentionable', 'remove', ROLE_MENTIONABLE_DISALLOW_ROW),
		);
	}
	if (emoji !== null) {
		rows.push(emojiRow(emoji));
	}
	if (icon !== null) {
		rows.push(iconRow(icon));
	}
	if (roleListMove !== null) {
		rows.push(row('position', 'neutral', ROLE_LIST_MOVE_ROWS[roleListMove]));
	}
	if (memberListPosition !== null) {
		rows.push(row('hoist-position', 'neutral', MEMBER_LIST_POSITION_ROWS[memberListPosition]));
	}
	return {summary: {descriptor: ROLE_UPDATE_SUMMARY, values: {actor, role}}, rows, blocks: []};
}

export function presentRoleDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const permissions = permissionFlagsIn(readBitfield(readOldValue(entry, 'permissions')) ?? 0n);
	return {
		summary: {
			descriptor: ROLE_DELETE_SUMMARY,
			values: {
				actor: actorPlaceholder(entry),
				role: rolePlaceholder(entry, readString(readOldValue(entry, 'name'))),
			},
		},
		rows:
			permissions.length > 0
				? [row('permissions', 'remove', ROLE_DELETED_PERMISSIONS_ROW, {permissions: permissionsValue(permissions)})]
				: [],
		blocks: [],
	};
}
