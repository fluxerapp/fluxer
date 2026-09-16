// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuditLogPlaceholder} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {SECONDS_PER_DAY, SECONDS_PER_HOUR, SECONDS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

const SYSTEM_USER_ID = '0';
const PERMISSION_FLAGS: ReadonlyArray<bigint> = Object.values(Permissions);
const DECIMAL_NUMBER_PATTERN = /^-?\d+(\.\d+)?$/;
const UNSIGNED_INTEGER_PATTERN = /^\d+$/;
const SNOWFLAKE_PATTERN = /^\d{1,20}$/;
const PERCENT_ENCODED_BYTE_PATTERN = /%[0-9A-Fa-f]{2}/;

interface ChangeValues {
	hasOld: boolean;
	hasNew: boolean;
	oldValue: unknown;
	newValue: unknown;
}

interface OverwriteState {
	allow: bigint;
	deny: bigint;
}

function hasFlag(mask: bigint, flag: bigint): boolean {
	return (mask & flag) === flag;
}

export function readChange(entry: GuildAuditLogEntryResponse, key: string): ChangeValues | null {
	const change = entry.changes?.find((candidate) => candidate.key === key);
	if (!change) return null;
	const hasOld = Object.hasOwn(change, 'old_value');
	const hasNew = Object.hasOwn(change, 'new_value');
	return {
		hasOld,
		hasNew,
		oldValue: hasOld ? change.old_value : null,
		newValue: hasNew ? change.new_value : null,
	};
}

export function readString(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : null;
}

export function readNumber(value: unknown): number | null {
	if (typeof value === 'number') return Number.isFinite(value) ? value : null;
	if (typeof value === 'string' && DECIMAL_NUMBER_PATTERN.test(value.trim())) return Number(value);
	return null;
}

export function readBoolean(value: unknown): boolean | null {
	if (typeof value === 'boolean') return value;
	if (value === 'true') return true;
	if (value === 'false') return false;
	return null;
}

export function readTimestamp(value: unknown): number | null {
	const text = readString(value);
	if (text === null) return null;
	const timestamp = Date.parse(text);
	return Number.isNaN(timestamp) ? null : timestamp;
}

export function readBitfield(value: unknown): bigint | null {
	if (typeof value === 'string') return UNSIGNED_INTEGER_PATTERN.test(value) ? BigInt(value) : null;
	if (typeof value === 'number') return Number.isSafeInteger(value) && value >= 0 ? BigInt(value) : null;
	return null;
}

export function readSnowflake(value: unknown): string | null {
	return typeof value === 'string' && SNOWFLAKE_PATTERN.test(value) ? value : null;
}

export function readStringArray(value: unknown): Array<string> | null {
	if (!Array.isArray(value)) return null;
	return value.every((item): item is string => typeof item === 'string') ? [...value] : null;
}

export function readOption(entry: GuildAuditLogEntryResponse, key: string): unknown {
	const options: Readonly<Record<string, unknown>> | undefined = entry.options;
	return options?.[key] ?? null;
}

export function entryTime(entry: GuildAuditLogEntryResponse): number {
	return SnowflakeUtils.extractTimestamp(entry.id);
}

export function actorPlaceholder(entry: GuildAuditLogEntryResponse): AuditLogPlaceholder {
	return entry.user_id === SYSTEM_USER_ID ? {kind: 'system'} : {kind: 'user', id: entry.user_id ?? ''};
}

export function decodeAuditReason(raw: unknown): string | null {
	const trimmed = readString(raw);
	if (trimmed === null || !PERCENT_ENCODED_BYTE_PATTERN.test(trimmed)) return trimmed;
	try {
		return readString(decodeURIComponent(trimmed));
	} catch {
		return trimmed;
	}
}

export function permissionFlagsIn(mask: bigint): Array<bigint> {
	return PERMISSION_FLAGS.filter((flag) => hasFlag(mask, flag));
}

export function diffBitfields(oldMask: bigint, newMask: bigint): {added: Array<bigint>; removed: Array<bigint>} {
	return {
		added: PERMISSION_FLAGS.filter((flag) => hasFlag(newMask, flag) && !hasFlag(oldMask, flag)),
		removed: PERMISSION_FLAGS.filter((flag) => hasFlag(oldMask, flag) && !hasFlag(newMask, flag)),
	};
}

export function diffOverwriteStates(
	previous: OverwriteState,
	next: OverwriteState,
): {allowed: Array<bigint>; denied: Array<bigint>; cleared: Array<bigint>} {
	return {
		allowed: PERMISSION_FLAGS.filter((flag) => hasFlag(next.allow, flag) && !hasFlag(previous.allow, flag)),
		denied: PERMISSION_FLAGS.filter((flag) => hasFlag(next.deny, flag) && !hasFlag(previous.deny, flag)),
		cleared: PERMISSION_FLAGS.filter(
			(flag) =>
				(hasFlag(previous.allow, flag) || hasFlag(previous.deny, flag)) &&
				!hasFlag(next.allow, flag) &&
				!hasFlag(next.deny, flag),
		),
	};
}

export function largestDurationUnit(seconds: number): {unit: 'days' | 'hours' | 'minutes' | 'seconds'; value: number} {
	const rounded = Math.round(seconds);
	if (rounded === 0) return {unit: 'seconds', value: 0};
	if (rounded % SECONDS_PER_DAY === 0) return {unit: 'days', value: rounded / SECONDS_PER_DAY};
	if (rounded % SECONDS_PER_HOUR === 0) return {unit: 'hours', value: rounded / SECONDS_PER_HOUR};
	if (rounded % SECONDS_PER_MINUTE === 0) return {unit: 'minutes', value: rounded / SECONDS_PER_MINUTE};
	return {unit: 'seconds', value: rounded};
}

export function channelNoun(type: number | null): 'text' | 'voice' | 'link' | 'category' | 'generic' {
	switch (type) {
		case ChannelTypes.GUILD_TEXT:
			return 'text';
		case ChannelTypes.GUILD_VOICE:
			return 'voice';
		case ChannelTypes.GUILD_LINK:
			return 'link';
		case ChannelTypes.GUILD_CATEGORY:
			return 'category';
		default:
			return 'generic';
	}
}

export function roleIdDiff(
	previous: ReadonlyArray<string>,
	next: ReadonlyArray<string>,
): {added: Array<string>; removed: Array<string>} {
	const previousIds = new Set(previous);
	const nextIds = new Set(next);
	return {
		added: [...nextIds].filter((id) => !previousIds.has(id)),
		removed: [...previousIds].filter((id) => !nextIds.has(id)),
	};
}

export function featureDelta(
	previous: ReadonlyArray<string>,
	next: ReadonlyArray<string>,
	feature: string,
): 'added' | 'removed' | null {
	const hadFeature = previous.includes(feature);
	const hasFeature = next.includes(feature);
	if (hadFeature === hasFeature) return null;
	return hasFeature ? 'added' : 'removed';
}
