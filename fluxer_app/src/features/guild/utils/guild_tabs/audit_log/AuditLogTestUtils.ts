// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AuditLogBlock,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogPresentationContext,
	AuditLogSentence,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	MORE_PERMISSIONS_DESCRIPTOR,
	SYSTEM_ACTOR_LABEL,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {largestDurationUnit} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {
	DAYS_DURATION_PLURAL_DESCRIPTOR,
	HOURS_DURATION_PLURAL_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getPermissionTitleDescriptor} from '@app/features/permissions/utils/PermissionLabelDescriptors';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {setupI18n} from '@lingui/core';

export const TEST_GUILD_ID = '1400000000000000000';
export const TEST_ACTOR_ID = '1400000000000000001';
export const TEST_ENTRY_TIME = Date.parse('2026-09-01T12:00:00.000Z');

const TEST_ENTRY_ID = SnowflakeUtils.fromTimestamp(TEST_ENTRY_TIME);
const PERMISSION_LIST_LIMIT = 8;
const PERMISSION_LIST_SHOWN = 7;
const MESSAGE_ARGUMENT_PATTERN = /\{\s*(\w+)\s*[,}]/g;
const TONE_MARKS: Record<AuditLogTone, string> = {add: '+', remove: '-', neutral: '~'};

const i18n = setupI18n({locale: 'en', messages: {en: {}}});
const listFormatter = new Intl.ListFormat('en', {type: 'conjunction', style: 'long'});

interface AuditLogEntryFixture {
	action_type: number;
	id?: string;
	user_id?: string | null;
	target_id?: string | null;
	reason?: string;
	options?: Record<string, unknown>;
	changes?: Array<{key: string; old_value?: unknown; new_value?: unknown}>;
}

interface FakeContextNames {
	webhooks?: Readonly<Record<string, string>>;
	emojis?: Readonly<Record<string, string>>;
	stickers?: Readonly<Record<string, string>>;
}

interface AuditLogResultText {
	summary: string;
	rows: Array<string>;
	blocks: Array<AuditLogBlock>;
}

export function makeEntry(fixture: AuditLogEntryFixture): GuildAuditLogEntryResponse {
	return {id: TEST_ENTRY_ID, user_id: TEST_ACTOR_ID, target_id: null, ...fixture} as GuildAuditLogEntryResponse;
}

export function fakeContext(names: FakeContextNames = {}): AuditLogPresentationContext {
	return {
		guildId: TEST_GUILD_ID,
		getWebhookName: (id) => names.webhooks?.[id] ?? null,
		getEmojiName: (id) => names.emojis?.[id] ?? null,
		getStickerName: (id) => names.stickers?.[id] ?? null,
	};
}

function formatDuration(seconds: number): string {
	const {unit, value} = largestDurationUnit(seconds);
	switch (unit) {
		case 'days':
			return i18n._(DAYS_DURATION_PLURAL_DESCRIPTOR, {days: value});
		case 'hours':
			return i18n._(HOURS_DURATION_PLURAL_DESCRIPTOR, {hours: value});
		case 'minutes':
			return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes: value});
		case 'seconds':
			return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds: value});
	}
}

function formatPermissions(flags: ReadonlyArray<bigint>): string {
	const titles = flags.map((flag) => {
		const descriptor = getPermissionTitleDescriptor(flag);
		return descriptor ? i18n._(descriptor) : flag.toString();
	});
	if (titles.length <= PERMISSION_LIST_LIMIT) return listFormatter.format(titles);
	return listFormatter.format([
		...titles.slice(0, PERMISSION_LIST_SHOWN),
		i18n._(MORE_PERMISSIONS_DESCRIPTOR, {count: titles.length - PERMISSION_LIST_SHOWN}),
	]);
}

function formatColor(value: number): string {
	return `#${value.toString(16).padStart(6, '0').toUpperCase()}`;
}

function placeholderToText(placeholder: AuditLogPlaceholder, userNames: Readonly<Record<string, string>>): string {
	switch (placeholder.kind) {
		case 'user':
			return userNames[placeholder.id] ?? `@${placeholder.id}`;
		case 'system':
			return i18n._(SYSTEM_ACTOR_LABEL);
		case 'channel':
			return `#${placeholder.recordedName ?? placeholder.id}`;
		case 'role':
			return placeholder.id === TEST_GUILD_ID ? '@everyone' : `@${placeholder.recordedName ?? placeholder.id}`;
		case 'name':
		case 'text':
			return placeholder.value;
		case 'emoji':
			return `:${placeholder.name}:`;
		case 'date':
			return new Date(placeholder.timestamp).toISOString();
		case 'duration':
			return formatDuration(placeholder.seconds);
		case 'permissions':
			return formatPermissions(placeholder.flags);
		case 'color':
			return formatColor(placeholder.value);
		case 'label':
			return i18n._(placeholder.descriptor);
	}
}

export function toText(sentence: AuditLogSentence, userNames: Readonly<Record<string, string>> = {}): string {
	const message = sentence.descriptor.message ?? '';
	const argumentNames = new Set(Array.from(message.matchAll(MESSAGE_ARGUMENT_PATTERN), (match) => match[1]));
	const passedNames = Object.keys(sentence.values);
	const unused = passedNames.filter((name) => !argumentNames.has(name));
	const missing = [...argumentNames].filter((name) => !Object.hasOwn(sentence.values, name));
	if (unused.length > 0 || missing.length > 0) {
		throw new Error(
			`Placeholders do not match "${message}". Not in the message: [${unused.join(', ')}]. Not passed: [${missing.join(', ')}].`,
		);
	}
	const values: Record<string, string | number> = {};
	for (const [name, value] of Object.entries(sentence.values)) {
		values[name] = typeof value === 'number' ? value : placeholderToText(value, userNames);
	}
	return i18n._(sentence.descriptor, values);
}

export function resultToText(
	result: AuditLogDomainResult,
	userNames: Readonly<Record<string, string>> = {},
): AuditLogResultText {
	return {
		summary: toText(result.summary, userNames),
		rows: result.rows.map((row) => `${TONE_MARKS[row.tone]} ${toText(row.sentence, userNames)}`),
		blocks: result.blocks,
	};
}
