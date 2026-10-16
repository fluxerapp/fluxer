// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	actorPlaceholder,
	readBoolean,
	readChange,
	readNumber,
	readSnowflake,
	readString,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {msg} from '@lingui/core/macro';

const THREAD_CREATE_SUMMARY = msg({
	message: '{actor} started the thread {channel}',
	comment:
		'Activity log summary when a thread was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the thread, shown as a channel chip with its name.',
});
const THREAD_UPDATE_SUMMARY = msg({
	message: '{actor} updated the thread {channel}',
	comment:
		'Activity log summary when a thread was changed. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the thread, shown as a channel chip with its name.',
});
const THREAD_DELETE_SUMMARY = msg({
	message: '{actor} deleted the thread {channel}',
	comment:
		'Activity log summary when a thread was deleted. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the thread, shown as a channel chip with its recorded name.',
});
const THREAD_NAME_CHANGED_ROW = msg({
	message: 'Changed the name from {oldName} to {newName}',
	comment:
		'Activity log detail row under an updated thread whose name changed. It is a past tense record, never an instruction. {oldName} and {newName} are shown in bold by the app.',
});
const THREAD_ARCHIVED_ROW = msg({
	message: 'Closed the thread',
	comment:
		'Activity log detail row under an updated thread that was archived. Past tense record, never an instruction.',
});
const THREAD_UNARCHIVED_ROW = msg({
	message: 'Opened the thread again',
	comment:
		'Activity log detail row under an updated thread that was unarchived. Past tense record, never an instruction.',
});
const THREAD_LOCKED_ROW = msg({
	message: 'Locked the thread',
	comment: 'Activity log detail row under an updated thread that was locked. Past tense record, never an instruction.',
});
const THREAD_UNLOCKED_ROW = msg({
	message: 'Unlocked the thread',
	comment:
		'Activity log detail row under an updated thread that was unlocked. Past tense record, never an instruction.',
});
const THREAD_AUTO_ARCHIVE_CHANGED_ROW = msg({
	message: 'Changed the hide after inactivity time to {duration}',
	comment:
		'Activity log detail row under an updated thread whose auto archive duration changed. {duration} is a formatted duration such as "1 hour". Past tense record, never an instruction.',
});
const THREAD_SLOWMODE_CHANGED_ROW = msg({
	message: 'Changed slowmode to {duration}',
	comment:
		'Activity log detail row under an updated thread whose slowmode changed. {duration} is a formatted duration such as "30 seconds". Past tense record, never an instruction.',
});

function threadPlaceholder(entry: GuildAuditLogEntryResponse, key: 'newValue' | 'oldValue'): AuditLogPlaceholder {
	return {
		kind: 'channel',
		id: readSnowflake(entry.target_id) ?? '',
		recordedName: readString(readChange(entry, 'name')?.[key]),
		fallback: 'channel',
	};
}

function booleanRow(
	entry: GuildAuditLogEntryResponse,
	key: string,
	onDescriptor: typeof THREAD_ARCHIVED_ROW,
	offDescriptor: typeof THREAD_ARCHIVED_ROW,
): AuditLogDetailRow | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const after = readBoolean(change.newValue);
	if (after === null || after === readBoolean(change.oldValue)) return null;
	return after
		? {id: key, tone: 'remove', sentence: {descriptor: onDescriptor, values: {}}}
		: {id: key, tone: 'add', sentence: {descriptor: offDescriptor, values: {}}};
}

function durationRow(
	entry: GuildAuditLogEntryResponse,
	key: string,
	descriptor: typeof THREAD_SLOWMODE_CHANGED_ROW,
	secondsPerUnit: number,
): AuditLogDetailRow | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const after = readNumber(change.newValue);
	if (after === null || after === readNumber(change.oldValue)) return null;
	return {
		id: key,
		tone: 'neutral',
		sentence: {descriptor, values: {duration: {kind: 'duration', seconds: after * secondsPerUnit}}},
	};
}

function nameRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const change = readChange(entry, 'name');
	const before = readString(change?.oldValue);
	const after = readString(change?.newValue);
	if (before === null || after === null || before === after) return null;
	return {
		id: 'name',
		tone: 'neutral',
		sentence: {
			descriptor: THREAD_NAME_CHANGED_ROW,
			values: {oldName: {kind: 'name', value: before}, newName: {kind: 'name', value: after}},
		},
	};
}

export function presentThreadCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: {
			descriptor: THREAD_CREATE_SUMMARY,
			values: {actor: actorPlaceholder(entry), channel: threadPlaceholder(entry, 'newValue')},
		},
		rows: [],
		blocks: [],
	};
}

export function presentThreadUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const rows = [
		nameRow(entry),
		booleanRow(entry, 'archived', THREAD_ARCHIVED_ROW, THREAD_UNARCHIVED_ROW),
		booleanRow(entry, 'locked', THREAD_LOCKED_ROW, THREAD_UNLOCKED_ROW),
		durationRow(entry, 'auto_archive_duration', THREAD_AUTO_ARCHIVE_CHANGED_ROW, 60),
		durationRow(entry, 'rate_limit_per_user', THREAD_SLOWMODE_CHANGED_ROW, 1),
	].filter((row): row is AuditLogDetailRow => row !== null);
	return {
		summary: {
			descriptor: THREAD_UPDATE_SUMMARY,
			values: {actor: actorPlaceholder(entry), channel: threadPlaceholder(entry, 'newValue')},
		},
		rows,
		blocks: [],
	};
}

export function presentThreadDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: {
			descriptor: THREAD_DELETE_SUMMARY,
			values: {actor: actorPlaceholder(entry), channel: threadPlaceholder(entry, 'oldValue')},
		},
		rows: [],
		blocks: [],
	};
}
