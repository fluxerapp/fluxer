// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EVENT_CREATE_SUMMARY,
	EVENT_CREATE_UNNAMED_SUMMARY,
	EVENT_CREATOR_ROW,
	EVENT_DELETE_SUMMARY,
	EVENT_DELETE_UNNAMED_SUMMARY,
	EVENT_DESCRIPTION_CHANGED_ROW,
	EVENT_DESCRIPTION_REMOVED_ROW,
	EVENT_DESCRIPTION_SET_ROW,
	EVENT_END_CHANGED_ROW,
	EVENT_END_REMOVED_ROW,
	EVENT_END_SET_ROW,
	EVENT_IMAGE_ADDED_ROW,
	EVENT_IMAGE_CHANGED_ROW,
	EVENT_IMAGE_REMOVED_ROW,
	EVENT_LOCATION_CHANGED_ROW,
	EVENT_LOCATION_REMOVED_ROW,
	EVENT_LOCATION_SET_ROW,
	EVENT_NAME_CHANGED_ROW,
	EVENT_START_CHANGED_ROW,
	EVENT_START_SET_ROW,
	EVENT_UPDATE_SUMMARY,
	EVENT_UPDATE_UNNAMED_SUMMARY,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogEventMessages';
import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogSentence,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	actorPlaceholder,
	readChange,
	readSnowflake,
	readString,
	readTimestamp,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

interface ValueSentences {
	set: MessageDescriptor;
	changed: MessageDescriptor;
	removed?: MessageDescriptor;
}

function readEditableChange(entry: GuildAuditLogEntryResponse, key: string): ReturnType<typeof readChange> {
	const change = readChange(entry, key);
	if (change === null || !change.hasNew) return null;
	if (entry.action_type === AuditLogActionType.GUILD_EVENT_UPDATE && !change.hasOld) return null;
	return change;
}

function readNullableText(value: unknown): string | null | undefined {
	if (value === null) return null;
	return typeof value === 'string' ? readString(value) : undefined;
}

function readNullableTimestamp(value: unknown): number | null | undefined {
	return value === null ? null : (readTimestamp(value) ?? undefined);
}

function textPlaceholder(value: string): AuditLogPlaceholder {
	return {kind: 'text', value};
}

function datePlaceholder(timestamp: number): AuditLogPlaceholder {
	return {kind: 'date', timestamp};
}

function valueChangeRow<T>(
	entry: GuildAuditLogEntryResponse,
	key: string,
	readValue: (value: unknown) => T | null | undefined,
	placeholder: (value: T) => AuditLogPlaceholder,
	sentences: ValueSentences,
): AuditLogDetailRow | null {
	const change = readEditableChange(entry, key);
	if (change === null) return null;
	const oldValue = readValue(change.oldValue);
	const newValue = readValue(change.newValue);
	if (oldValue === undefined || newValue === undefined || oldValue === newValue) return null;
	if (newValue === null) {
		if (oldValue === null || sentences.removed === undefined) return null;
		return {
			id: key,
			tone: 'remove',
			sentence: {descriptor: sentences.removed, values: {value: placeholder(oldValue)}},
		};
	}
	if (oldValue === null) {
		return {
			id: key,
			tone: 'add',
			sentence: {descriptor: sentences.set, values: {value: placeholder(newValue)}},
		};
	}
	return {
		id: key,
		tone: 'neutral',
		sentence: {
			descriptor: sentences.changed,
			values: {oldValue: placeholder(oldValue), newValue: placeholder(newValue)},
		},
	};
}

function eventRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const rows: Array<AuditLogDetailRow> = [];
	const nameChange = readChange(entry, 'name');
	const oldName = readString(nameChange?.oldValue);
	const newName = readString(nameChange?.newValue);
	if (oldName !== null && newName !== null && oldName !== newName) {
		rows.push({
			id: 'name',
			tone: 'neutral',
			sentence: {
				descriptor: EVENT_NAME_CHANGED_ROW,
				values: {oldValue: {kind: 'name', value: oldName}, newValue: {kind: 'name', value: newName}},
			},
		});
	}
	const detailRows = [
		valueChangeRow(entry, 'description', readNullableText, textPlaceholder, {
			set: EVENT_DESCRIPTION_SET_ROW,
			changed: EVENT_DESCRIPTION_CHANGED_ROW,
			removed: EVENT_DESCRIPTION_REMOVED_ROW,
		}),
		valueChangeRow(entry, 'location', readNullableText, textPlaceholder, {
			set: EVENT_LOCATION_SET_ROW,
			changed: EVENT_LOCATION_CHANGED_ROW,
			removed: EVENT_LOCATION_REMOVED_ROW,
		}),
		valueChangeRow(entry, 'starts_at', readNullableTimestamp, datePlaceholder, {
			set: EVENT_START_SET_ROW,
			changed: EVENT_START_CHANGED_ROW,
		}),
		valueChangeRow(entry, 'ends_at', readNullableTimestamp, datePlaceholder, {
			set: EVENT_END_SET_ROW,
			changed: EVENT_END_CHANGED_ROW,
			removed: EVENT_END_REMOVED_ROW,
		}),
	];
	for (const row of detailRows) {
		if (row !== null) rows.push(row);
	}
	const imageChange = readEditableChange(entry, 'image_hash');
	if (imageChange !== null) {
		const oldImage = readNullableText(imageChange.oldValue);
		const newImage = readNullableText(imageChange.newValue);
		if (oldImage !== undefined && newImage !== undefined && oldImage !== newImage) {
			rows.push({
				id: 'image_hash',
				tone: newImage === null ? 'remove' : oldImage === null ? 'add' : 'neutral',
				sentence: {
					descriptor:
						newImage === null
							? EVENT_IMAGE_REMOVED_ROW
							: oldImage === null
								? EVENT_IMAGE_ADDED_ROW
								: EVENT_IMAGE_CHANGED_ROW,
					values: {},
				},
			});
		}
	}
	return rows;
}

function eventSummary(
	entry: GuildAuditLogEntryResponse,
	name: string | null,
	named: MessageDescriptor,
	unnamed: MessageDescriptor,
): AuditLogSentence {
	const actor = actorPlaceholder(entry);
	return name === null
		? {descriptor: unnamed, values: {actor}}
		: {descriptor: named, values: {actor, name: {kind: 'name', value: name}}};
}

export function presentEventCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: eventSummary(
			entry,
			readString(readChange(entry, 'name')?.newValue),
			EVENT_CREATE_SUMMARY,
			EVENT_CREATE_UNNAMED_SUMMARY,
		),
		rows: eventRows(entry),
		blocks: [],
	};
}

export function presentEventUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const nameChange = readChange(entry, 'name');
	return {
		summary: eventSummary(
			entry,
			readString(nameChange?.newValue) ?? readString(nameChange?.oldValue),
			EVENT_UPDATE_SUMMARY,
			EVENT_UPDATE_UNNAMED_SUMMARY,
		),
		rows: eventRows(entry),
		blocks: [],
	};
}

export function presentEventDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const creatorId = readSnowflake(readChange(entry, 'creator_id')?.oldValue);
	const rows: Array<AuditLogDetailRow> = [];
	if (creatorId !== null && creatorId !== entry.user_id) {
		rows.push({
			id: 'creator',
			tone: 'neutral',
			sentence: {descriptor: EVENT_CREATOR_ROW, values: {user: {kind: 'user', id: creatorId}}},
		});
	}
	return {
		summary: eventSummary(
			entry,
			readString(readChange(entry, 'name')?.oldValue),
			EVENT_DELETE_SUMMARY,
			EVENT_DELETE_UNNAMED_SUMMARY,
		),
		rows,
		blocks: [],
	};
}
