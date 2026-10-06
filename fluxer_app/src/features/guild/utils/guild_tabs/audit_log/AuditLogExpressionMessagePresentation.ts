// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EMOJI_CREATE_SUMMARY,
	EMOJI_CREATE_UNNAMED_SUMMARY,
	EMOJI_DELETE_SUMMARY,
	EMOJI_DELETE_UNNAMED_SUMMARY,
	EMOJI_RENAME_SUMMARY,
	EMOJI_UPDATE_SUMMARY,
	EMOJI_UPDATE_UNNAMED_SUMMARY,
	EMOJI_UPLOADER_ROW,
	MESSAGE_BULK_DELETE_IN_CHANNEL_SUMMARY,
	MESSAGE_BULK_DELETE_SUMMARY,
	MESSAGE_BULK_DELETE_UNCOUNTED_IN_CHANNEL_SUMMARY,
	MESSAGE_BULK_DELETE_UNCOUNTED_SUMMARY,
	MESSAGE_DELETE_IN_CHANNEL_SUMMARY,
	MESSAGE_DELETE_SUMMARY,
	MESSAGE_PIN_IN_CHANNEL_SUMMARY,
	MESSAGE_PIN_SUMMARY,
	MESSAGE_UNPIN_IN_CHANNEL_SUMMARY,
	MESSAGE_UNPIN_SUMMARY,
	STICKER_CREATE_SUMMARY,
	STICKER_CREATE_UNNAMED_SUMMARY,
	STICKER_DELETE_SUMMARY,
	STICKER_DELETE_UNNAMED_SUMMARY,
	STICKER_DESCRIPTION_CHANGED_ROW,
	STICKER_DESCRIPTION_REMOVED_ROW,
	STICKER_DESCRIPTION_SET_ROW,
	STICKER_NAME_CHANGED_ROW,
	STICKER_RENAME_SUMMARY,
	STICKER_UPDATE_SUMMARY,
	STICKER_UPDATE_UNNAMED_SUMMARY,
	STICKER_UPLOADER_ROW,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogExpressionMessageMessages';
import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogPresentationContext,
	AuditLogSentence,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	actorPlaceholder,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

interface NameChange {
	oldName: string;
	newName: string;
}

function summaryOnly(summary: AuditLogSentence): AuditLogDomainResult {
	return {summary, rows: [], blocks: []};
}

function namePlaceholder(value: string): AuditLogPlaceholder {
	return {kind: 'name', value};
}

function textPlaceholder(value: string): AuditLogPlaceholder {
	return {kind: 'text', value};
}

function readOldString(entry: GuildAuditLogEntryResponse, key: string): string | null {
	return readString(readChange(entry, key)?.oldValue);
}

function readNewString(entry: GuildAuditLogEntryResponse, key: string): string | null {
	return readString(readChange(entry, key)?.newValue);
}

function readNameChange(entry: GuildAuditLogEntryResponse): NameChange | null {
	const oldName = readOldString(entry, 'name');
	const newName = readNewString(entry, 'name');
	if (oldName === null || newName === null || oldName === newName) return null;
	return {oldName, newName};
}

function readCurrentName(entry: GuildAuditLogEntryResponse, lookup: (id: string) => string | null): string | null {
	const recordedName = readNewString(entry, 'name');
	if (recordedName !== null) return recordedName;
	const targetId = readSnowflake(entry.target_id);
	return targetId === null ? null : readString(lookup(targetId));
}

function uploaderRows(entry: GuildAuditLogEntryResponse, descriptor: MessageDescriptor): Array<AuditLogDetailRow> {
	const creatorId = readSnowflake(readChange(entry, 'creator_id')?.oldValue);
	if (creatorId === null || creatorId === entry.user_id) return [];
	return [{id: 'uploader', tone: 'neutral', sentence: {descriptor, values: {user: {kind: 'user', id: creatorId}}}}];
}

function readStickerDescriptionRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const oldText = readOldString(entry, 'description');
	const newText = readNewString(entry, 'description');
	if (oldText === null) {
		if (newText === null) return null;
		return {
			id: 'description',
			tone: 'add',
			sentence: {descriptor: STICKER_DESCRIPTION_SET_ROW, values: {text: textPlaceholder(newText)}},
		};
	}
	if (newText === null) {
		return {
			id: 'description',
			tone: 'remove',
			sentence: {descriptor: STICKER_DESCRIPTION_REMOVED_ROW, values: {text: textPlaceholder(oldText)}},
		};
	}
	if (oldText === newText) return null;
	return {
		id: 'description',
		tone: 'neutral',
		sentence: {
			descriptor: STICKER_DESCRIPTION_CHANGED_ROW,
			values: {oldText: textPlaceholder(oldText), newText: textPlaceholder(newText)},
		},
	};
}

function readMessageChannel(entry: GuildAuditLogEntryResponse): AuditLogPlaceholder | null {
	const channelId = readSnowflake(readOption(entry, 'channel_id'));
	return channelId === null ? null : {kind: 'channel', id: channelId, recordedName: null, fallback: 'channel'};
}

function readDeletedMessageCount(entry: GuildAuditLogEntryResponse): number | null {
	const count = readNumber(readOption(entry, 'count'));
	return count !== null && Number.isInteger(count) && count > 0 ? count : null;
}

function presentMessageInChannel(
	entry: GuildAuditLogEntryResponse,
	inChannel: MessageDescriptor,
	withoutChannel: MessageDescriptor,
): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const channel = readMessageChannel(entry);
	return summaryOnly(
		channel === null
			? {descriptor: withoutChannel, values: {actor}}
			: {descriptor: inChannel, values: {actor, channel}},
	);
}

export function presentEmojiCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const name = readNewString(entry, 'name');
	if (name === null) return summaryOnly({descriptor: EMOJI_CREATE_UNNAMED_SUMMARY, values: {actor}});
	return summaryOnly({
		descriptor: EMOJI_CREATE_SUMMARY,
		values: {actor, emoji: {kind: 'emoji', id: readSnowflake(entry.target_id), name}},
	});
}

export function presentEmojiUpdate(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const emojiId = readSnowflake(entry.target_id);
	const nameChange = readNameChange(entry);
	if (nameChange !== null) {
		return summaryOnly({
			descriptor: EMOJI_RENAME_SUMMARY,
			values: {
				actor,
				oldEmoji: {kind: 'emoji', id: null, name: nameChange.oldName},
				newEmoji: {kind: 'emoji', id: emojiId, name: nameChange.newName},
			},
		});
	}
	const name = readCurrentName(entry, (id) => context.getEmojiName(id));
	if (name === null) return summaryOnly({descriptor: EMOJI_UPDATE_UNNAMED_SUMMARY, values: {actor}});
	return summaryOnly({descriptor: EMOJI_UPDATE_SUMMARY, values: {actor, emoji: {kind: 'emoji', id: emojiId, name}}});
}

export function presentEmojiDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const name = readOldString(entry, 'name');
	return {
		summary:
			name === null
				? {descriptor: EMOJI_DELETE_UNNAMED_SUMMARY, values: {actor}}
				: {descriptor: EMOJI_DELETE_SUMMARY, values: {actor, emoji: {kind: 'emoji', id: null, name}}},
		rows: uploaderRows(entry, EMOJI_UPLOADER_ROW),
		blocks: [],
	};
}

export function presentStickerCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const name = readNewString(entry, 'name');
	const description = readNewString(entry, 'description');
	return {
		summary:
			name === null
				? {descriptor: STICKER_CREATE_UNNAMED_SUMMARY, values: {actor}}
				: {descriptor: STICKER_CREATE_SUMMARY, values: {actor, name: namePlaceholder(name)}},
		rows:
			description === null
				? []
				: [
						{
							id: 'description',
							tone: 'neutral',
							sentence: {descriptor: STICKER_DESCRIPTION_SET_ROW, values: {text: textPlaceholder(description)}},
						},
					],
		blocks: [],
	};
}

export function presentStickerUpdate(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const nameChange = readNameChange(entry);
	const descriptionRow = readStickerDescriptionRow(entry);
	if (nameChange !== null && descriptionRow === null) {
		return summaryOnly({
			descriptor: STICKER_RENAME_SUMMARY,
			values: {actor, oldName: namePlaceholder(nameChange.oldName), newName: namePlaceholder(nameChange.newName)},
		});
	}
	const name = readCurrentName(entry, (id) => context.getStickerName(id));
	const rows: Array<AuditLogDetailRow> = [];
	if (nameChange !== null) {
		rows.push({
			id: 'name',
			tone: 'neutral',
			sentence: {
				descriptor: STICKER_NAME_CHANGED_ROW,
				values: {oldName: namePlaceholder(nameChange.oldName), newName: namePlaceholder(nameChange.newName)},
			},
		});
	}
	if (descriptionRow !== null) rows.push(descriptionRow);
	return {
		summary:
			name === null
				? {descriptor: STICKER_UPDATE_UNNAMED_SUMMARY, values: {actor}}
				: {descriptor: STICKER_UPDATE_SUMMARY, values: {actor, name: namePlaceholder(name)}},
		rows,
		blocks: [],
	};
}

export function presentStickerDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const name = readOldString(entry, 'name');
	return {
		summary:
			name === null
				? {descriptor: STICKER_DELETE_UNNAMED_SUMMARY, values: {actor}}
				: {descriptor: STICKER_DELETE_SUMMARY, values: {actor, name: namePlaceholder(name)}},
		rows: uploaderRows(entry, STICKER_UPLOADER_ROW),
		blocks: [],
	};
}

export function presentMessageDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return presentMessageInChannel(entry, MESSAGE_DELETE_IN_CHANNEL_SUMMARY, MESSAGE_DELETE_SUMMARY);
}

export function presentMessageBulkDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const count = readDeletedMessageCount(entry);
	if (count === null) {
		return presentMessageInChannel(
			entry,
			MESSAGE_BULK_DELETE_UNCOUNTED_IN_CHANNEL_SUMMARY,
			MESSAGE_BULK_DELETE_UNCOUNTED_SUMMARY,
		);
	}
	const actor = actorPlaceholder(entry);
	const channel = readMessageChannel(entry);
	return summaryOnly(
		channel === null
			? {descriptor: MESSAGE_BULK_DELETE_SUMMARY, values: {actor, count}}
			: {descriptor: MESSAGE_BULK_DELETE_IN_CHANNEL_SUMMARY, values: {actor, count, channel}},
	);
}

export function presentMessagePin(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return presentMessageInChannel(entry, MESSAGE_PIN_IN_CHANNEL_SUMMARY, MESSAGE_PIN_SUMMARY);
}

export function presentMessageUnpin(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return presentMessageInChannel(entry, MESSAGE_UNPIN_IN_CHANNEL_SUMMARY, MESSAGE_UNPIN_SUMMARY);
}
