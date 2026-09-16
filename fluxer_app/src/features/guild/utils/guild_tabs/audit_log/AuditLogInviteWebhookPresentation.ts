// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	INVITE_CREATE_FOR_CHANNEL_SUMMARY,
	INVITE_CREATE_SUMMARY,
	INVITE_CREATED_BY_ON_ROW,
	INVITE_CREATED_BY_ROW,
	INVITE_CREATED_ON_ROW,
	INVITE_DELETE_FOR_CHANNEL_SUMMARY,
	INVITE_DELETE_SUMMARY,
	INVITE_EXPIRES_AFTER_ROW,
	INVITE_EXPIRES_ON_ROW,
	INVITE_NEVER_EXPIRES_ROW,
	INVITE_NEVER_USED_ROW,
	INVITE_TEMPORARY_MEMBERSHIP_ROW,
	INVITE_UNLIMITED_USES_ROW,
	INVITE_UPDATE_SUMMARY,
	INVITE_USE_COUNT_ROW,
	INVITE_USE_LIMIT_ROW,
	WEBHOOK_AVATAR_ADDED_ROW,
	WEBHOOK_AVATAR_CHANGED_ROW,
	WEBHOOK_AVATAR_REMOVED_ROW,
	WEBHOOK_CHANNEL_CHANGED_ROW,
	WEBHOOK_CREATE_IN_CHANNEL_SUMMARY,
	WEBHOOK_CREATE_SUMMARY,
	WEBHOOK_CREATE_UNNAMED_SUMMARY,
	WEBHOOK_CREATED_BY_ROW,
	WEBHOOK_DELETE_FROM_CHANNEL_SUMMARY,
	WEBHOOK_DELETE_SUMMARY,
	WEBHOOK_DELETE_UNNAMED_SUMMARY,
	WEBHOOK_NAME_CHANGED_ROW,
	WEBHOOK_RENAME_SUMMARY,
	WEBHOOK_UPDATE_SUMMARY,
	WEBHOOK_UPDATE_UNNAMED_SUMMARY,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogInviteWebhookMessages';
import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogPresentationContext,
	AuditLogSentence,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	actorPlaceholder,
	readBoolean,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
	readTimestamp,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

type ChangeSide = 'old' | 'new';

interface InviteSettings {
	maxAge: number | null;
	maxUses: number | null;
	temporary: boolean | null;
}

interface WebhookSnapshotSummaries {
	inChannel: MessageDescriptor;
	named: MessageDescriptor;
	unnamed: MessageDescriptor;
}

function readChangeSide(entry: GuildAuditLogEntryResponse, key: string, side: ChangeSide): unknown {
	const change = readChange(entry, key);
	if (change === null) return null;
	return side === 'old' ? change.oldValue : change.newValue;
}

function readCount(value: unknown): number | null {
	const count = readNumber(value);
	return count !== null && Number.isInteger(count) && count >= 0 ? count : null;
}

function row(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: AuditLogSentence['values'] = {},
): AuditLogDetailRow {
	return {id, tone, sentence: {descriptor, values}};
}

function channelPlaceholder(id: string): AuditLogPlaceholder {
	return {kind: 'channel', id, recordedName: null, fallback: 'channel'};
}

function readSnapshotChannelId(entry: GuildAuditLogEntryResponse, side: ChangeSide): string | null {
	return readSnowflake(readOption(entry, 'channel_id')) ?? readSnowflake(readChangeSide(entry, 'channel_id', side));
}

function inviteCodePlaceholder(entry: GuildAuditLogEntryResponse, side: ChangeSide): AuditLogPlaceholder {
	const code = readString(entry.target_id) ?? readString(readChangeSide(entry, 'code', side));
	return {kind: 'text', value: code ?? ''};
}

function readInviteSettings(entry: GuildAuditLogEntryResponse, side: ChangeSide): InviteSettings {
	return {
		maxAge: readCount(readOption(entry, 'max_age')) ?? readCount(readChangeSide(entry, 'max_age', side)),
		maxUses: readCount(readOption(entry, 'max_uses')) ?? readCount(readChangeSide(entry, 'max_uses', side)),
		temporary: readBoolean(readOption(entry, 'temporary')) ?? readBoolean(readChangeSide(entry, 'temporary', side)),
	};
}

function inviteSnapshotSummary(
	entry: GuildAuditLogEntryResponse,
	side: ChangeSide,
	inChannel: MessageDescriptor,
	withoutChannel: MessageDescriptor,
): AuditLogSentence {
	const actor = actorPlaceholder(entry);
	const code = inviteCodePlaceholder(entry, side);
	const channelId = readSnapshotChannelId(entry, side);
	return channelId === null
		? {descriptor: withoutChannel, values: {actor, code}}
		: {descriptor: inChannel, values: {actor, code, channel: channelPlaceholder(channelId)}};
}

function inviteMaxAgeRow(maxAge: number): AuditLogDetailRow {
	return maxAge === 0
		? row('max_age', 'neutral', INVITE_NEVER_EXPIRES_ROW)
		: row('max_age', 'neutral', INVITE_EXPIRES_AFTER_ROW, {duration: {kind: 'duration', seconds: maxAge}});
}

function inviteMaxUsesRow(maxUses: number): AuditLogDetailRow {
	return maxUses === 0
		? row('max_uses', 'neutral', INVITE_UNLIMITED_USES_ROW)
		: row('max_uses', 'neutral', INVITE_USE_LIMIT_ROW, {count: maxUses});
}

function inviteTemporaryRow(): AuditLogDetailRow {
	return row('temporary', 'neutral', INVITE_TEMPORARY_MEMBERSHIP_ROW);
}

function inviteCreatorRow(creatorId: string | null, createdAt: number | null): AuditLogDetailRow | null {
	if (creatorId !== null && createdAt !== null) {
		return row('creator', 'neutral', INVITE_CREATED_BY_ON_ROW, {
			user: {kind: 'user', id: creatorId},
			date: {kind: 'date', timestamp: createdAt},
		});
	}
	if (creatorId !== null) {
		return row('creator', 'neutral', INVITE_CREATED_BY_ROW, {user: {kind: 'user', id: creatorId}});
	}
	if (createdAt !== null) {
		return row('creator', 'neutral', INVITE_CREATED_ON_ROW, {date: {kind: 'date', timestamp: createdAt}});
	}
	return null;
}

function inviteUsesRow(uses: number): AuditLogDetailRow {
	return uses === 0
		? row('uses', 'neutral', INVITE_NEVER_USED_ROW)
		: row('uses', 'neutral', INVITE_USE_COUNT_ROW, {count: uses});
}

export function presentInviteCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {maxAge, maxUses, temporary} = readInviteSettings(entry, 'new');
	const rows: Array<AuditLogDetailRow> = [];
	if (maxAge !== null) rows.push(inviteMaxAgeRow(maxAge));
	if (maxUses !== null) rows.push(inviteMaxUsesRow(maxUses));
	if (temporary === true) rows.push(inviteTemporaryRow());
	return {
		summary: inviteSnapshotSummary(entry, 'new', INVITE_CREATE_FOR_CHANNEL_SUMMARY, INVITE_CREATE_SUMMARY),
		rows,
		blocks: [],
	};
}

export function presentInviteUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: {
			descriptor: INVITE_UPDATE_SUMMARY,
			values: {actor: actorPlaceholder(entry), code: inviteCodePlaceholder(entry, 'new')},
		},
		rows: [],
		blocks: [],
	};
}

export function presentInviteDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {maxAge, maxUses, temporary} = readInviteSettings(entry, 'old');
	const inviterId =
		readSnowflake(readOption(entry, 'inviter_id')) ?? readSnowflake(readChangeSide(entry, 'inviter_id', 'old'));
	const creatorId = inviterId !== null && inviterId !== entry.user_id ? inviterId : null;
	const createdAt = readTimestamp(readChangeSide(entry, 'created_at', 'old'));
	const uses = readCount(readChangeSide(entry, 'uses', 'old'));
	const rows: Array<AuditLogDetailRow> = [];
	const creatorRow = inviteCreatorRow(creatorId, createdAt);
	if (creatorRow !== null) rows.push(creatorRow);
	if (uses !== null) rows.push(inviteUsesRow(uses));
	if (maxAge !== null && maxAge > 0 && createdAt !== null) {
		rows.push(
			row('max_age', 'neutral', INVITE_EXPIRES_ON_ROW, {
				date: {kind: 'date', timestamp: createdAt + maxAge * MS_PER_SECOND},
			}),
		);
	} else if (maxAge !== null) {
		rows.push(inviteMaxAgeRow(maxAge));
	}
	if (maxUses !== null) rows.push(inviteMaxUsesRow(maxUses));
	if (temporary === true) rows.push(inviteTemporaryRow());
	return {
		summary: inviteSnapshotSummary(entry, 'old', INVITE_DELETE_FOR_CHANNEL_SUMMARY, INVITE_DELETE_SUMMARY),
		rows,
		blocks: [],
	};
}

function webhookSnapshotSummary(
	entry: GuildAuditLogEntryResponse,
	side: ChangeSide,
	summaries: WebhookSnapshotSummaries,
): AuditLogSentence {
	const actor = actorPlaceholder(entry);
	const name = readString(readChangeSide(entry, 'name', side));
	if (name === null) return {descriptor: summaries.unnamed, values: {actor}};
	const channelId = readSnapshotChannelId(entry, side);
	return channelId === null
		? {descriptor: summaries.named, values: {actor, name: {kind: 'name', value: name}}}
		: {
				descriptor: summaries.inChannel,
				values: {actor, name: {kind: 'name', value: name}, channel: channelPlaceholder(channelId)},
			};
}

function isAvatarHashValue(value: unknown): boolean {
	return value === null || typeof value === 'string';
}

function webhookAvatarRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const change = readChange(entry, 'avatar_hash');
	if (change === null || !isAvatarHashValue(change.oldValue) || !isAvatarHashValue(change.newValue)) return null;
	const previous = readString(change.oldValue);
	const next = readString(change.newValue);
	if (previous === next) return null;
	if (previous === null) return row('avatar_hash', 'add', WEBHOOK_AVATAR_ADDED_ROW);
	if (next === null) return row('avatar_hash', 'remove', WEBHOOK_AVATAR_REMOVED_ROW);
	return row('avatar_hash', 'neutral', WEBHOOK_AVATAR_CHANGED_ROW);
}

function webhookChannelRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const change = readChange(entry, 'channel_id');
	const previous = readSnowflake(change?.oldValue);
	const next = readSnowflake(change?.newValue);
	if (previous === null || next === null || previous === next) return null;
	return row('channel_id', 'neutral', WEBHOOK_CHANNEL_CHANGED_ROW, {
		oldChannel: channelPlaceholder(previous),
		newChannel: channelPlaceholder(next),
	});
}

function readKnownWebhookName(entry: GuildAuditLogEntryResponse, context: AuditLogPresentationContext): string | null {
	const webhookId = readSnowflake(entry.target_id);
	return webhookId === null ? null : readString(context.getWebhookName(webhookId));
}

export function presentWebhookCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: webhookSnapshotSummary(entry, 'new', {
			inChannel: WEBHOOK_CREATE_IN_CHANNEL_SUMMARY,
			named: WEBHOOK_CREATE_SUMMARY,
			unnamed: WEBHOOK_CREATE_UNNAMED_SUMMARY,
		}),
		rows: [],
		blocks: [],
	};
}

export function presentWebhookUpdate(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	const actor = actorPlaceholder(entry);
	const nameChange = readChange(entry, 'name');
	const oldName = readString(nameChange?.oldValue);
	const newName = readString(nameChange?.newValue);
	const rename: Record<'oldName' | 'newName', AuditLogPlaceholder> | null =
		oldName !== null && newName !== null && oldName !== newName
			? {oldName: {kind: 'name', value: oldName}, newName: {kind: 'name', value: newName}}
			: null;
	const channelRow = webhookChannelRow(entry);
	const avatarRow = webhookAvatarRow(entry);
	const rows: Array<AuditLogDetailRow> = [];
	if (rename !== null) rows.push(row('name', 'neutral', WEBHOOK_NAME_CHANGED_ROW, rename));
	if (channelRow !== null) rows.push(channelRow);
	if (avatarRow !== null) rows.push(avatarRow);
	if (rename !== null && rows.length === 1) {
		return {summary: {descriptor: WEBHOOK_RENAME_SUMMARY, values: {actor, ...rename}}, rows: [], blocks: []};
	}
	const name = newName ?? readKnownWebhookName(entry, context);
	return {
		summary:
			name === null
				? {descriptor: WEBHOOK_UPDATE_UNNAMED_SUMMARY, values: {actor}}
				: {descriptor: WEBHOOK_UPDATE_SUMMARY, values: {actor, name: {kind: 'name', value: name}}},
		rows,
		blocks: [],
	};
}

export function presentWebhookDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const creatorId = readSnowflake(readChangeSide(entry, 'creator_id', 'old'));
	return {
		summary: webhookSnapshotSummary(entry, 'old', {
			inChannel: WEBHOOK_DELETE_FROM_CHANNEL_SUMMARY,
			named: WEBHOOK_DELETE_SUMMARY,
			unnamed: WEBHOOK_DELETE_UNNAMED_SUMMARY,
		}),
		rows:
			creatorId !== null && creatorId !== entry.user_id
				? [row('creator', 'neutral', WEBHOOK_CREATED_BY_ROW, {user: {kind: 'user', id: creatorId}})]
				: [],
		blocks: [],
	};
}
