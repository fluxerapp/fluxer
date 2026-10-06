// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BOT_ADD_SUMMARY,
	BOT_ADD_TEMPORARY_SUMMARY,
	MEMBER_ACCENT_COLOR_REMOVED_ROW,
	MEMBER_ACCENT_COLOR_SET_ROW,
	MEMBER_AVATAR_CHANGED_ROW,
	MEMBER_AVATAR_REMOVED_ROW,
	MEMBER_AVATAR_SET_ROW,
	MEMBER_BAN_ADD_SUMMARY,
	MEMBER_BAN_ADD_TEMPORARY_SUMMARY,
	MEMBER_BAN_DELETED_MESSAGES_ROW,
	MEMBER_BAN_EXPIRY_PASSED_ROW,
	MEMBER_BAN_EXPIRY_PENDING_ROW,
	MEMBER_BAN_ORIGINAL_DATE_ROW,
	MEMBER_BAN_ORIGINAL_MODERATOR_AND_DATE_ROW,
	MEMBER_BAN_ORIGINAL_MODERATOR_ROW,
	MEMBER_BAN_REMOVE_SUMMARY,
	MEMBER_BANNER_CHANGED_ROW,
	MEMBER_BANNER_REMOVED_ROW,
	MEMBER_BANNER_SET_ROW,
	MEMBER_BIO_CHANGED_ROW,
	MEMBER_BIO_REMOVED_ROW,
	MEMBER_BIO_SET_ROW,
	MEMBER_DISCONNECT_SUMMARY,
	MEMBER_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY,
	MEMBER_KICK_SUMMARY,
	MEMBER_MOVE_SUMMARY,
	MEMBER_MOVE_TO_SUMMARY,
	MEMBER_MOVE_UNKNOWN_CHANNEL_SUMMARY,
	MEMBER_NICKNAME_CHANGED_ROW,
	MEMBER_NICKNAME_CHANGED_SUMMARY,
	MEMBER_NICKNAME_REMOVED_ROW,
	MEMBER_NICKNAME_REMOVED_SUMMARY,
	MEMBER_NICKNAME_SET_ROW,
	MEMBER_NICKNAME_SET_SUMMARY,
	MEMBER_NO_LONGER_TEMPORARY_ROW,
	MEMBER_OWN_ACCENT_COLOR_REMOVED_SUMMARY,
	MEMBER_OWN_ACCENT_COLOR_SET_SUMMARY,
	MEMBER_OWN_AVATAR_CHANGED_SUMMARY,
	MEMBER_OWN_AVATAR_REMOVED_SUMMARY,
	MEMBER_OWN_AVATAR_SET_SUMMARY,
	MEMBER_OWN_BANNER_CHANGED_SUMMARY,
	MEMBER_OWN_BANNER_REMOVED_SUMMARY,
	MEMBER_OWN_BANNER_SET_SUMMARY,
	MEMBER_OWN_BIO_CHANGED_SUMMARY,
	MEMBER_OWN_BIO_REMOVED_SUMMARY,
	MEMBER_OWN_BIO_SET_SUMMARY,
	MEMBER_OWN_DISCONNECT_SUMMARY,
	MEMBER_OWN_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY,
	MEMBER_OWN_MOVE_SUMMARY,
	MEMBER_OWN_MOVE_TO_SUMMARY,
	MEMBER_OWN_MOVE_UNKNOWN_CHANNEL_SUMMARY,
	MEMBER_OWN_NICKNAME_CHANGED_SUMMARY,
	MEMBER_OWN_NICKNAME_REMOVED_SUMMARY,
	MEMBER_OWN_NICKNAME_SET_SUMMARY,
	MEMBER_OWN_PROFILE_UPDATED_SUMMARY,
	MEMBER_OWN_PRONOUNS_REMOVED_SUMMARY,
	MEMBER_OWN_PRONOUNS_SET_SUMMARY,
	MEMBER_PRONOUNS_REMOVED_ROW,
	MEMBER_PRONOUNS_SET_ROW,
	MEMBER_PRUNE_SUMMARY,
	MEMBER_ROLE_ADDED_BY_SYSTEM_SUMMARY,
	MEMBER_ROLE_ADDED_ROW,
	MEMBER_ROLE_ADDED_SUMMARY,
	MEMBER_ROLE_REMOVED_BY_SYSTEM_SUMMARY,
	MEMBER_ROLE_REMOVED_ROW,
	MEMBER_ROLE_REMOVED_SUMMARY,
	MEMBER_ROLES_UPDATED_SUMMARY,
	MEMBER_SERVER_DEAFEN_APPLIED_ROW,
	MEMBER_SERVER_DEAFEN_APPLIED_SUMMARY,
	MEMBER_SERVER_DEAFEN_REMOVED_ROW,
	MEMBER_SERVER_DEAFEN_REMOVED_SUMMARY,
	MEMBER_SERVER_MUTE_APPLIED_ROW,
	MEMBER_SERVER_MUTE_APPLIED_SUMMARY,
	MEMBER_SERVER_MUTE_REMOVED_ROW,
	MEMBER_SERVER_MUTE_REMOVED_SUMMARY,
	MEMBER_TIMEOUT_CHANGED_ROW,
	MEMBER_TIMEOUT_CHANGED_SUMMARY,
	MEMBER_TIMEOUT_REMOVED_ROW,
	MEMBER_TIMEOUT_REMOVED_SUMMARY,
	MEMBER_TIMEOUT_SET_ROW,
	MEMBER_TIMEOUT_SET_SUMMARY,
	MEMBER_UPDATED_SUMMARY,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberMessages';
import type {
	AuditLogDetailRow,
	AuditLogDomainResult,
	AuditLogPlaceholder,
	AuditLogSentence,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	actorPlaceholder,
	decodeAuditReason,
	entryTime,
	readBoolean,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
	readStringArray,
	readTimestamp,
	roleIdDiff,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {MS_PER_SECOND, SECONDS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

const MAX_COLOR_VALUE = 0xffffff;

type SentenceValues = AuditLogSentence['values'];

type ValueChangeKind = 'set' | 'changed' | 'removed';

type ValueChange<T> = {kind: 'set'; next: T} | {kind: 'changed'; previous: T; next: T} | {kind: 'removed'; previous: T};

type TimeoutChange = {kind: 'set' | 'changed'; until: number} | {kind: 'removed'};

interface MemberSubject {
	actor: AuditLogPlaceholder;
	target: AuditLogPlaceholder;
	self: boolean;
}

interface RoleChanges {
	added: Array<string>;
	removed: Array<string>;
}

interface SingleRoleChange {
	kind: 'added' | 'removed';
	roleId: string;
}

interface MemberChangeGroup {
	summary: AuditLogSentence | null;
	summaryRows: Array<AuditLogDetailRow>;
	rows: Array<AuditLogDetailRow>;
}

interface VoiceStateMessages {
	appliedSummary: MessageDescriptor;
	removedSummary: MessageDescriptor;
	appliedRow: MessageDescriptor;
	removedRow: MessageDescriptor;
}

interface ProfileGroupSpec<T extends string | number> {
	key: string;
	read: (value: unknown) => T | null;
	values: (next: T) => SentenceValues;
	tones: Record<ValueChangeKind, AuditLogTone>;
	summaries: Record<ValueChangeKind, MessageDescriptor>;
	rows: Record<ValueChangeKind, MessageDescriptor>;
}

const SERVER_MUTE_MESSAGES: VoiceStateMessages = {
	appliedSummary: MEMBER_SERVER_MUTE_APPLIED_SUMMARY,
	removedSummary: MEMBER_SERVER_MUTE_REMOVED_SUMMARY,
	appliedRow: MEMBER_SERVER_MUTE_APPLIED_ROW,
	removedRow: MEMBER_SERVER_MUTE_REMOVED_ROW,
};

const SERVER_DEAFEN_MESSAGES: VoiceStateMessages = {
	appliedSummary: MEMBER_SERVER_DEAFEN_APPLIED_SUMMARY,
	removedSummary: MEMBER_SERVER_DEAFEN_REMOVED_SUMMARY,
	appliedRow: MEMBER_SERVER_DEAFEN_APPLIED_ROW,
	removedRow: MEMBER_SERVER_DEAFEN_REMOVED_ROW,
};

const AVATAR_GROUP: ProfileGroupSpec<string> = {
	key: 'avatar_hash',
	read: readString,
	values: () => ({}),
	tones: {set: 'add', changed: 'neutral', removed: 'remove'},
	summaries: {
		set: MEMBER_OWN_AVATAR_SET_SUMMARY,
		changed: MEMBER_OWN_AVATAR_CHANGED_SUMMARY,
		removed: MEMBER_OWN_AVATAR_REMOVED_SUMMARY,
	},
	rows: {set: MEMBER_AVATAR_SET_ROW, changed: MEMBER_AVATAR_CHANGED_ROW, removed: MEMBER_AVATAR_REMOVED_ROW},
};

const BANNER_GROUP: ProfileGroupSpec<string> = {
	key: 'banner_hash',
	read: readString,
	values: () => ({}),
	tones: {set: 'add', changed: 'neutral', removed: 'remove'},
	summaries: {
		set: MEMBER_OWN_BANNER_SET_SUMMARY,
		changed: MEMBER_OWN_BANNER_CHANGED_SUMMARY,
		removed: MEMBER_OWN_BANNER_REMOVED_SUMMARY,
	},
	rows: {set: MEMBER_BANNER_SET_ROW, changed: MEMBER_BANNER_CHANGED_ROW, removed: MEMBER_BANNER_REMOVED_ROW},
};

const BIO_GROUP: ProfileGroupSpec<string> = {
	key: 'bio',
	read: readString,
	values: (text) => ({text: {kind: 'text', value: text}}),
	tones: {set: 'add', changed: 'neutral', removed: 'remove'},
	summaries: {
		set: MEMBER_OWN_BIO_SET_SUMMARY,
		changed: MEMBER_OWN_BIO_CHANGED_SUMMARY,
		removed: MEMBER_OWN_BIO_REMOVED_SUMMARY,
	},
	rows: {set: MEMBER_BIO_SET_ROW, changed: MEMBER_BIO_CHANGED_ROW, removed: MEMBER_BIO_REMOVED_ROW},
};

const PRONOUNS_GROUP: ProfileGroupSpec<string> = {
	key: 'pronouns',
	read: readString,
	values: (text) => ({text: {kind: 'text', value: text}}),
	tones: {set: 'neutral', changed: 'neutral', removed: 'remove'},
	summaries: {
		set: MEMBER_OWN_PRONOUNS_SET_SUMMARY,
		changed: MEMBER_OWN_PRONOUNS_SET_SUMMARY,
		removed: MEMBER_OWN_PRONOUNS_REMOVED_SUMMARY,
	},
	rows: {set: MEMBER_PRONOUNS_SET_ROW, changed: MEMBER_PRONOUNS_SET_ROW, removed: MEMBER_PRONOUNS_REMOVED_ROW},
};

const ACCENT_COLOR_GROUP: ProfileGroupSpec<number> = {
	key: 'accent_color',
	read: readColor,
	values: (color) => ({color: {kind: 'color', value: color}}),
	tones: {set: 'neutral', changed: 'neutral', removed: 'remove'},
	summaries: {
		set: MEMBER_OWN_ACCENT_COLOR_SET_SUMMARY,
		changed: MEMBER_OWN_ACCENT_COLOR_SET_SUMMARY,
		removed: MEMBER_OWN_ACCENT_COLOR_REMOVED_SUMMARY,
	},
	rows: {
		set: MEMBER_ACCENT_COLOR_SET_ROW,
		changed: MEMBER_ACCENT_COLOR_SET_ROW,
		removed: MEMBER_ACCENT_COLOR_REMOVED_ROW,
	},
};

function sentence(descriptor: MessageDescriptor, values: SentenceValues): AuditLogSentence {
	return {descriptor, values};
}

function detailRow(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: SentenceValues = {},
): AuditLogDetailRow {
	return {id, tone, sentence: {descriptor, values}};
}

function summaryOnly(summary: AuditLogSentence): AuditLogDomainResult {
	return {summary, rows: [], blocks: []};
}

function userPlaceholder(id: string): AuditLogPlaceholder {
	return {kind: 'user', id};
}

function channelPlaceholder(id: string): AuditLogPlaceholder {
	return {kind: 'channel', id, recordedName: null, fallback: 'channel'};
}

function rolePlaceholder(id: string, recordedName: string | null): AuditLogPlaceholder {
	return {kind: 'role', id, recordedName};
}

function datePlaceholder(timestamp: number): AuditLogPlaceholder {
	return {kind: 'date', timestamp};
}

function namePlaceholder(value: string): AuditLogPlaceholder {
	return {kind: 'name', value};
}

function readColor(value: unknown): number | null {
	const color = readNumber(value);
	return color !== null && Number.isInteger(color) && color >= 0 && color <= MAX_COLOR_VALUE ? color : null;
}

function readMemberSubject(entry: GuildAuditLogEntryResponse): MemberSubject {
	const actorId = readSnowflake(entry.user_id);
	const targetId = readSnowflake(entry.target_id);
	return {
		actor: actorPlaceholder(entry),
		target: userPlaceholder(targetId ?? ''),
		self: actorId !== null && actorId === targetId,
	};
}

function readValueChange<T extends string | number>(
	entry: GuildAuditLogEntryResponse,
	key: string,
	read: (value: unknown) => T | null,
): ValueChange<T> | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const previous = read(change.oldValue);
	const next = read(change.newValue);
	if (previous === next) return null;
	if (previous === null) return next === null ? null : {kind: 'set', next};
	if (next === null) return {kind: 'removed', previous};
	return {kind: 'changed', previous, next};
}

function readToggle(entry: GuildAuditLogEntryResponse, key: string): boolean | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const previous = readBoolean(change.oldValue);
	const next = readBoolean(change.newValue);
	return previous === null || next === null || previous === next ? null : next;
}

function readTimeoutChange(entry: GuildAuditLogEntryResponse): TimeoutChange | null {
	const change = readChange(entry, 'communication_disabled_until');
	if (change === null) return null;
	const time = entryTime(entry);
	const previous = readTimestamp(change.oldValue);
	const next = readTimestamp(change.newValue);
	const wasActive = previous !== null && previous > time;
	if (next !== null && next > time) {
		if (!wasActive) return {kind: 'set', until: next};
		return previous === next ? null : {kind: 'changed', until: next};
	}
	return wasActive ? {kind: 'removed'} : null;
}

function readRoleChanges(entry: GuildAuditLogEntryResponse): RoleChanges {
	const change = readChange(entry, 'roles');
	const previous = readStringArray(change?.oldValue);
	const next = readStringArray(change?.newValue);
	if (previous === null || next === null) return {added: [], removed: []};
	const {added, removed} = roleIdDiff(previous, next);
	const isRoleId = (id: string) => readSnowflake(id) !== null;
	return {added: added.filter(isRoleId), removed: removed.filter(isRoleId)};
}

function readSingleRoleChange(changes: RoleChanges): SingleRoleChange | null {
	const [addedId] = changes.added;
	const [removedId] = changes.removed;
	if (addedId !== undefined && changes.added.length === 1 && changes.removed.length === 0) {
		return {kind: 'added', roleId: addedId};
	}
	if (removedId !== undefined && changes.removed.length === 1 && changes.added.length === 0) {
		return {kind: 'removed', roleId: removedId};
	}
	return null;
}

function roleRows(changes: RoleChanges): Array<AuditLogDetailRow> {
	return [
		...changes.added.map((id) =>
			detailRow(`role_added_${id}`, 'add', MEMBER_ROLE_ADDED_ROW, {role: rolePlaceholder(id, null)}),
		),
		...changes.removed.map((id) =>
			detailRow(`role_removed_${id}`, 'remove', MEMBER_ROLE_REMOVED_ROW, {role: rolePlaceholder(id, null)}),
		),
	];
}

function temporaryRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const change = readChange(entry, 'temporary');
	if (change === null || readBoolean(change.oldValue) !== true || readBoolean(change.newValue) !== false) return [];
	return [detailRow('temporary', 'neutral', MEMBER_NO_LONGER_TEMPORARY_ROW)];
}

function roleChangeSummary(
	subject: MemberSubject,
	change: SingleRoleChange,
	role: AuditLogPlaceholder,
): AuditLogSentence {
	const {actor, target} = subject;
	return change.kind === 'added'
		? sentence(MEMBER_ROLE_ADDED_SUMMARY, {actor, target, role})
		: sentence(MEMBER_ROLE_REMOVED_SUMMARY, {actor, target, role});
}

function nicknameGroup(entry: GuildAuditLogEntryResponse, subject: MemberSubject): MemberChangeGroup | null {
	const change = readValueChange(entry, 'nick', readString);
	if (change === null) return null;
	const {actor, target, self} = subject;
	switch (change.kind) {
		case 'set': {
			const nick = namePlaceholder(change.next);
			return {
				summary: self
					? sentence(MEMBER_OWN_NICKNAME_SET_SUMMARY, {actor, nick})
					: sentence(MEMBER_NICKNAME_SET_SUMMARY, {actor, target, nick}),
				summaryRows: [],
				rows: [detailRow('nick', 'add', MEMBER_NICKNAME_SET_ROW, {nick})],
			};
		}
		case 'changed': {
			const oldNick = namePlaceholder(change.previous);
			const newNick = namePlaceholder(change.next);
			return {
				summary: self
					? sentence(MEMBER_OWN_NICKNAME_CHANGED_SUMMARY, {actor, oldNick, newNick})
					: sentence(MEMBER_NICKNAME_CHANGED_SUMMARY, {actor, target, oldNick, newNick}),
				summaryRows: [],
				rows: [detailRow('nick', 'neutral', MEMBER_NICKNAME_CHANGED_ROW, {oldNick, newNick})],
			};
		}
		case 'removed': {
			const nick = namePlaceholder(change.previous);
			return {
				summary: self
					? sentence(MEMBER_OWN_NICKNAME_REMOVED_SUMMARY, {actor, nick})
					: sentence(MEMBER_NICKNAME_REMOVED_SUMMARY, {actor, target, nick}),
				summaryRows: [],
				rows: [detailRow('nick', 'remove', MEMBER_NICKNAME_REMOVED_ROW, {nick})],
			};
		}
	}
}

function rolesGroup(entry: GuildAuditLogEntryResponse, subject: MemberSubject): MemberChangeGroup | null {
	const changes = readRoleChanges(entry);
	const rows = roleRows(changes);
	if (rows.length === 0) return null;
	const single = readSingleRoleChange(changes);
	if (single !== null) {
		return {
			summary: roleChangeSummary(subject, single, rolePlaceholder(single.roleId, null)),
			summaryRows: [],
			rows,
		};
	}
	const {actor, target} = subject;
	return {summary: sentence(MEMBER_ROLES_UPDATED_SUMMARY, {actor, target}), summaryRows: rows, rows};
}

function timeoutGroup(entry: GuildAuditLogEntryResponse, subject: MemberSubject): MemberChangeGroup | null {
	const change = readTimeoutChange(entry);
	if (change === null) return null;
	const {actor, target} = subject;
	switch (change.kind) {
		case 'set': {
			const date = datePlaceholder(change.until);
			return {
				summary: sentence(MEMBER_TIMEOUT_SET_SUMMARY, {actor, target, date}),
				summaryRows: [],
				rows: [detailRow('communication_disabled_until', 'add', MEMBER_TIMEOUT_SET_ROW, {date})],
			};
		}
		case 'changed': {
			const date = datePlaceholder(change.until);
			return {
				summary: sentence(MEMBER_TIMEOUT_CHANGED_SUMMARY, {actor, target, date}),
				summaryRows: [],
				rows: [detailRow('communication_disabled_until', 'neutral', MEMBER_TIMEOUT_CHANGED_ROW, {date})],
			};
		}
		case 'removed':
			return {
				summary: sentence(MEMBER_TIMEOUT_REMOVED_SUMMARY, {actor, target}),
				summaryRows: [],
				rows: [detailRow('communication_disabled_until', 'remove', MEMBER_TIMEOUT_REMOVED_ROW)],
			};
	}
}

function voiceStateGroup(
	entry: GuildAuditLogEntryResponse,
	subject: MemberSubject,
	key: string,
	messages: VoiceStateMessages,
): MemberChangeGroup | null {
	const applied = readToggle(entry, key);
	if (applied === null) return null;
	const {actor, target} = subject;
	return {
		summary: sentence(applied ? messages.appliedSummary : messages.removedSummary, {actor, target}),
		summaryRows: [],
		rows: [applied ? detailRow(key, 'add', messages.appliedRow) : detailRow(key, 'remove', messages.removedRow)],
	};
}

function profileGroup<T extends string | number>(
	entry: GuildAuditLogEntryResponse,
	subject: MemberSubject,
	spec: ProfileGroupSpec<T>,
): MemberChangeGroup | null {
	const change = readValueChange(entry, spec.key, spec.read);
	if (change === null) return null;
	const values = change.kind === 'removed' ? {} : spec.values(change.next);
	return {
		summary: subject.self ? sentence(spec.summaries[change.kind], {actor: subject.actor, ...values}) : null,
		summaryRows: [],
		rows: [detailRow(spec.key, spec.tones[change.kind], spec.rows[change.kind], values)],
	};
}

function readBanDurationSeconds(entry: GuildAuditLogEntryResponse): number | null {
	const expiresAt = readTimestamp(readChange(entry, 'expires_at')?.newValue);
	if (expiresAt === null) return null;
	const bannedAt = readTimestamp(readChange(entry, 'banned_at')?.newValue) ?? entryTime(entry);
	const seconds = Math.round((expiresAt - bannedAt) / MS_PER_SECOND);
	return seconds > 0 ? seconds : null;
}

function readDeletedMessageSeconds(entry: GuildAuditLogEntryResponse): number | null {
	const seconds = readNumber(readOption(entry, 'delete_message_seconds'));
	if (seconds !== null && seconds > 0) return seconds;
	const days = readNumber(readOption(entry, 'delete_member_days'));
	return days !== null && Number.isInteger(days) && days > 0 ? days * SECONDS_PER_DAY : null;
}

function originalBanRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const moderatorId = readSnowflake(readChange(entry, 'moderator_id')?.oldValue);
	const bannedAt = readTimestamp(readChange(entry, 'banned_at')?.oldValue);
	const otherModeratorId = moderatorId !== null && moderatorId !== entry.user_id ? moderatorId : null;
	if (otherModeratorId !== null && bannedAt !== null) {
		return [
			detailRow('original_ban', 'neutral', MEMBER_BAN_ORIGINAL_MODERATOR_AND_DATE_ROW, {
				user: userPlaceholder(otherModeratorId),
				date: datePlaceholder(bannedAt),
			}),
		];
	}
	if (bannedAt !== null) {
		return [detailRow('original_ban', 'neutral', MEMBER_BAN_ORIGINAL_DATE_ROW, {date: datePlaceholder(bannedAt)})];
	}
	if (otherModeratorId !== null) {
		return [
			detailRow('original_ban', 'neutral', MEMBER_BAN_ORIGINAL_MODERATOR_ROW, {
				user: userPlaceholder(otherModeratorId),
			}),
		];
	}
	return [];
}

function banExpiryRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const expiresAt = readTimestamp(readChange(entry, 'expires_at')?.oldValue);
	if (expiresAt === null) return [];
	const descriptor = expiresAt > entryTime(entry) ? MEMBER_BAN_EXPIRY_PENDING_ROW : MEMBER_BAN_EXPIRY_PASSED_ROW;
	return [detailRow('ban_expiry', 'neutral', descriptor, {date: datePlaceholder(expiresAt)})];
}

export function presentMemberKick(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target} = readMemberSubject(entry);
	return summaryOnly(sentence(MEMBER_KICK_SUMMARY, {actor, target}));
}

export function presentMemberPrune(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return summaryOnly(sentence(MEMBER_PRUNE_SUMMARY, {actor: actorPlaceholder(entry)}));
}

export function presentMemberBanAdd(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target} = readMemberSubject(entry);
	const durationSeconds = readBanDurationSeconds(entry);
	const deletedSeconds = readDeletedMessageSeconds(entry);
	const banReason = decodeAuditReason(readChange(entry, 'reason')?.newValue);
	return {
		summary:
			durationSeconds === null
				? sentence(MEMBER_BAN_ADD_SUMMARY, {actor, target})
				: sentence(MEMBER_BAN_ADD_TEMPORARY_SUMMARY, {
						actor,
						target,
						duration: {kind: 'duration', seconds: durationSeconds},
					}),
		rows:
			deletedSeconds === null
				? []
				: [
						detailRow('delete_messages', 'remove', MEMBER_BAN_DELETED_MESSAGES_ROW, {
							duration: {kind: 'duration', seconds: deletedSeconds},
						}),
					],
		blocks:
			banReason === null || banReason === decodeAuditReason(entry.reason)
				? []
				: [{kind: 'ban_reason', text: banReason}],
	};
}

export function presentMemberBanRemove(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target} = readMemberSubject(entry);
	const banReason = decodeAuditReason(readChange(entry, 'reason')?.oldValue);
	return {
		summary: sentence(MEMBER_BAN_REMOVE_SUMMARY, {actor, target}),
		rows: [...originalBanRows(entry), ...banExpiryRows(entry)],
		blocks: banReason === null ? [] : [{kind: 'ban_reason', text: banReason}],
	};
}

export function presentMemberUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const subject = readMemberSubject(entry);
	const groups = [
		nicknameGroup(entry, subject),
		rolesGroup(entry, subject),
		timeoutGroup(entry, subject),
		voiceStateGroup(entry, subject, 'mute', SERVER_MUTE_MESSAGES),
		voiceStateGroup(entry, subject, 'deaf', SERVER_DEAFEN_MESSAGES),
		profileGroup(entry, subject, AVATAR_GROUP),
		profileGroup(entry, subject, BANNER_GROUP),
		profileGroup(entry, subject, BIO_GROUP),
		profileGroup(entry, subject, PRONOUNS_GROUP),
		profileGroup(entry, subject, ACCENT_COLOR_GROUP),
	].filter((group): group is MemberChangeGroup => group !== null);
	const temporary = temporaryRows(entry);
	const [onlyGroup] = groups;
	if (groups.length === 1 && onlyGroup?.summary) {
		return {summary: onlyGroup.summary, rows: [...onlyGroup.summaryRows, ...temporary], blocks: []};
	}
	const {actor, target, self} = subject;
	return {
		summary: self
			? sentence(MEMBER_OWN_PROFILE_UPDATED_SUMMARY, {actor})
			: sentence(MEMBER_UPDATED_SUMMARY, {actor, target}),
		rows: [...groups.flatMap((group) => group.rows), ...temporary],
		blocks: [],
	};
}

export function presentMemberRoleUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const subject = readMemberSubject(entry);
	const changes = readRoleChanges(entry);
	const single = readSingleRoleChange(changes);
	const temporary = temporaryRows(entry);
	if (single === null) {
		const {actor, target} = subject;
		return {
			summary: sentence(MEMBER_ROLES_UPDATED_SUMMARY, {actor, target}),
			rows: [...roleRows(changes), ...temporary],
			blocks: [],
		};
	}
	const role = rolePlaceholder(single.roleId, readString(readOption(entry, 'role_name')));
	const {target} = subject;
	const summary =
		subject.actor.kind !== 'system'
			? roleChangeSummary(subject, single, role)
			: single.kind === 'added'
				? sentence(MEMBER_ROLE_ADDED_BY_SYSTEM_SUMMARY, {target, role})
				: sentence(MEMBER_ROLE_REMOVED_BY_SYSTEM_SUMMARY, {target, role});
	return {summary, rows: temporary, blocks: []};
}

export function presentMemberMove(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target, self} = readMemberSubject(entry);
	const change = readChange(entry, 'channel_id');
	const oldChannelId = readSnowflake(change?.oldValue);
	const newChannelId = readSnowflake(change?.newValue) ?? readSnowflake(readOption(entry, 'channel_id'));
	if (newChannelId === null) {
		return summaryOnly(
			self
				? sentence(MEMBER_OWN_MOVE_UNKNOWN_CHANNEL_SUMMARY, {actor})
				: sentence(MEMBER_MOVE_UNKNOWN_CHANNEL_SUMMARY, {actor, target}),
		);
	}
	const newChannel = channelPlaceholder(newChannelId);
	if (oldChannelId === null) {
		return summaryOnly(
			self
				? sentence(MEMBER_OWN_MOVE_TO_SUMMARY, {actor, channel: newChannel})
				: sentence(MEMBER_MOVE_TO_SUMMARY, {actor, target, channel: newChannel}),
		);
	}
	const oldChannel = channelPlaceholder(oldChannelId);
	return summaryOnly(
		self
			? sentence(MEMBER_OWN_MOVE_SUMMARY, {actor, oldChannel, newChannel})
			: sentence(MEMBER_MOVE_SUMMARY, {actor, target, oldChannel, newChannel}),
	);
}

export function presentMemberDisconnect(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target, self} = readMemberSubject(entry);
	const channelId =
		readSnowflake(readChange(entry, 'channel_id')?.oldValue) ?? readSnowflake(readOption(entry, 'channel_id'));
	if (channelId === null) {
		return summaryOnly(
			self
				? sentence(MEMBER_OWN_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY, {actor})
				: sentence(MEMBER_DISCONNECT_UNKNOWN_CHANNEL_SUMMARY, {actor, target}),
		);
	}
	const channel = channelPlaceholder(channelId);
	return summaryOnly(
		self
			? sentence(MEMBER_OWN_DISCONNECT_SUMMARY, {actor, channel})
			: sentence(MEMBER_DISCONNECT_SUMMARY, {actor, target, channel}),
	);
}

export function presentBotAdd(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const {actor, target} = readMemberSubject(entry);
	const temporary = readBoolean(readOption(entry, 'temporary')) === true;
	return summaryOnly(sentence(temporary ? BOT_ADD_TEMPORARY_SUMMARY : BOT_ADD_SUMMARY, {actor, target}));
}
