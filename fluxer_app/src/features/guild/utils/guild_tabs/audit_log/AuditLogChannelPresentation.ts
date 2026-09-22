// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CHANNEL_AUDIO_QUALITY_CHANGED_ROW,
	CHANNEL_AUDIO_QUALITY_SET_ROW,
	CHANNEL_CONNECTION_LIMIT_CHANGED_ROW,
	CHANNEL_CONNECTION_LIMIT_SET_ROW,
	CHANNEL_CONTENT_WARNING_ADDED_ROW,
	CHANNEL_CONTENT_WARNING_ADDED_WITH_TEXT_ROW,
	CHANNEL_CONTENT_WARNING_REMOVED_ROW,
	CHANNEL_CONTENT_WARNING_TEXT_CHANGED_ROW,
	CHANNEL_CONTENT_WARNING_TEXT_REMOVED_ROW,
	CHANNEL_CONTENT_WARNING_TEXT_SET_ROW,
	CHANNEL_CREATE_CATEGORY_SUMMARY,
	CHANNEL_CREATE_GENERIC_IN_CATEGORY_SUMMARY,
	CHANNEL_CREATE_GENERIC_SUMMARY,
	CHANNEL_CREATE_LINK_IN_CATEGORY_SUMMARY,
	CHANNEL_CREATE_LINK_SUMMARY,
	CHANNEL_CREATE_TEXT_IN_CATEGORY_SUMMARY,
	CHANNEL_CREATE_TEXT_SUMMARY,
	CHANNEL_CREATE_VOICE_IN_CATEGORY_SUMMARY,
	CHANNEL_CREATE_VOICE_SUMMARY,
	CHANNEL_DELETE_CATEGORY_SUMMARY,
	CHANNEL_DELETE_GENERIC_SUMMARY,
	CHANNEL_DELETE_LINK_SUMMARY,
	CHANNEL_DELETE_TEXT_SUMMARY,
	CHANNEL_DELETE_VOICE_SUMMARY,
	CHANNEL_LINK_CHANGED_ROW,
	CHANNEL_LINK_REMOVED_ROW,
	CHANNEL_LINK_SET_ROW,
	CHANNEL_MARKED_MATURE_ROW,
	CHANNEL_MARKED_NOT_MATURE_ROW,
	CHANNEL_MATURE_FOLLOWS_COMMUNITY_ROW,
	CHANNEL_MOVED_BETWEEN_CATEGORIES_ROW,
	CHANNEL_MOVED_INTO_CATEGORY_ROW,
	CHANNEL_MOVED_OUT_OF_CATEGORY_ROW,
	CHANNEL_NAME_CHANGED_ROW,
	CHANNEL_OVERRIDES_CHANGED_CATEGORY_SUMMARY,
	CHANNEL_OVERRIDES_CHANGED_GENERIC_SUMMARY,
	CHANNEL_OVERRIDES_CHANGED_LINK_SUMMARY,
	CHANNEL_OVERRIDES_CHANGED_TEXT_SUMMARY,
	CHANNEL_OVERRIDES_CHANGED_VOICE_SUMMARY,
	CHANNEL_OVERWRITE_ALLOWED_ROW,
	CHANNEL_OVERWRITE_CREATE_EVERYONE_SUMMARY,
	CHANNEL_OVERWRITE_CREATE_MEMBER_SUMMARY,
	CHANNEL_OVERWRITE_CREATE_ROLE_SUMMARY,
	CHANNEL_OVERWRITE_DELETE_EVERYONE_SUMMARY,
	CHANNEL_OVERWRITE_DELETE_MEMBER_SUMMARY,
	CHANNEL_OVERWRITE_DELETE_ROLE_SUMMARY,
	CHANNEL_OVERWRITE_DENIED_ROW,
	CHANNEL_OVERWRITE_REMOVED_ALLOWED_ROW,
	CHANNEL_OVERWRITE_REMOVED_DENIED_ROW,
	CHANNEL_OVERWRITE_STOPPED_OVERRIDING_ROW,
	CHANNEL_OVERWRITE_UPDATE_EVERYONE_SUMMARY,
	CHANNEL_OVERWRITE_UPDATE_MEMBER_SUMMARY,
	CHANNEL_OVERWRITE_UPDATE_ROLE_SUMMARY,
	CHANNEL_RENAME_CATEGORY_SUMMARY,
	CHANNEL_RENAME_GENERIC_SUMMARY,
	CHANNEL_RENAME_LINK_SUMMARY,
	CHANNEL_RENAME_TEXT_SUMMARY,
	CHANNEL_RENAME_VOICE_SUMMARY,
	CHANNEL_SLOWMODE_CHANGED_ROW,
	CHANNEL_SLOWMODE_SET_ROW,
	CHANNEL_SLOWMODE_TURNED_OFF_ROW,
	CHANNEL_TOPIC_CHANGED_ROW,
	CHANNEL_TOPIC_REMOVED_ROW,
	CHANNEL_TOPIC_SET_ROW,
	CHANNEL_UPDATE_CATEGORY_SUMMARY,
	CHANNEL_UPDATE_GENERIC_SUMMARY,
	CHANNEL_UPDATE_LINK_SUMMARY,
	CHANNEL_UPDATE_TEXT_SUMMARY,
	CHANNEL_UPDATE_VOICE_SUMMARY,
	CHANNEL_USER_LIMIT_CHANGED_ROW,
	CHANNEL_USER_LIMIT_REMOVED_ROW,
	CHANNEL_USER_LIMIT_SET_ROW,
	CHANNEL_VOICE_REGION_AUTOMATIC_ROW,
	CHANNEL_VOICE_REGION_CHANGED_ROW,
	CHANNEL_VOICE_REGION_SET_ROW,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogChannelMessages';
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
	channelNoun,
	diffOverwriteStates,
	permissionFlagsIn,
	readBitfield,
	readBoolean,
	readChange,
	readNumber,
	readOption,
	readSnowflake,
	readString,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {
	VOICE_CHANNEL_BITRATE_DEFAULT,
	VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
} from '@fluxer/constants/src/LimitConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';

type ChannelNoun = ReturnType<typeof channelNoun>;
type OverwriteTarget = 'role' | 'member' | 'everyone';
type Transition<T> = {kind: 'set'; after: T} | {kind: 'changed'; before: T; after: T} | {kind: 'removed'; before: T};

const CONTENT_WARNING_LEVEL_ON = 1;
const MEMBER_OVERWRITE_TYPE = 1;
const BITS_PER_KILOBIT = 1000;

const CREATE_SUMMARIES: Record<Exclude<ChannelNoun, 'category'>, MessageDescriptor> = {
	text: CHANNEL_CREATE_TEXT_SUMMARY,
	voice: CHANNEL_CREATE_VOICE_SUMMARY,
	link: CHANNEL_CREATE_LINK_SUMMARY,
	generic: CHANNEL_CREATE_GENERIC_SUMMARY,
};

const CREATE_IN_CATEGORY_SUMMARIES: Record<Exclude<ChannelNoun, 'category'>, MessageDescriptor> = {
	text: CHANNEL_CREATE_TEXT_IN_CATEGORY_SUMMARY,
	voice: CHANNEL_CREATE_VOICE_IN_CATEGORY_SUMMARY,
	link: CHANNEL_CREATE_LINK_IN_CATEGORY_SUMMARY,
	generic: CHANNEL_CREATE_GENERIC_IN_CATEGORY_SUMMARY,
};

const RENAME_SUMMARIES: Record<ChannelNoun, MessageDescriptor> = {
	text: CHANNEL_RENAME_TEXT_SUMMARY,
	voice: CHANNEL_RENAME_VOICE_SUMMARY,
	link: CHANNEL_RENAME_LINK_SUMMARY,
	category: CHANNEL_RENAME_CATEGORY_SUMMARY,
	generic: CHANNEL_RENAME_GENERIC_SUMMARY,
};

const OVERRIDES_CHANGED_SUMMARIES: Record<ChannelNoun, MessageDescriptor> = {
	text: CHANNEL_OVERRIDES_CHANGED_TEXT_SUMMARY,
	voice: CHANNEL_OVERRIDES_CHANGED_VOICE_SUMMARY,
	link: CHANNEL_OVERRIDES_CHANGED_LINK_SUMMARY,
	category: CHANNEL_OVERRIDES_CHANGED_CATEGORY_SUMMARY,
	generic: CHANNEL_OVERRIDES_CHANGED_GENERIC_SUMMARY,
};

const UPDATE_SUMMARIES: Record<ChannelNoun, MessageDescriptor> = {
	text: CHANNEL_UPDATE_TEXT_SUMMARY,
	voice: CHANNEL_UPDATE_VOICE_SUMMARY,
	link: CHANNEL_UPDATE_LINK_SUMMARY,
	category: CHANNEL_UPDATE_CATEGORY_SUMMARY,
	generic: CHANNEL_UPDATE_GENERIC_SUMMARY,
};

const DELETE_SUMMARIES: Record<ChannelNoun, MessageDescriptor> = {
	text: CHANNEL_DELETE_TEXT_SUMMARY,
	voice: CHANNEL_DELETE_VOICE_SUMMARY,
	link: CHANNEL_DELETE_LINK_SUMMARY,
	category: CHANNEL_DELETE_CATEGORY_SUMMARY,
	generic: CHANNEL_DELETE_GENERIC_SUMMARY,
};

const OVERWRITE_CREATE_SUMMARIES: Record<OverwriteTarget, MessageDescriptor> = {
	role: CHANNEL_OVERWRITE_CREATE_ROLE_SUMMARY,
	member: CHANNEL_OVERWRITE_CREATE_MEMBER_SUMMARY,
	everyone: CHANNEL_OVERWRITE_CREATE_EVERYONE_SUMMARY,
};

const OVERWRITE_UPDATE_SUMMARIES: Record<OverwriteTarget, MessageDescriptor> = {
	role: CHANNEL_OVERWRITE_UPDATE_ROLE_SUMMARY,
	member: CHANNEL_OVERWRITE_UPDATE_MEMBER_SUMMARY,
	everyone: CHANNEL_OVERWRITE_UPDATE_EVERYONE_SUMMARY,
};

const OVERWRITE_DELETE_SUMMARIES: Record<OverwriteTarget, MessageDescriptor> = {
	role: CHANNEL_OVERWRITE_DELETE_ROLE_SUMMARY,
	member: CHANNEL_OVERWRITE_DELETE_MEMBER_SUMMARY,
	everyone: CHANNEL_OVERWRITE_DELETE_EVERYONE_SUMMARY,
};

function row(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: AuditLogSentence['values'] = {},
): AuditLogDetailRow {
	return {id, tone, sentence: {descriptor, values}};
}

function compactRows(candidates: ReadonlyArray<AuditLogDetailRow | null>): Array<AuditLogDetailRow> {
	return candidates.filter((candidate): candidate is AuditLogDetailRow => candidate !== null);
}

function channelPlaceholder(id: string | null, recordedName: string | null, noun: ChannelNoun): AuditLogPlaceholder {
	return {kind: 'channel', id: id ?? '', recordedName, fallback: noun === 'category' ? 'category' : 'channel'};
}

function categoryPlaceholder(id: string): AuditLogPlaceholder {
	return {kind: 'channel', id, recordedName: null, fallback: 'category'};
}

function readTransition<T>(
	entry: GuildAuditLogEntryResponse,
	key: string,
	reader: (value: unknown) => T | null,
): Transition<T> | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const before = reader(change.oldValue);
	const after = reader(change.newValue);
	if (before === null) return after === null ? null : {kind: 'set', after};
	if (after === null) return {kind: 'removed', before};
	return before === after ? null : {kind: 'changed', before, after};
}

function readNsfwOverride(value: unknown): boolean | null | undefined {
	return value === null ? null : (readBoolean(value) ?? undefined);
}

function maturityRow(nsfw: boolean): AuditLogDetailRow {
	return nsfw ? row('nsfw', 'add', CHANNEL_MARKED_MATURE_ROW) : row('nsfw', 'remove', CHANNEL_MARKED_NOT_MATURE_ROW);
}

function addedContentWarningRow(text: string | null): AuditLogDetailRow {
	return text === null
		? row('content_warning', 'add', CHANNEL_CONTENT_WARNING_ADDED_ROW)
		: row('content_warning', 'add', CHANNEL_CONTENT_WARNING_ADDED_WITH_TEXT_ROW, {text: {kind: 'text', value: text}});
}

function createSummary(entry: GuildAuditLogEntryResponse, noun: ChannelNoun): AuditLogSentence {
	const actor = actorPlaceholder(entry);
	const channel = channelPlaceholder(
		readSnowflake(entry.target_id),
		readString(readChange(entry, 'name')?.newValue),
		noun,
	);
	if (noun === 'category') return {descriptor: CHANNEL_CREATE_CATEGORY_SUMMARY, values: {actor, channel}};
	const categoryId = readSnowflake(readChange(entry, 'parent_id')?.newValue);
	if (categoryId === null) return {descriptor: CREATE_SUMMARIES[noun], values: {actor, channel}};
	return {
		descriptor: CREATE_IN_CATEGORY_SUMMARIES[noun],
		values: {actor, channel, category: categoryPlaceholder(categoryId)},
	};
}

function createRows(entry: GuildAuditLogEntryResponse, noun: ChannelNoun): Array<AuditLogDetailRow> {
	const created = (key: string): unknown => readChange(entry, key)?.newValue;
	const isVoice = noun === 'voice';
	const url = noun === 'link' ? readString(created('url')) : null;
	const topic = noun === 'text' || isVoice || noun === 'link' ? readString(created('topic')) : null;
	const nsfw = readBoolean(created('nsfw'));
	const warningLevel = noun === 'category' ? null : readNumber(created('content_warning_level'));
	const slowmode = noun === 'text' || isVoice ? readNumber(created('rate_limit_per_user')) : null;
	const bitrate = isVoice ? readNumber(created('bitrate')) : null;
	const userLimit = isVoice ? readNumber(created('user_limit')) : null;
	const connectionLimit = isVoice ? readNumber(created('voice_connection_limit')) : null;
	const region = isVoice ? readString(created('rtc_region')) : null;
	return compactRows([
		url === null ? null : row('url', 'neutral', CHANNEL_LINK_SET_ROW, {url: {kind: 'text', value: url}}),
		topic === null ? null : row('topic', 'neutral', CHANNEL_TOPIC_SET_ROW, {text: {kind: 'text', value: topic}}),
		nsfw === null ? null : maturityRow(nsfw),
		warningLevel === CONTENT_WARNING_LEVEL_ON
			? addedContentWarningRow(readString(created('content_warning_text')))
			: null,
		slowmode !== null && slowmode > 0
			? row('rate_limit_per_user', 'neutral', CHANNEL_SLOWMODE_SET_ROW, {
					duration: {kind: 'duration', seconds: slowmode},
				})
			: null,
		bitrate !== null && bitrate > 0 && bitrate !== VOICE_CHANNEL_BITRATE_DEFAULT
			? row('bitrate', 'neutral', CHANNEL_AUDIO_QUALITY_SET_ROW, {kbps: bitrate / BITS_PER_KILOBIT})
			: null,
		userLimit !== null && userLimit > 0
			? row('user_limit', 'neutral', CHANNEL_USER_LIMIT_SET_ROW, {count: userLimit})
			: null,
		connectionLimit !== null && connectionLimit !== VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT
			? row('voice_connection_limit', 'neutral', CHANNEL_CONNECTION_LIMIT_SET_ROW, {count: connectionLimit})
			: null,
		region === null
			? null
			: row('rtc_region', 'neutral', CHANNEL_VOICE_REGION_SET_ROW, {region: {kind: 'name', value: region}}),
	]);
}

export function presentChannelCreate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const noun = channelNoun(readNumber(readChange(entry, 'type')?.newValue) ?? readNumber(readOption(entry, 'type')));
	return {summary: createSummary(entry, noun), rows: createRows(entry, noun), blocks: []};
}

function nameRow(name: Transition<string> | null): AuditLogDetailRow | null {
	if (name?.kind !== 'changed') return null;
	return row('name', 'neutral', CHANNEL_NAME_CHANGED_ROW, {
		oldName: {kind: 'name', value: name.before},
		newName: {kind: 'name', value: name.after},
	});
}

function parentRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const parent = readTransition(entry, 'parent_id', readSnowflake);
	if (parent === null) return null;
	switch (parent.kind) {
		case 'set':
			return row('parent_id', 'neutral', CHANNEL_MOVED_INTO_CATEGORY_ROW, {
				category: categoryPlaceholder(parent.after),
			});
		case 'removed':
			return row('parent_id', 'neutral', CHANNEL_MOVED_OUT_OF_CATEGORY_ROW, {
				category: categoryPlaceholder(parent.before),
			});
		case 'changed':
			return row('parent_id', 'neutral', CHANNEL_MOVED_BETWEEN_CATEGORIES_ROW, {
				oldCategory: categoryPlaceholder(parent.before),
				newCategory: categoryPlaceholder(parent.after),
			});
	}
}

function urlRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const url = readTransition(entry, 'url', readString);
	if (url === null) return null;
	switch (url.kind) {
		case 'set':
			return row('url', 'add', CHANNEL_LINK_SET_ROW, {url: {kind: 'text', value: url.after}});
		case 'changed':
			return row('url', 'neutral', CHANNEL_LINK_CHANGED_ROW, {
				oldUrl: {kind: 'text', value: url.before},
				newUrl: {kind: 'text', value: url.after},
			});
		case 'removed':
			return row('url', 'remove', CHANNEL_LINK_REMOVED_ROW);
	}
}

function topicRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const topic = readTransition(entry, 'topic', readString);
	if (topic === null) return null;
	switch (topic.kind) {
		case 'set':
			return row('topic', 'add', CHANNEL_TOPIC_SET_ROW, {text: {kind: 'text', value: topic.after}});
		case 'changed':
			return row('topic', 'neutral', CHANNEL_TOPIC_CHANGED_ROW, {
				oldText: {kind: 'text', value: topic.before},
				newText: {kind: 'text', value: topic.after},
			});
		case 'removed':
			return row('topic', 'remove', CHANNEL_TOPIC_REMOVED_ROW, {text: {kind: 'text', value: topic.before}});
	}
}

function nsfwRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const nsfw = readChange(entry, 'nsfw');
	if (nsfw === null) return null;
	const after = readNsfwOverride(nsfw.newValue);
	if (after === undefined || after === readNsfwOverride(nsfw.oldValue)) return null;
	return after === null ? row('nsfw', 'neutral', CHANNEL_MATURE_FOLLOWS_COMMUNITY_ROW) : maturityRow(after);
}

function contentWarningRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const level = readChange(entry, 'content_warning_level');
	const levelBefore = readNumber(level?.oldValue);
	const levelAfter = readNumber(level?.newValue);
	if (levelAfter === CONTENT_WARNING_LEVEL_ON && levelBefore !== CONTENT_WARNING_LEVEL_ON) {
		return addedContentWarningRow(readString(readChange(entry, 'content_warning_text')?.newValue));
	}
	if (levelBefore === CONTENT_WARNING_LEVEL_ON && levelAfter !== CONTENT_WARNING_LEVEL_ON) {
		return row('content_warning', 'remove', CHANNEL_CONTENT_WARNING_REMOVED_ROW);
	}
	const text = readTransition(entry, 'content_warning_text', readString);
	if (text === null) return null;
	switch (text.kind) {
		case 'set':
			return row('content_warning', 'neutral', CHANNEL_CONTENT_WARNING_TEXT_SET_ROW, {
				text: {kind: 'text', value: text.after},
			});
		case 'changed':
			return row('content_warning', 'neutral', CHANNEL_CONTENT_WARNING_TEXT_CHANGED_ROW, {
				oldText: {kind: 'text', value: text.before},
				newText: {kind: 'text', value: text.after},
			});
		case 'removed':
			return row('content_warning', 'neutral', CHANNEL_CONTENT_WARNING_TEXT_REMOVED_ROW);
	}
}

function slowmodeRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const slowmode = readTransition(entry, 'rate_limit_per_user', readNumber);
	if (slowmode?.kind !== 'changed') return null;
	const {before, after} = slowmode;
	if (before === 0 && after > 0) {
		return row('rate_limit_per_user', 'add', CHANNEL_SLOWMODE_SET_ROW, {duration: {kind: 'duration', seconds: after}});
	}
	if (before > 0 && after === 0) return row('rate_limit_per_user', 'remove', CHANNEL_SLOWMODE_TURNED_OFF_ROW);
	if (before > 0 && after > 0) {
		return row('rate_limit_per_user', 'neutral', CHANNEL_SLOWMODE_CHANGED_ROW, {
			oldDuration: {kind: 'duration', seconds: before},
			newDuration: {kind: 'duration', seconds: after},
		});
	}
	return null;
}

function bitrateRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const bitrate = readTransition(entry, 'bitrate', readNumber);
	if (bitrate?.kind !== 'changed' || bitrate.before <= 0 || bitrate.after <= 0) return null;
	return row('bitrate', 'neutral', CHANNEL_AUDIO_QUALITY_CHANGED_ROW, {
		oldKbps: bitrate.before / BITS_PER_KILOBIT,
		newKbps: bitrate.after / BITS_PER_KILOBIT,
	});
}

function userLimitRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const userLimit = readTransition(entry, 'user_limit', readNumber);
	if (userLimit?.kind !== 'changed') return null;
	const {before, after} = userLimit;
	if (before === 0 && after > 0) return row('user_limit', 'neutral', CHANNEL_USER_LIMIT_SET_ROW, {count: after});
	if (before > 0 && after === 0) return row('user_limit', 'neutral', CHANNEL_USER_LIMIT_REMOVED_ROW);
	if (before > 0 && after > 0) {
		return row('user_limit', 'neutral', CHANNEL_USER_LIMIT_CHANGED_ROW, {oldCount: before, newCount: after});
	}
	return null;
}

function connectionLimitRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const connectionLimit = readTransition(entry, 'voice_connection_limit', readNumber);
	if (connectionLimit?.kind !== 'changed') return null;
	return row('voice_connection_limit', 'neutral', CHANNEL_CONNECTION_LIMIT_CHANGED_ROW, {
		oldCount: connectionLimit.before,
		newCount: connectionLimit.after,
	});
}

function voiceRegionRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const region = readTransition(entry, 'rtc_region', readString);
	if (region === null) return null;
	switch (region.kind) {
		case 'set':
			return row('rtc_region', 'neutral', CHANNEL_VOICE_REGION_SET_ROW, {region: {kind: 'name', value: region.after}});
		case 'changed':
			return row('rtc_region', 'neutral', CHANNEL_VOICE_REGION_CHANGED_ROW, {
				oldRegion: {kind: 'name', value: region.before},
				newRegion: {kind: 'name', value: region.after},
			});
		case 'removed':
			return row('rtc_region', 'neutral', CHANNEL_VOICE_REGION_AUTOMATIC_ROW);
	}
}

export function presentChannelUpdate(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const noun = channelNoun(readNumber(readOption(entry, 'type')));
	const actor = actorPlaceholder(entry);
	const name = readTransition(entry, 'name', readString);
	const rows = compactRows([
		nameRow(name),
		parentRow(entry),
		urlRow(entry),
		topicRow(entry),
		nsfwRow(entry),
		contentWarningRow(entry),
		slowmodeRow(entry),
		bitrateRow(entry),
		userLimitRow(entry),
		connectionLimitRow(entry),
		voiceRegionRow(entry),
	]);
	const overridesChanged = readTransition(entry, 'permission_overwrite_count', readNumber)?.kind === 'changed';
	if (name?.kind === 'changed' && rows.length === 1 && !overridesChanged) {
		return {
			summary: {
				descriptor: RENAME_SUMMARIES[noun],
				values: {actor, oldName: {kind: 'name', value: name.before}, newName: {kind: 'name', value: name.after}},
			},
			rows: [],
			blocks: [],
		};
	}
	const channel = channelPlaceholder(
		readSnowflake(entry.target_id),
		readString(readChange(entry, 'name')?.newValue),
		noun,
	);
	if (rows.length === 0 && overridesChanged) {
		return {summary: {descriptor: OVERRIDES_CHANGED_SUMMARIES[noun], values: {actor, channel}}, rows: [], blocks: []};
	}
	return {summary: {descriptor: UPDATE_SUMMARIES[noun], values: {actor, channel}}, rows, blocks: []};
}

export function presentChannelDelete(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	const noun = channelNoun(readNumber(readOption(entry, 'type')) ?? readNumber(readChange(entry, 'type')?.oldValue));
	const channel = channelPlaceholder(
		readSnowflake(entry.target_id),
		readString(readChange(entry, 'name')?.oldValue),
		noun,
	);
	return {
		summary: {descriptor: DELETE_SUMMARIES[noun], values: {actor: actorPlaceholder(entry), channel}},
		rows: [],
		blocks: [],
	};
}

function overwriteTarget(entry: GuildAuditLogEntryResponse, context: AuditLogPresentationContext): OverwriteTarget {
	const typeChange = readChange(entry, 'type');
	const overwriteType =
		readNumber(readOption(entry, 'type')) ?? readNumber(typeChange?.newValue) ?? readNumber(typeChange?.oldValue);
	if (overwriteType === MEMBER_OVERWRITE_TYPE) return 'member';
	return entry.target_id === context.guildId ? 'everyone' : 'role';
}

function overwriteSummary(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
	summaries: Record<OverwriteTarget, MessageDescriptor>,
): AuditLogSentence {
	const actor = actorPlaceholder(entry);
	const targetId = readSnowflake(entry.target_id) ?? '';
	const channel: AuditLogPlaceholder = {
		kind: 'channel',
		id: readSnowflake(readOption(entry, 'channel_id')) ?? '',
		recordedName: null,
		fallback: 'channel',
	};
	switch (overwriteTarget(entry, context)) {
		case 'role':
			return {
				descriptor: summaries.role,
				values: {
					actor,
					role: {kind: 'role', id: targetId, recordedName: readString(readOption(entry, 'role_name'))},
					channel,
				},
			};
		case 'member':
			return {descriptor: summaries.member, values: {actor, user: {kind: 'user', id: targetId}, channel}};
		case 'everyone':
			return {descriptor: summaries.everyone, values: {actor, channel}};
	}
}

function readOverwriteMask(value: unknown): bigint | null {
	return value === null || value === undefined ? 0n : readBitfield(value);
}

function permissionFlagsInValue(value: unknown): Array<bigint> {
	const mask = readOverwriteMask(value);
	return mask === null ? [] : permissionFlagsIn(mask);
}

function permissionsRow(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	flags: Array<bigint>,
): AuditLogDetailRow | null {
	return flags.length === 0 ? null : row(id, tone, descriptor, {permissions: {kind: 'permissions', flags}});
}

export function presentChannelOverwriteCreate(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	return {
		summary: overwriteSummary(entry, context, OVERWRITE_CREATE_SUMMARIES),
		rows: compactRows([
			permissionsRow(
				'allow',
				'add',
				CHANNEL_OVERWRITE_ALLOWED_ROW,
				permissionFlagsInValue(readChange(entry, 'allow')?.newValue),
			),
			permissionsRow(
				'deny',
				'remove',
				CHANNEL_OVERWRITE_DENIED_ROW,
				permissionFlagsInValue(readChange(entry, 'deny')?.newValue),
			),
		]),
		blocks: [],
	};
}

function overwriteUpdateRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const allow = readChange(entry, 'allow');
	const deny = readChange(entry, 'deny');
	const previousAllow = readOverwriteMask(allow?.oldValue);
	const previousDeny = readOverwriteMask(deny?.oldValue);
	const nextAllow = readOverwriteMask(allow?.newValue);
	const nextDeny = readOverwriteMask(deny?.newValue);
	if (previousAllow === null || previousDeny === null || nextAllow === null || nextDeny === null) return [];
	const {allowed, denied, cleared} = diffOverwriteStates(
		{allow: previousAllow, deny: previousDeny},
		{allow: nextAllow, deny: nextDeny},
	);
	return compactRows([
		permissionsRow('allowed', 'add', CHANNEL_OVERWRITE_ALLOWED_ROW, allowed),
		permissionsRow('denied', 'remove', CHANNEL_OVERWRITE_DENIED_ROW, denied),
		permissionsRow('cleared', 'neutral', CHANNEL_OVERWRITE_STOPPED_OVERRIDING_ROW, cleared),
	]);
}

export function presentChannelOverwriteUpdate(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	return {
		summary: overwriteSummary(entry, context, OVERWRITE_UPDATE_SUMMARIES),
		rows: overwriteUpdateRows(entry),
		blocks: [],
	};
}

export function presentChannelOverwriteDelete(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogDomainResult {
	return {
		summary: overwriteSummary(entry, context, OVERWRITE_DELETE_SUMMARIES),
		rows: compactRows([
			permissionsRow(
				'allow',
				'neutral',
				CHANNEL_OVERWRITE_REMOVED_ALLOWED_ROW,
				permissionFlagsInValue(readChange(entry, 'allow')?.oldValue),
			),
			permissionsRow(
				'deny',
				'neutral',
				CHANNEL_OVERWRITE_REMOVED_DENIED_ROW,
				permissionFlagsInValue(readChange(entry, 'deny')?.oldValue),
			),
		]),
		blocks: [],
	};
}
