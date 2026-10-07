// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AuditLogDetailRow,
	AuditLogTone,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	readChange,
	readNumber,
	readSnowflake,
	readString,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {ChannelFlags, ForumLayoutTypes, ForumSortOrderTypes} from '@fluxer/constants/src/ThreadConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const CHANNEL_CREATE_FORUM_SUMMARY = msg({
	message: '{actor} created the forum channel {channel}',
	comment:
		'Activity log summary when a forum channel was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_FORUM_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the forum channel {channel} in {category}',
	comment:
		'Activity log summary when a forum channel was created inside a category. {actor} is the member who made the change. {channel} is the new channel and {category} the category that holds it, both shown as channel chips.',
});
export const CHANNEL_CREATE_MEDIA_SUMMARY = msg({
	message: '{actor} created the media channel {channel}',
	comment:
		'Activity log summary when a media channel was created. {actor} is the member who made the change, shown as a clickable user chip, or the word System. {channel} is the new channel, shown as a channel chip with its name.',
});
export const CHANNEL_CREATE_MEDIA_IN_CATEGORY_SUMMARY = msg({
	message: '{actor} created the media channel {channel} in {category}',
	comment:
		'Activity log summary when a media channel was created inside a category. {actor} is the member who made the change. {channel} is the new channel and {category} the category that holds it, both shown as channel chips.',
});
export const CHANNEL_RENAME_FORUM_SUMMARY = msg({
	message: '{actor} renamed the forum channel {oldName} to {newName}',
	comment:
		'Activity log summary when the only change to a forum channel was its name. {oldName} and {newName} are shown in bold by the app, so do not add ** or <b>.',
});
export const CHANNEL_RENAME_MEDIA_SUMMARY = msg({
	message: '{actor} renamed the media channel {oldName} to {newName}',
	comment:
		'Activity log summary when the only change to a media channel was its name. {oldName} and {newName} are shown in bold by the app, so do not add ** or <b>.',
});
export const CHANNEL_OVERRIDES_CHANGED_FORUM_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the forum channel {channel}',
	comment:
		'Activity log summary for an entry that only recorded that the permission overrides of a forum channel were changed. {channel} is shown as a channel chip.',
});
export const CHANNEL_OVERRIDES_CHANGED_MEDIA_SUMMARY = msg({
	message: '{actor} changed the permission overrides of the media channel {channel}',
	comment:
		'Activity log summary for an entry that only recorded that the permission overrides of a media channel were changed. {channel} is shown as a channel chip.',
});
export const CHANNEL_UPDATE_FORUM_SUMMARY = msg({
	message: '{actor} updated the forum channel {channel}',
	comment:
		'Activity log summary when settings of a forum channel were changed. The changes are listed as detail rows below it. {channel} is shown as a channel chip.',
});
export const CHANNEL_UPDATE_MEDIA_SUMMARY = msg({
	message: '{actor} updated the media channel {channel}',
	comment:
		'Activity log summary when settings of a media channel were changed. The changes are listed as detail rows below it. {channel} is shown as a channel chip.',
});
export const CHANNEL_DELETE_FORUM_SUMMARY = msg({
	message: '{actor} deleted the forum channel {channel}',
	comment:
		'Activity log summary when a forum channel was deleted. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});
export const CHANNEL_DELETE_MEDIA_SUMMARY = msg({
	message: '{actor} deleted the media channel {channel}',
	comment:
		'Activity log summary when a media channel was deleted. {channel} is the deleted channel, shown as a channel chip with the name it had.',
});
const TAG_ADDED_ROW = msg({
	message: 'Added the tag {name}',
	comment:
		'Activity log detail row under a forum channel that gained a post tag. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {name} is the tag name in bold.',
});
const TAG_REMOVED_ROW = msg({
	message: 'Removed the tag {name}',
	comment:
		'Activity log detail row under a forum channel that lost a post tag. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {name} is the tag name in bold.',
});
const TAG_EDITED_ROW = msg({
	message: 'Edited the tag {name}',
	comment:
		'Activity log detail row under a forum channel whose post tag was renamed or changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {name} is the new tag name in bold.',
});
const DEFAULT_REACTION_SET_ROW = msg({
	message: 'Set the default reaction to {emoji}',
	comment:
		'Activity log detail row under a forum channel whose default post reaction changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {emoji} is the emoji.',
});
const DEFAULT_REACTION_REMOVED_ROW = msg({
	message: 'Removed the default reaction',
	comment:
		'Activity log detail row under a forum channel whose default post reaction was removed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader.',
});
const DEFAULT_SORT_ORDER_ROW = msg({
	message: 'Changed the default sort order to {order}',
	comment:
		'Activity log detail row under a forum channel whose default post order changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {order} is Recently active or Date posted.',
});
const DEFAULT_LAYOUT_ROW = msg({
	message: 'Changed the default layout to {layout}',
	comment:
		'Activity log detail row under a forum channel whose default layout changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {layout} is List or Gallery.',
});
const DEFAULT_AUTO_ARCHIVE_ROW = msg({
	message: 'Changed the default hide after inactivity time to {duration}',
	comment:
		'Activity log detail row under a channel whose default thread or post auto archive time changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {duration} is a formatted duration such as "1 day".',
});
const DEFAULT_THREAD_SLOWMODE_ROW = msg({
	message: 'Changed the default slowmode in threads to {duration}',
	comment:
		'Activity log detail row under a channel whose slowmode copied onto new threads or posts changed. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader. {duration} is a formatted duration such as "30 seconds".',
});
const REQUIRE_TAG_ON_ROW = msg({
	message: 'Required a tag on new posts',
	comment:
		'Activity log detail row under a forum channel that now requires a tag on new posts. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader.',
});
const REQUIRE_TAG_OFF_ROW = msg({
	message: 'Stopped requiring a tag on new posts',
	comment:
		'Activity log detail row under a forum channel that no longer requires a tag on new posts. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader.',
});
const HIDE_MEDIA_DOWNLOAD_ON_ROW = msg({
	message: 'Hid media download options',
	comment:
		'Activity log detail row under a media channel that now hides download options on images and videos. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader.',
});
const HIDE_MEDIA_DOWNLOAD_OFF_ROW = msg({
	message: 'Showed media download options',
	comment:
		'Activity log detail row under a media channel that shows download options on images and videos again. It continues the entry summary, so it has no subject. It is a past tense record of a change the actor in the summary already made, never an instruction to the reader.',
});
const SORT_RECENT_ACTIVITY_LABEL = msg({
	message: 'Recently active',
	comment: 'Forum sort order name shown inside an activity log row.',
});
const SORT_CREATION_DATE_LABEL = msg({
	message: 'Date posted',
	comment: 'Forum sort order name shown inside an activity log row.',
});
const LAYOUT_LIST_LABEL = msg({
	message: 'List',
	comment: 'Forum layout name shown inside an activity log row.',
});
const LAYOUT_GALLERY_LABEL = msg({
	message: 'Gallery',
	comment: 'Forum layout name shown inside an activity log row.',
});

const MINUTE_SECONDS = 60;

interface TagSnapshot {
	id: string;
	name: string;
	key: string;
}

function row(
	id: string,
	tone: AuditLogTone,
	descriptor: MessageDescriptor,
	values: AuditLogDetailRow['sentence']['values'] = {},
): AuditLogDetailRow {
	return {id, tone, sentence: {descriptor, values}};
}

function readTags(value: unknown): Array<TagSnapshot> {
	if (!Array.isArray(value)) return [];
	const tags: Array<TagSnapshot> = [];
	for (const raw of value) {
		if (raw == null || typeof raw !== 'object') continue;
		const record = raw as Record<string, unknown>;
		const id = readSnowflake(record.id);
		const name = readString(record.name);
		if (id == null || name == null) continue;
		tags.push({id, name, key: JSON.stringify([name, record.moderated, record.emoji_id, record.emoji_name])});
	}
	return tags;
}

function tagRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const change = readChange(entry, 'available_tags');
	if (change === null) return [];
	const before = new Map(readTags(change.oldValue).map((tag) => [tag.id, tag]));
	const after = readTags(change.newValue);
	const rows: Array<AuditLogDetailRow> = [];
	for (const tag of after) {
		const previous = before.get(tag.id);
		if (!previous) {
			rows.push(row(`tag-added-${tag.id}`, 'add', TAG_ADDED_ROW, {name: {kind: 'name', value: tag.name}}));
		} else if (previous.key !== tag.key) {
			rows.push(row(`tag-edited-${tag.id}`, 'neutral', TAG_EDITED_ROW, {name: {kind: 'name', value: tag.name}}));
		}
		before.delete(tag.id);
	}
	for (const tag of before.values()) {
		rows.push(row(`tag-removed-${tag.id}`, 'remove', TAG_REMOVED_ROW, {name: {kind: 'name', value: tag.name}}));
	}
	return rows;
}

function readEmoji(value: unknown): {id: string | null; name: string} | null {
	if (value == null || typeof value !== 'object') return null;
	const record = value as Record<string, unknown>;
	const id = readSnowflake(record.emoji_id);
	const name = readString(record.emoji_name);
	if (id == null && name == null) return null;
	return {id, name: name ?? ''};
}

function defaultReactionRow(entry: GuildAuditLogEntryResponse): AuditLogDetailRow | null {
	const change = readChange(entry, 'default_reaction_emoji');
	if (change === null) return null;
	const after = readEmoji(change.newValue);
	if (after == null) {
		return readEmoji(change.oldValue) == null
			? null
			: row('default_reaction_emoji', 'remove', DEFAULT_REACTION_REMOVED_ROW);
	}
	return row('default_reaction_emoji', 'neutral', DEFAULT_REACTION_SET_ROW, {
		emoji: {kind: 'emoji', id: after.id, name: after.name},
	});
}

function changedNumber(entry: GuildAuditLogEntryResponse, key: string): number | null {
	const change = readChange(entry, key);
	if (change === null) return null;
	const after = readNumber(change.newValue);
	return after === null || after === readNumber(change.oldValue) ? null : after;
}

function flagRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const change = readChange(entry, 'flags');
	if (change === null) return [];
	const before = readNumber(change.oldValue) ?? 0;
	const after = readNumber(change.newValue) ?? 0;
	const rows: Array<AuditLogDetailRow> = [];
	const toggled = (flag: number) => (before & flag) !== (after & flag);
	if (toggled(ChannelFlags.REQUIRE_TAG)) {
		rows.push(
			(after & ChannelFlags.REQUIRE_TAG) !== 0
				? row('require_tag', 'add', REQUIRE_TAG_ON_ROW)
				: row('require_tag', 'remove', REQUIRE_TAG_OFF_ROW),
		);
	}
	if (toggled(ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS)) {
		rows.push(
			(after & ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS) !== 0
				? row('hide_media_download', 'add', HIDE_MEDIA_DOWNLOAD_ON_ROW)
				: row('hide_media_download', 'remove', HIDE_MEDIA_DOWNLOAD_OFF_ROW),
		);
	}
	return rows;
}

export function threadParentUpdateRows(entry: GuildAuditLogEntryResponse): Array<AuditLogDetailRow> {
	const rows: Array<AuditLogDetailRow> = [...tagRows(entry)];
	const reaction = defaultReactionRow(entry);
	if (reaction) rows.push(reaction);
	const sortOrder = changedNumber(entry, 'default_sort_order');
	if (sortOrder !== null) {
		rows.push(
			row('default_sort_order', 'neutral', DEFAULT_SORT_ORDER_ROW, {
				order: {
					kind: 'label',
					descriptor:
						sortOrder === ForumSortOrderTypes.CREATION_TIME ? SORT_CREATION_DATE_LABEL : SORT_RECENT_ACTIVITY_LABEL,
				},
			}),
		);
	}
	const layout = changedNumber(entry, 'default_forum_layout');
	if (layout !== null && layout !== ForumLayoutTypes.DEFAULT) {
		rows.push(
			row('default_forum_layout', 'neutral', DEFAULT_LAYOUT_ROW, {
				layout: {
					kind: 'label',
					descriptor: layout === ForumLayoutTypes.GRID ? LAYOUT_GALLERY_LABEL : LAYOUT_LIST_LABEL,
				},
			}),
		);
	}
	const autoArchive = changedNumber(entry, 'default_auto_archive_duration');
	if (autoArchive !== null) {
		rows.push(
			row('default_auto_archive_duration', 'neutral', DEFAULT_AUTO_ARCHIVE_ROW, {
				duration: {kind: 'duration', seconds: autoArchive * MINUTE_SECONDS},
			}),
		);
	}
	const threadSlowmode = changedNumber(entry, 'default_thread_rate_limit_per_user');
	if (threadSlowmode !== null) {
		rows.push(
			row('default_thread_rate_limit_per_user', 'neutral', DEFAULT_THREAD_SLOWMODE_ROW, {
				duration: {kind: 'duration', seconds: threadSlowmode},
			}),
		);
	}
	rows.push(...flagRows(entry));
	return rows;
}
