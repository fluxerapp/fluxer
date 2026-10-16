// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {TimestampStyle} from '@app/features/messaging/utils/markdown/parser/Enums';
import Users from '@app/features/user/state/Users';
import {shouldUse12HourFormat} from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {formatTimestampWithStyle} from '@fluxer/date_utils/src/DateTimestampStyle';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'unknown user',
	comment:
		'TTS substitution token spoken in place of a mention when the user cannot be resolved. Lowercase, mid-sentence.',
});
const UNKNOWN_ROLE_DESCRIPTOR = msg({
	message: 'unknown role',
	comment:
		'TTS substitution token spoken in place of a role mention when the role cannot be resolved. Lowercase, mid-sentence.',
});
const UNKNOWN_CHANNEL_DESCRIPTOR = msg({
	message: 'unknown channel',
	comment:
		'TTS substitution token spoken in place of a channel mention when the channel cannot be resolved. Lowercase, mid-sentence.',
});
const CODE_BLOCK_DESCRIPTOR = msg({
	message: 'code block',
	comment: 'TTS substitution token spoken in place of a fenced code block. Lowercase, mid-sentence.',
});
const SPOILER_DESCRIPTOR = msg({
	message: 'spoiler',
	comment: 'TTS substitution token spoken in place of spoiler-tagged content. Lowercase, mid-sentence.',
});
const EMOJI_DESCRIPTOR = msg({
	message: 'emoji {emojiName}',
	comment:
		'TTS substitution token spoken in place of a custom emoji. {emojiName} is the emoji shortcode without colons. Lowercase, mid-sentence.',
});
const SLASH_DESCRIPTOR = msg({
	message: 'slash {commandName}',
	comment:
		'TTS substitution token spoken in place of a slash-command invocation. {commandName} is the command name without the slash.',
});
const REPLYING_TO_SAID_DESCRIPTOR = msg({
	message: 'Replying to {replyAuthorName}, {authorName} said: {formatted}',
	comment:
		'TTS sentence spoken for a reply message. {replyAuthorName} is the original author, {authorName} is the replier, {formatted} is the speakable message body.',
});
const SAID_DESCRIPTOR = msg({
	message: '{authorName} said: {formatted}',
	comment:
		'TTS sentence spoken for a normal message. {authorName} is the author display name, {formatted} is the speakable message body.',
});
const TTS_STYLE_MAP: Record<string, TimestampStyle> = {
	t: TimestampStyle.ShortTime,
	T: TimestampStyle.LongTime,
	d: TimestampStyle.ShortDate,
	D: TimestampStyle.LongDate,
	f: TimestampStyle.ShortDateTime,
	F: TimestampStyle.LongDateTime,
};
const USER_MENTION_PATTERN = /<@!?(\d+)>/g;
const ROLE_MENTION_PATTERN = /<@&(\d+)>/g;
const CHANNEL_MENTION_PATTERN = /<#(\d+)>/g;
const CUSTOM_EMOJI_PATTERN = /<a?:([^:]+):\d+>/g;
const SPOILER_PATTERN = /\|\|[^|]+\|\|/g;
const SLASH_COMMAND_PATTERN = /<\/([^:]+):\d+>/g;
const TIMESTAMP_PATTERN = /<t:(\d+)(?::([tTdDfFR]))?>/g;
const CODE_BLOCK_PATTERN = /```[\s\S]*?```/g;
const INLINE_CODE_PATTERN = /`([^`\n]+)`/g;
const MASKED_LINK_PATTERN = /\[([^\]]+)\]\([^)]+\)/g;
const BOLD_ITALIC_PATTERN = /\*{3}(.+?)\*{3}/g;
const BOLD_PATTERN = /\*{2}(.+?)\*{2}/g;
const UNDERLINE_PATTERN = /__(.+?)__/g;
const STRIKETHROUGH_PATTERN = /~~(.+?)~~/g;
const ITALIC_PATTERN = /\*(.+?)\*/g;
const BLOCKQUOTE_PATTERN = /^>\s?/gm;
const HEADER_PATTERN = /^(?:-#|#{1,3})\s+/gm;

function formatUserMention(userId: string, guildId: string | null, i18n: I18n): string {
	const user = Users.getUser(userId);
	if (!user) {
		return i18n._(UNKNOWN_USER_DESCRIPTOR);
	}
	return NicknameUtils.getNickname(user, guildId ?? null);
}

function formatRoleMention(roleId: string, guildId: string | null, i18n: I18n): string {
	if (!guildId) {
		return i18n._(UNKNOWN_ROLE_DESCRIPTOR);
	}
	const guild = Guilds.getGuild(guildId);
	if (!guild) {
		return i18n._(UNKNOWN_ROLE_DESCRIPTOR);
	}
	const role = guild.roles[roleId];
	return role?.name ?? i18n._(UNKNOWN_ROLE_DESCRIPTOR);
}

function formatChannelMention(channelId: string, i18n: I18n): string {
	const channel = Channels.getChannel(channelId);
	return channel?.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR);
}

function formatTimestampForTts(timestamp: number, style: string | undefined): string {
	const locale = getCurrentLocale();
	const hour12 = shouldUse12HourFormat(locale);
	const resolvedStyle = (style ? TTS_STYLE_MAP[style] : undefined) ?? TimestampStyle.ShortDateTime;
	return formatTimestampWithStyle(timestamp, resolvedStyle, locale, hour12);
}

export function formatMessageForTts(
	content: string,
	authorName: string,
	guildId: string | null,
	i18n: I18n,
	replyAuthorName?: string | null,
): string {
	let formatted = content;
	formatted = formatted.replace(CODE_BLOCK_PATTERN, ` ${i18n._(CODE_BLOCK_DESCRIPTOR)} `);
	formatted = formatted.replace(SPOILER_PATTERN, i18n._(SPOILER_DESCRIPTOR));
	formatted = formatted.replace(USER_MENTION_PATTERN, (_match, userId) => formatUserMention(userId, guildId, i18n));
	formatted = formatted.replace(ROLE_MENTION_PATTERN, (_match, roleId) => formatRoleMention(roleId, guildId, i18n));
	formatted = formatted.replace(CHANNEL_MENTION_PATTERN, (_match, channelId) => formatChannelMention(channelId, i18n));
	formatted = formatted.replace(CUSTOM_EMOJI_PATTERN, (_match, emojiName) => i18n._(EMOJI_DESCRIPTOR, {emojiName}));
	formatted = formatted.replace(SLASH_COMMAND_PATTERN, (_match, commandName) =>
		i18n._(SLASH_DESCRIPTOR, {commandName}),
	);
	formatted = formatted.replace(TIMESTAMP_PATTERN, (_match, timestamp, style) =>
		formatTimestampForTts(Number.parseInt(timestamp, 10), style),
	);
	formatted = formatted.replace(INLINE_CODE_PATTERN, '$1');
	formatted = formatted.replace(MASKED_LINK_PATTERN, '$1');
	formatted = formatted.replace(BOLD_ITALIC_PATTERN, '$1');
	formatted = formatted.replace(BOLD_PATTERN, '$1');
	formatted = formatted.replace(UNDERLINE_PATTERN, '$1');
	formatted = formatted.replace(STRIKETHROUGH_PATTERN, '$1');
	formatted = formatted.replace(ITALIC_PATTERN, '$1');
	formatted = formatted.replace(BLOCKQUOTE_PATTERN, '');
	formatted = formatted.replace(HEADER_PATTERN, '');
	formatted = formatted.replace(/\s+/g, ' ').trim();
	if (replyAuthorName) {
		return i18n._(REPLYING_TO_SAID_DESCRIPTOR, {replyAuthorName, authorName, formatted});
	}
	return i18n._(SAID_DESCRIPTOR, {authorName, formatted});
}
