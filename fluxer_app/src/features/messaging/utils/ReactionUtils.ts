// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import Channels from '@app/features/channel/state/Channels';
import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg, plural} from '@lingui/core/macro';

const REACTED_BY_DESCRIPTOR = msg({
	message: '{emojiName} reacted by {reactors}',
	comment: 'Label in the reaction utils helper. Preserve {emojiName}, {reactors}; they are inserted by code.',
});
const listFormatterCache = new Map<string, Intl.ListFormat>();

function getReactorListFormatter(locale: string): Intl.ListFormat {
	let formatter = listFormatterCache.get(locale);
	if (!formatter) {
		formatter = new Intl.ListFormat(locale, {type: 'conjunction', style: 'long'});
		listFormatterCache.set(locale, formatter);
	}
	return formatter;
}

export interface ReactionEmoji {
	id?: string | null;
	name: string;
	animated?: boolean;
	url?: string | null;
}

export function getReactionTooltip(message: Message, emoji: ReactionEmoji) {
	const guildId = Channels.getChannel(message.channelId)?.guildId ?? message.guildId;
	const users = MessageReactions.getReactions(message.id, emoji)
		.slice(0, 3)
		.map((user) => NicknameUtils.getNickname(user, guildId));
	if (users.length === 0) {
		return '';
	}
	const reaction = message.getReaction(emoji);
	const othersCount = Math.max(0, (reaction?.count || 0) - users.length);
	const emojiName = getEmojiNameWithColons(emoji);
	const parts: Array<string> = [...users];
	if (othersCount > 0) {
		parts.push(plural({count: othersCount}, {one: '# other', other: '# others'}));
	}
	const reactors = getReactorListFormatter(getCurrentLocale()).format(parts);
	return i18n._(REACTED_BY_DESCRIPTOR, {emojiName, reactors});
}

const isCustomEmoji = (emoji: UnicodeEmoji | ReactionEmoji): emoji is ReactionEmoji =>
	'id' in emoji && emoji.id != null;

export function toReactionEmoji(emoji: UnicodeEmoji | ReactionEmoji): ReactionEmoji {
	if (isCustomEmoji(emoji)) {
		const uniqueName = (emoji as {uniqueName?: string}).uniqueName;
		const canonicalName = uniqueName ?? emoji.name.replace(/~\d+$/, '');
		if (canonicalName === emoji.name) {
			return emoji;
		}
		return {...emoji, name: canonicalName};
	}
	return {name: getSkinTonedSurrogate(emoji)};
}

export function emojiEquals(reactionEmoji: ReactionEmoji, emoji: UnicodeEmoji | ReactionEmoji) {
	return isCustomEmoji(emoji)
		? emoji.id === reactionEmoji.id
		: reactionEmoji.id == null && emoji.name === reactionEmoji.name;
}

export function getReactionKey(messageId: string, emoji: ReactionEmoji) {
	return `${messageId}:${emoji.name}:${emoji.id || ''}`;
}

export function getEmojiName(emoji: ReactionEmoji): string {
	if (emoji.id != null) {
		return `:${emoji.name}:`;
	}
	const surrogate = UnicodeEmojis.normalizeEmojiNameToSurrogate(emoji.name);
	return UnicodeEmojis.getSurrogateName(surrogate) || surrogate;
}

export function getEmojiNameWithColons(emoji: ReactionEmoji): string {
	if (emoji.id != null) {
		return `:${emoji.name}:`;
	}
	const surrogate = UnicodeEmojis.normalizeEmojiNameToSurrogate(emoji.name);
	const name = UnicodeEmojis.getSurrogateName(surrogate);
	return name ? `:${name}:` : surrogate;
}

export function useEmojiURL({
	emoji,
	isHovering = false,
	size,
	forceAnimate = false,
}: {
	emoji: ReactionEmoji;
	isHovering?: boolean;
	size?: number;
	forceAnimate?: boolean;
}): string | null {
	const shouldAnimate = useShouldAnimate({
		kind: 'emoji',
		isAnimated: emoji.animated === true,
		isHovering: isHovering || forceAnimate,
	});
	if (emoji.id == null) {
		return EmojiUtils.getEmojiURL(UnicodeEmojis.normalizeEmojiNameToSurrogate(emoji.name));
	}
	return buildCustomEmojiURL({id: emoji.id, animated: emoji.animated === true && shouldAnimate, size});
}
