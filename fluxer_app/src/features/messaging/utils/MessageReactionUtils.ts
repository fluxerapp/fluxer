// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import Channels from '@app/features/channel/state/Channels';
import type {UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import EmojiCatalog from '@app/features/expressions/utils/EmojiCatalog';
import * as EmojiImageUtils from '@app/features/expressions/utils/EmojiImageUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {getReactionKey, type ReactionEmoji} from '@app/features/messaging/utils/ReactionEmoji';
import * as AvatarSourceUtils from '@app/features/user/utils/AvatarSourceUtils';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {msg, plural} from '@lingui/core/macro';
import {useEffect, useState} from 'react';

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

export {getReactionKey, type ReactionEmoji};

export function getReactionTooltip(message: Message, emoji: ReactionEmoji) {
	const channel = Channels.getChannel(message.channelId);
	let guildId = message.guildId;
	if (channel !== null && channel !== undefined && channel.guildId !== undefined) {
		guildId = channel.guildId;
	}
	const users = MessageReactions.getReactions(message.id, emoji)
		.slice(0, 3)
		.map((user) => DisplayNameUtils.getNickname(user, guildId));
	if (users.length === 0) {
		return '';
	}
	const reaction = message.getReaction(emoji);
	const reactionCount = reaction === null || reaction === undefined ? 0 : reaction.count;
	const othersCount = Math.max(0, (reactionCount || 0) - users.length);
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
		const canonicalName =
			emoji.uniqueName === null || emoji.uniqueName === undefined ? emoji.name.replace(/~\d+$/, '') : emoji.uniqueName;
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

export function getEmojiName(emoji: ReactionEmoji): string {
	if (emoji.id != null) {
		return `:${emoji.name}:`;
	}
	const surrogate = EmojiCatalog.normalizeShortcodeToSurrogate(emoji.name);
	return EmojiCatalog.getSurrogateName(surrogate) || surrogate;
}

export function getEmojiNameWithColons(emoji: ReactionEmoji): string {
	if (emoji.id != null) {
		return `:${emoji.name}:`;
	}
	const surrogate = EmojiCatalog.normalizeShortcodeToSurrogate(emoji.name);
	const name = EmojiCatalog.getSurrogateName(surrogate);
	return name ? `:${name}:` : surrogate;
}

function getCustomEmojiURL(id: string, animated: boolean, size: number): string {
	return setUrlQueryParams(AvatarSourceUtils.getEmojiURL({id, animated}), {size, quality: 'lossless'});
}

export interface EmojiURLParams {
	readonly emoji: ReactionEmoji;
	readonly isHovering?: boolean;
	readonly size?: number;
	readonly forceAnimate?: boolean;
	readonly enabled?: boolean;
}

export function useEmojiURL({
	emoji,
	isHovering = false,
	size = 128,
	forceAnimate = false,
	enabled = true,
}: EmojiURLParams): string | null {
	const shouldAnimate = useShouldAnimate({
		kind: 'emoji',
		isHovering: isHovering || forceAnimate,
	});
	const staticUrl =
		emoji.id == null
			? EmojiImageUtils.getEmojiURL(EmojiCatalog.normalizeShortcodeToSurrogate(emoji.name))
			: getCustomEmojiURL(emoji.id, false, size);
	const animatedUrl = emoji.id != null && emoji.animated ? getCustomEmojiURL(emoji.id, true, size) : null;
	const [loadedAnimatedUrl, setLoadedAnimatedUrl] = useState<string | null>(() =>
		ImageCacheUtils.hasImage(animatedUrl) ? animatedUrl : null,
	);
	useEffect(() => {
		if (!enabled) return;
		ImageCacheUtils.pinImage(staticUrl);
	}, [enabled, staticUrl]);
	useEffect(() => {
		if (!enabled || !shouldAnimate || !animatedUrl) return;
		return ImageCacheUtils.loadImage(animatedUrl, () => setLoadedAnimatedUrl(animatedUrl));
	}, [animatedUrl, enabled, shouldAnimate]);
	if (!enabled) return null;
	return shouldAnimate && animatedUrl === loadedAnimatedUrl ? animatedUrl : staticUrl;
}
