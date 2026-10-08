// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Messages from '@app/features/messaging/state/MessagingMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/MessageReactionUtils';
import {resolveTypedEmojiToken} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import type {I18n} from '@lingui/core';

const REACTION_SHORTHAND_PATTERN = /^\s*\+:([^\s:]+(?:::skin-tone-[1-5])?):\s*$/u;
const REACTION_SHORTHAND_PREFIX_PATTERN = /^\s*\+$/;

export function isReactionShorthandText(text: string): boolean {
	return REACTION_SHORTHAND_PATTERN.test(text);
}

export function isReactionShorthandPrefix(textBefore: string, textAfter: string): boolean {
	return REACTION_SHORTHAND_PREFIX_PATTERN.test(textBefore) && textAfter.trim() === '';
}

export function getReactionShortcodeName(emoji: FlatEmoji): string {
	if (emoji.id) {
		return emoji.name;
	}
	const name = UnicodeEmojis.nameForSurrogate(getSkinTonedSurrogate(emoji), false);
	return name === '' ? emoji.name : name;
}

export function parseReactionShorthand(
	content: string,
	channel: Channel | null,
	guildId: string | null,
	i18n: I18n,
): ReactionEmoji | null {
	const match = REACTION_SHORTHAND_PATTERN.exec(content);
	if (match === null) {
		return null;
	}
	const shortcodeName = match[1];
	const unicodeEmoji = UnicodeEmojis.findEmojiByShortcodeName(shortcodeName);
	if (unicodeEmoji !== null) {
		return {name: unicodeEmoji.surrogates};
	}
	const resolved = resolveTypedEmojiToken(shortcodeName, channel, guildId, i18n);
	if (resolved === null || resolved.kind !== 'custom') {
		return null;
	}
	return {id: resolved.emojiId, name: shortcodeName.replace(/~\d+$/, ''), animated: resolved.animated};
}

export function getReactionShorthandTargetId(channelId: string): string | null {
	const reply = MessageReply.getReplyingMessage(channelId);
	if (reply !== null) {
		return reply.messageId;
	}
	const messages = Messages.getMessages(channelId).toArray();
	return messages[messages.length - 1]?.id ?? null;
}
