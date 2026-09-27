// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {type ReactionEmoji, toReactionEmoji} from '@app/features/messaging/utils/MessageReactionUtils';

const REACTION_SHORTHAND_PATTERN = /^\+(\S+)$/u;
const CUSTOM_EMOJI_MARKDOWN_PATTERN = /^<(a)?:([a-zA-Z0-9_+-]{2,}):(\d+)>$/;
const SHORTCODE_PATTERN = /^:([^\s:]+):$/;

export function parseReactionShorthand(content: string): ReactionEmoji | null {
	const match = REACTION_SHORTHAND_PATTERN.exec(content.trim());
	if (match === null) {
		return null;
	}
	const token = match[1];
	const customMatch = CUSTOM_EMOJI_MARKDOWN_PATTERN.exec(token);
	if (customMatch !== null) {
		return {id: customMatch[3], name: customMatch[2], animated: customMatch[1] === 'a'};
	}
	const shortcodeMatch = SHORTCODE_PATTERN.exec(token);
	if (shortcodeMatch !== null) {
		const emoji = UnicodeEmojis.findEmojiByShortcodeName(shortcodeMatch[1]);
		return emoji === null ? null : toReactionEmoji(emoji);
	}
	const name = UnicodeEmojis.nameForSurrogate(token, false);
	if (name === '') {
		return null;
	}
	return {name: UnicodeEmojis.surrogateForName(name, token)};
}

export function getReactionShorthandTargetId(channelId: string): string | null {
	const reply = MessageReply.getReplyingMessage(channelId);
	if (reply !== null) {
		return reply.messageId;
	}
	const messages = Messages.getMessages(channelId).toArray();
	return messages[messages.length - 1]?.id ?? null;
}
