/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Command} from '~/components/channel/Autocomplete';

const MENTION_REGEX = /(^|\s)@(\S*)$/;
const CHANNEL_REGEX = /(^|\s)#(\S*)$/;
const EMOJI_REGEX = /(^|\s):([a-z0-9_+-]{2,})$/i;
const EMOJI_REACTION_REGEX = /^\+:([a-z0-9_+-]*)$/i;
const COMMAND_REGEX = /(^\s*)\/(\S*)$/;
const MEME_SEARCH_REGEX = /(^\s*)\/saved\s*(.*)$/;
const GIF_SEARCH_REGEX = /(^\s*)\/(tenor|gif)\s*(.*)$/;
const STICKER_SEARCH_REGEX = /(^\s*)\/sticker\s*(.*)$/;
const COMMAND_ARG_MENTION_REGEX = /(^\s*)\/(kick|ban|msg|saved)\s+@(\S*)$/;
const COMMAND_ARG_REGEX = /(^\s*)\/(kick|ban|msg)\s+(\S*)$/;

interface AutocompleteTrigger {
	type:
		| 'mention'
		| 'channel'
		| 'emoji'
		| 'emojiReaction'
		| 'command'
		| 'meme'
		| 'gif'
		| 'sticker'
		| 'commandArgMention'
		| 'commandArg';
	match: RegExpMatchArray;
	matchedText: string;
}

export function detectAutocompleteTrigger(textUpToCursor: string): AutocompleteTrigger | null {
	const emojiReactionMatch = textUpToCursor.match(EMOJI_REACTION_REGEX);
	if (emojiReactionMatch) {
		return {
			type: 'emojiReaction',
			match: emojiReactionMatch,
			matchedText: emojiReactionMatch[1] || '',
		};
	}

	const commandArgMentionMatch = textUpToCursor.match(COMMAND_ARG_MENTION_REGEX);
	if (commandArgMentionMatch) {
		return {
			type: 'commandArgMention',
			match: commandArgMentionMatch,
			matchedText: commandArgMentionMatch[3],
		};
	}

	const commandArgMatch = textUpToCursor.match(COMMAND_ARG_REGEX);
	if (commandArgMatch) {
		return {
			type: 'commandArg',
			match: commandArgMatch,
			matchedText: commandArgMatch[3],
		};
	}

	const mentionMatch = textUpToCursor.match(MENTION_REGEX);
	if (mentionMatch) {
		return {
			type: 'mention',
			match: mentionMatch,
			matchedText: mentionMatch[2],
		};
	}

	const channelMatch = textUpToCursor.match(CHANNEL_REGEX);
	if (channelMatch) {
		return {
			type: 'channel',
			match: channelMatch,
			matchedText: channelMatch[2],
		};
	}

	const emojiMatch = textUpToCursor.match(EMOJI_REGEX);
	if (emojiMatch) {
		const q = emojiMatch[2] || '';
		if (q.length < 2) return null;
		return {
			type: 'emoji',
			match: emojiMatch,
			matchedText: q,
		};
	}

	const memeSearchMatch = textUpToCursor.match(MEME_SEARCH_REGEX);
	if (memeSearchMatch) {
		return {
			type: 'meme',
			match: memeSearchMatch,
			matchedText: memeSearchMatch[2],
		};
	}

	const gifSearchMatch = textUpToCursor.match(GIF_SEARCH_REGEX);
	if (gifSearchMatch) {
		return {
			type: 'gif',
			match: gifSearchMatch,
			matchedText: gifSearchMatch[3],
		};
	}

	const stickerSearchMatch = textUpToCursor.match(STICKER_SEARCH_REGEX);
	if (stickerSearchMatch) {
		return {
			type: 'sticker',
			match: stickerSearchMatch,
			matchedText: stickerSearchMatch[2],
		};
	}

	const commandMatch = textUpToCursor.match(COMMAND_REGEX);
	if (commandMatch) {
		return {
			type: 'command',
			match: commandMatch,
			matchedText: commandMatch[2],
		};
	}

	return null;
}

export function filterCommandsByQuery(commands: Array<Command>, query: string): Array<Command> {
	if (!query) {
		return commands;
	}

	return commands.filter((command) => command.name.toLowerCase().includes(query.toLowerCase()));
}

export function isCommandRequiringUserMention(commandName: string): boolean {
	return ['/kick', '/ban', '/msg', '/saved'].includes(commandName);
}

export function getCommandInsertionText(command: Command): string {
	if (command.type === 'simple') {
		return command.content;
	}

	return `${command.name} `;
}
