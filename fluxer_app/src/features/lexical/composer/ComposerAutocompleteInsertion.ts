// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Command} from '@app/features/devtools/hooks/useCommands';
import {getCommandInsertionText} from '@app/features/messaging/utils/SlashCommandUtils';

export type ComposerAutocompleteTriggerType =
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

export function getComposerAutocompleteReplacementStart(
	textUpToCursor: string,
	type: ComposerAutocompleteTriggerType,
	match: RegExpMatchArray,
): number {
	const matchedArgument = match[3];
	const matchedPrefix = match[1];
	if (type === 'commandArgMention') {
		return Math.max(0, textUpToCursor.length - (matchedArgument == null ? 0 : matchedArgument.length) - 1);
	}
	if (type === 'commandArg') {
		return Math.max(0, textUpToCursor.length - (matchedArgument == null ? 0 : matchedArgument.length));
	}
	return Math.max(0, (match.index == null ? 0 : match.index) + (matchedPrefix == null ? 0 : matchedPrefix.length));
}

export function getComposerCommandInsertionText(command: Command): string {
	return command.type === 'simple' ? `${command.content} ` : getCommandInsertionText(command);
}

export interface ComposerCommandReplacement {
	start: number;
	end: number;
	text: string;
}

export function createComposerCommandReplacement(
	display: string,
	textUpToCursor: string,
	matchStart: number,
	caret: number,
	command: Command,
): ComposerCommandReplacement {
	const beforeMatch = textUpToCursor.slice(0, matchStart);
	const start = beforeMatch.trim().length === 0 ? 0 : matchStart;
	const replacesRemainder = command.name === '/me' || command.name === '/spoiler';
	return {
		start,
		end: replacesRemainder ? display.length : caret,
		text: getComposerCommandInsertionText(command),
	};
}
