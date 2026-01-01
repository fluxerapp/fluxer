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

import {type MentionSegment, TextareaSegmentManager} from './TextareaSegmentManager';

interface AutocompleteMatch {
	type: 'mention' | 'channel' | 'emoji' | 'command';
	query: string;
	matchStart: number;
	matchEnd: number;
}

const MENTION_REGEX = /(^|\s)@(\S*)$/;
const CHANNEL_REGEX = /(^|\s)#(\S*)$/;
const EMOJI_REGEX = /(^|\s):([^\s]{2,})$/;
const COMMAND_REGEX = /(^|\s)\/$/;

export class TextareaStateManager {
	private segmentManager: TextareaSegmentManager;
	private text: string = '';
	private cursorPosition: number = 0;

	constructor() {
		this.segmentManager = new TextareaSegmentManager();
	}

	getText(): string {
		return this.text;
	}

	setText(text: string): void {
		this.text = text;
	}

	getCursorPosition(): number {
		return this.cursorPosition;
	}

	setCursorPosition(position: number): void {
		this.cursorPosition = position;
	}

	getSegmentManager(): TextareaSegmentManager {
		return this.segmentManager;
	}

	clear(): void {
		this.text = '';
		this.cursorPosition = 0;
		this.segmentManager.clear();
	}

	handleTextChange(newText: string): void {
		const {changeStart, changeEnd, replacementLength} = TextareaSegmentManager.detectChange(this.text, newText);
		this.segmentManager.updateSegmentsForTextChange(changeStart, changeEnd, replacementLength);
		this.text = newText;
	}

	getTextUpToCursor(): string {
		return this.text.slice(0, this.cursorPosition);
	}

	detectAutocompleteMatch(): AutocompleteMatch | null {
		const textUpToCursor = this.getTextUpToCursor();

		const mentionMatch = textUpToCursor.match(MENTION_REGEX);
		if (mentionMatch) {
			return {
				type: 'mention',
				query: mentionMatch[2],
				matchStart: mentionMatch.index ?? 0,
				matchEnd: (mentionMatch.index ?? 0) + mentionMatch[0].length,
			};
		}

		const channelMatch = textUpToCursor.match(CHANNEL_REGEX);
		if (channelMatch) {
			return {
				type: 'channel',
				query: channelMatch[2],
				matchStart: channelMatch.index ?? 0,
				matchEnd: (channelMatch.index ?? 0) + channelMatch[0].length,
			};
		}

		const emojiMatch = textUpToCursor.match(EMOJI_REGEX);
		if (emojiMatch) {
			return {
				type: 'emoji',
				query: emojiMatch[2],
				matchStart: emojiMatch.index ?? 0,
				matchEnd: (emojiMatch.index ?? 0) + emojiMatch[0].length,
			};
		}

		const commandMatch = textUpToCursor.match(COMMAND_REGEX);
		if (commandMatch) {
			return {
				type: 'command',
				query: '',
				matchStart: commandMatch.index ?? 0,
				matchEnd: (commandMatch.index ?? 0) + commandMatch[0].length,
			};
		}

		return null;
	}

	insertAutocompleteSegment(
		match: AutocompleteMatch,
		displayText: string,
		actualText: string,
		segmentType: MentionSegment['type'],
		segmentId: string,
	): {newText: string; newCursorPosition: number} {
		const beforeMatch = this.text.slice(0, match.matchStart);
		const afterMatch = this.text.slice(match.matchEnd);

		const needsSpaceBefore = !beforeMatch.endsWith(' ') && beforeMatch.length > 0;
		const prefix = needsSpaceBefore ? ' ' : '';
		const insertPosition = match.matchStart + prefix.length;

		this.segmentManager.updateSegmentsForTextChange(match.matchStart, match.matchEnd, prefix.length);

		const tempText = beforeMatch + prefix;
		const {newText: updatedText} = this.segmentManager.insertSegment(
			tempText,
			insertPosition,
			`${displayText} `,
			`${actualText} `,
			segmentType,
			segmentId,
		);

		const finalText = (updatedText + afterMatch).trimStart();
		const newCursorPosition = beforeMatch.length + prefix.length + displayText.length + 1;

		this.text = finalText;
		this.cursorPosition = newCursorPosition;

		return {newText: finalText, newCursorPosition};
	}

	insertPlainText(match: AutocompleteMatch, text: string): {newText: string; newCursorPosition: number} {
		const beforeMatch = this.text.slice(0, match.matchStart);
		const afterMatch = this.text.slice(match.matchEnd);

		const needsSpaceBefore = !beforeMatch.endsWith(' ') && beforeMatch.length > 0;
		const prefix = needsSpaceBefore ? ' ' : '';

		const newText = `${beforeMatch + prefix + text} ${afterMatch}`.trimStart();
		const newCursorPosition = beforeMatch.length + prefix.length + text.length + 1;

		this.text = newText;
		this.cursorPosition = newCursorPosition;

		return {newText, newCursorPosition};
	}

	insertSegmentAtCursor(
		displayText: string,
		actualText: string,
		segmentType: MentionSegment['type'],
		segmentId: string,
	): string {
		const needsSpace = this.text.length > 0 && !this.text.endsWith(' ');
		const prefix = this.text.length === 0 ? '' : needsSpace ? ' ' : '';
		const insertPosition = this.text.length + prefix.length;

		const {newText} = this.segmentManager.insertSegment(
			this.text + prefix,
			insertPosition,
			displayText,
			actualText,
			segmentType,
			segmentId,
		);

		this.text = newText;

		return newText;
	}

	getActualContent(): string {
		return this.segmentManager.displayToActual(this.text);
	}

	hasOpenCodeBlock(): boolean {
		const textUpToCursor = this.getTextUpToCursor();
		const match = textUpToCursor.match(/```/g);
		return match != null && match.length > 0 && match.length % 2 !== 0;
	}
}
