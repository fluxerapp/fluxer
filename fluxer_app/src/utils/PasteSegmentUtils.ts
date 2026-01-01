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

import type {MentionSegment} from './TextareaSegmentManager';

const MARKDOWN_MENTION_PATTERN = /<(@|#|@&)([a-zA-Z0-9]+)>/g;

const EMOJI_MARKDOWN_PATTERN = /<(a)?:([^:]+):([a-zA-Z0-9]+)>/g;

interface PastedSegmentInfo {
	displayText: string;
	actualText: string;
	type: MentionSegment['type'];
	id: string;
	start: number;
	end: number;
}

export interface LookupFunctions {
	userById: (id: string) => {id: string; tag: string} | null;
	channelById: (id: string) => {id: string; name: string} | null;
	roleById: (id: string) => {id: string; name: string} | null;
	emojiById: (id: string) => {id: string; name: string; uniqueName: string} | null;
}

export function detectPastedSegments(
	pastedText: string,
	pastePosition: number,
	lookups: LookupFunctions,
): Array<PastedSegmentInfo> {
	const segments: Array<PastedSegmentInfo> = [];

	let match: RegExpExecArray | null;
	MARKDOWN_MENTION_PATTERN.lastIndex = 0;
	while ((match = MARKDOWN_MENTION_PATTERN.exec(pastedText)) !== null) {
		const [fullMatch, prefix, id] = match;
		const start = pastePosition + match.index;
		const end = start + fullMatch.length;

		let type: MentionSegment['type'];
		let displayText: string | null = null;

		if (prefix === '@') {
			type = 'user';
			const user = lookups.userById(id);
			if (user) {
				displayText = `@${user.tag}`;
			}
		} else if (prefix === '#') {
			type = 'channel';
			const channel = lookups.channelById(id);
			if (channel) {
				displayText = `#${channel.name}`;
			}
		} else if (prefix === '@&') {
			type = 'role';
			const role = lookups.roleById(id);
			if (role) {
				displayText = `@${role.name}`;
			}
		} else {
			continue;
		}

		if (displayText) {
			segments.push({
				displayText,
				actualText: fullMatch,
				type,
				id,
				start,
				end,
			});
		}
	}

	EMOJI_MARKDOWN_PATTERN.lastIndex = 0;
	while ((match = EMOJI_MARKDOWN_PATTERN.exec(pastedText)) !== null) {
		const [fullMatch, , , emojiId] = match;
		const emoji = lookups.emojiById(emojiId);

		if (emoji) {
			const start = pastePosition + match.index;
			const end = start + fullMatch.length;

			const overlaps = segments.some((seg) => start < seg.end && end > seg.start);
			if (!overlaps) {
				segments.push({
					displayText: `:${emoji.name}:`,
					actualText: fullMatch,
					type: 'emoji',
					id: emoji.id,
					start,
					end,
				});
			}
		}
	}

	return segments.sort((a, b) => a.start - b.start);
}
