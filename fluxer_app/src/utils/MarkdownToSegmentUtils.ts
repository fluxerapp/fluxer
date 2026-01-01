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

import ChannelStore from '~/stores/ChannelStore';
import EmojiStore from '~/stores/EmojiStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import type {MentionSegment, TextareaSegmentManager} from './TextareaSegmentManager';

const MARKDOWN_SEGMENT_PATTERN = /<(@|#|@&)([a-zA-Z0-9]+)>|<(a)?:([^:]+):([a-zA-Z0-9]+)>/g;

interface SegmentConversionResult {
	displayText: string;
	segments: Array<{
		start: number;
		displayText: string;
		actualText: string;
		type: MentionSegment['type'];
		id: string;
	}>;
}

export function convertMarkdownToSegments(markdown: string, guildId?: string | null): SegmentConversionResult {
	let displayText = markdown;
	let offset = 0;
	const segments: SegmentConversionResult['segments'] = [];

	const matches = Array.from(markdown.matchAll(MARKDOWN_SEGMENT_PATTERN));

	for (const match of matches) {
		const fullMatch = match[0];
		const originalStart = match.index!;
		const adjustedStart = originalStart + offset;

		let segmentDisplayText: string | null = null;
		let segmentType: MentionSegment['type'] | null = null;
		let segmentId: string | null = null;

		if (match[1] && match[2]) {
			const prefix = match[1];
			const id = match[2];

			if (prefix === '@') {
				const user = UserStore.getUser(id);
				if (user) {
					segmentDisplayText = `@${user.tag}`;
					segmentType = 'user';
					segmentId = id;
				}
			} else if (prefix === '#') {
				const foundChannel = ChannelStore.getChannel(id);
				if (foundChannel?.name) {
					segmentDisplayText = `#${foundChannel.name}`;
					segmentType = 'channel';
					segmentId = id;
				}
			} else if (prefix === '@&') {
				const roles = GuildStore.getGuildRoles(guildId ?? '');
				const role = roles.find((r) => r.id === id);
				if (role) {
					segmentDisplayText = `@${role.name}`;
					segmentType = 'role';
					segmentId = id;
				}
			}
		} else if (match[4] && match[5]) {
			const emojiId = match[5];
			let emoji: {id: string; name: string} | undefined;

			if (guildId) {
				const emojis = EmojiStore.getGuildEmoji(guildId);
				emoji = emojis.find((e) => e.id === emojiId);
			}

			if (!emoji) {
				const guilds = GuildStore.getGuilds();
				for (const guild of guilds) {
					const emojis = EmojiStore.getGuildEmoji(guild.id);
					emoji = emojis.find((e) => e.id === emojiId);
					if (emoji) break;
				}
			}

			if (emoji) {
				segmentDisplayText = `:${emoji.name}:`;
				segmentType = 'emoji';
				segmentId = emojiId;
			}
		}

		if (segmentDisplayText && segmentType && segmentId) {
			displayText =
				displayText.slice(0, adjustedStart) + segmentDisplayText + displayText.slice(adjustedStart + fullMatch.length);

			segments.push({
				start: adjustedStart,
				displayText: segmentDisplayText,
				actualText: fullMatch,
				type: segmentType,
				id: segmentId,
			});

			offset += segmentDisplayText.length - fullMatch.length;
		}
	}

	return {displayText, segments};
}

export function applyMarkdownSegments(
	markdown: string,
	guildId: string | null | undefined,
	segmentManager: TextareaSegmentManager,
): string {
	const {displayText, segments} = convertMarkdownToSegments(markdown, guildId);

	for (const segment of segments) {
		segmentManager.insertSegment(
			displayText.slice(0, segment.start + segment.displayText.length),
			segment.start,
			segment.displayText,
			segment.actualText,
			segment.type,
			segment.id,
		);
	}

	return displayText;
}
