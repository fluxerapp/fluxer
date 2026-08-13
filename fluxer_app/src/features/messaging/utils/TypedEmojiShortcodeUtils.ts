// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {
	type AvailabilityCheck,
	checkEmojiAvailabilityWithGuildFallback,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import type {I18n} from '@lingui/core';

const TYPED_EMOJI_SHORTCODE_PATTERN = /:([\p{L}\p{N}_+~.-]{2,}):/gu;
const CUSTOM_EMOJI_SHORTCODE_NAME_PATTERN = /^[a-zA-Z0-9_+~-]{2,}$/;
const HTTP_PREFIX = 'http://';
const HTTPS_PREFIX = 'https://';
const APP_PROTOCOL_SCHEME = 'fluxer:';
const TRIMMED_AUTOLINK_PUNCTUATION = new Set(['.', ',', ';', ':', '!', '?']);

type ShortcodeResolver = (shortcodeName: string) => string | null | undefined;

export type ResolvedTypedEmoji =
	| {
			kind: 'standard';
			name: string;
			surrogate: string;
			url: string | null;
			display: string;
	  }
	| {
			kind: 'custom';
			emojiId: string;
			animated: boolean;
			display: string;
			wire: string;
	  };

interface ResolveTypedEmojiShortcodesOptions {
	content: string;
	channel: Channel | null;
	guildIdFallback?: string | null;
	i18n: I18n;
}

interface TextSpan {
	start: number;
	end: number;
}

function isExistingCustomEmojiMarkdown(content: string, matchIndex: number): boolean {
	return content[matchIndex - 1] === '<' || (content[matchIndex - 2] === '<' && content[matchIndex - 1] === 'a');
}

function isUrlStart(content: string, index: number): boolean {
	if (content.startsWith(HTTP_PREFIX, index) || content.startsWith(HTTPS_PREFIX, index)) {
		return true;
	}
	if (!content.startsWith(APP_PROTOCOL_SCHEME, index)) {
		return false;
	}
	const nextChar = content[index + APP_PROTOCOL_SCHEME.length] ?? '';
	return nextChar === '/' || /[A-Za-z0-9_-]/u.test(nextChar);
}

function isUrlTerminationChar(char: string): boolean {
	return (
		char === '' ||
		char === ' ' ||
		char === '\t' ||
		char === '\n' ||
		char === '\r' ||
		char === ')' ||
		char === '"' ||
		char === '<' ||
		char === '>'
	);
}

function hasTerminalTld(text: string): boolean {
	let index = text.length;
	let letterCount = 0;
	while (index > 0 && /[A-Za-z]/u.test(text[index - 1])) {
		letterCount++;
		index--;
	}
	return letterCount >= 2 && index > 0 && text[index - 1] === '.';
}

function findUrlEnd(content: string, start: number): number {
	let end = start;
	let parenDepth = 0;
	while (end < content.length) {
		const char = content[end];
		if (char === '(') {
			parenDepth++;
			end++;
			continue;
		}
		if (char === ')') {
			if (parenDepth > 0) {
				parenDepth--;
				end++;
				continue;
			}
			break;
		}
		if (isUrlTerminationChar(char)) {
			break;
		}
		end++;
	}
	while (
		end > start &&
		TRIMMED_AUTOLINK_PUNCTUATION.has(content[end - 1]) &&
		!hasTerminalTld(content.slice(start, end))
	) {
		end--;
	}
	return end;
}

function findUrlSpans(content: string): Array<TextSpan> {
	const spans: Array<TextSpan> = [];
	let index = 0;
	while (index < content.length) {
		if (!isUrlStart(content, index)) {
			index++;
			continue;
		}
		const end = findUrlEnd(content, index);
		if (end > index) {
			spans.push({start: index, end});
			index = end;
			continue;
		}
		index++;
	}
	return spans;
}

function isInsideSpan(spans: ReadonlyArray<TextSpan>, index: number): boolean {
	return spans.some((span) => index >= span.start && index < span.end);
}

function isCustomEmoji(emoji: FlatEmoji): boolean {
	return !!emoji.id && !!emoji.guildId;
}

function isAvailable(i18n: I18n, emoji: FlatEmoji, channel: Channel | null, guildIdFallback: string | null): boolean {
	const availability: AvailabilityCheck = checkEmojiAvailabilityWithGuildFallback(
		i18n,
		emoji,
		channel,
		guildIdFallback,
	);
	return availability.canUse;
}

export function replaceTypedEmojiShortcodes(content: string, resolveShortcode: ShortcodeResolver): string {
	if (!content.includes(':')) {
		return content;
	}
	let urlSpans: Array<TextSpan> | null = null;
	return content.replace(TYPED_EMOJI_SHORTCODE_PATTERN, (match, shortcodeName: string, matchIndex: number) => {
		if (isExistingCustomEmojiMarkdown(content, matchIndex)) {
			return match;
		}
		urlSpans ??= findUrlSpans(content);
		if (isInsideSpan(urlSpans, matchIndex)) {
			return match;
		}
		return resolveShortcode(shortcodeName) ?? match;
	});
}

export function resolveTypedEmojiShortcodes({
	content,
	channel,
	guildIdFallback = null,
	i18n,
}: ResolveTypedEmojiShortcodesOptions): string {
	return replaceTypedEmojiShortcodes(content, (shortcodeName) => {
		if (UnicodeEmojis.findEmojiByShortcodeName(shortcodeName)) {
			return null;
		}
		if (!CUSTOM_EMOJI_SHORTCODE_NAME_PATTERN.test(shortcodeName)) {
			return null;
		}
		const emoji = Emoji.findCustomEmojiForShortcode(channel, shortcodeName, guildIdFallback);
		if (!emoji || !isCustomEmoji(emoji) || !isAvailable(i18n, emoji, channel, guildIdFallback)) {
			return null;
		}
		return Emoji.getEmojiMarkdown(emoji);
	});
}

export function resolveTypedEmojiToken(
	shortcodeName: string,
	channel: Channel | null,
	guildIdFallback: string | null,
	i18n: I18n,
): ResolvedTypedEmoji | null {
	if (UnicodeEmojis.findEmojiByShortcodeName(shortcodeName)) {
		const emoji = UnicodeEmojis.findEmojiByShortcodeName(shortcodeName);
		if (!emoji) return null;
		return {
			kind: 'standard',
			name: emoji.uniqueName,
			surrogate: emoji.surrogates,
			url: EmojiUtils.getEmojiURL(emoji.surrogates),
			display: `:${emoji.uniqueName}:`,
		};
	}
	if (!CUSTOM_EMOJI_SHORTCODE_NAME_PATTERN.test(shortcodeName)) return null;
	const emoji = Emoji.findCustomEmojiForShortcode(channel, shortcodeName, guildIdFallback);
	if (!emoji || !isCustomEmoji(emoji) || !emoji.id || !isAvailable(i18n, emoji, channel, guildIdFallback)) return null;
	return {
		kind: 'custom',
		emojiId: emoji.id,
		animated: Boolean(emoji.animated),
		display: `:${shortcodeName}:`,
		wire: Emoji.getEmojiMarkdown(emoji),
	};
}
