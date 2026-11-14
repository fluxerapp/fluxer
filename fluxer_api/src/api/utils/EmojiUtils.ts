// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, UserID, WebhookID} from '@app/api/BrandedTypes';
import {createEmojiID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

type EmojiGuildRepository = Pick<GuildRepository, 'getEmoji' | 'getEmojiById' | 'findUnique'>;
type EmojiUserRepository = Pick<UserRepository, 'findUnique'>;

const CUSTOM_EMOJI_MARKDOWN_REGEX = /<(a)?:([^:]+):(\d+)>/g;
const CUSTOM_EMOJI_MARKDOWN_REGEX_GLOBAL = new RegExp(CUSTOM_EMOJI_MARKDOWN_REGEX.source, 'g');
const CODE_FENCE_LENGTH = 3;
const MAX_CODE_BLOCK_SEARCH_LINES = 1000;
const MAX_LINE_LENGTH = 4096;
const LIST_ITEM_PREFIX = /^(?: {2,})?(?:[*-] |\d+\. )/;
const SPOILER_LINE_START = /(?:^|\n)[ \t\r>]*\|\|/;
const CODE_FENCE_LANGUAGE_TOKEN = /^[A-Za-z0-9_+.#/-]+$/;
const INVISIBLE_CODE_POINTS = new Set([
	0x0, 0x85, 0xad, 0x34f, 0x61c, 0x115f, 0x1160, 0x17b4, 0x17b5, 0x180e, 0x2800, 0x3164, 0xfeff, 0xffa0,
]);
const INVISIBLE_CODE_POINT_RANGES: ReadonlyArray<readonly [number, number]> = [
	[0x200b, 0x200f],
	[0x202a, 0x202e],
	[0x2060, 0x2069],
	[0xfe00, 0xfe0f],
	[0xe0100, 0xe01ef],
];

interface SanitizeCustomEmojisParams {
	content: string;
	userId: UserID | null;
	webhookId: WebhookID | null;
	guildId: GuildID | null;
	userRepository: EmojiUserRepository;
	guildRepository: EmojiGuildRepository;
	limitConfigService: LimitConfigService;
	hasPermission?: (permission: bigint) => Promise<boolean>;
	allowCodeBlocks?: boolean;
}

interface EmojiMatch {
	fullMatch: string;
	name: string;
	emojiId: EmojiID;
	start: number;
	end: number;
}

interface CodeBlock {
	start: number;
	end: number;
}

export async function sanitizeCustomEmojis(params: SanitizeCustomEmojisParams): Promise<string> {
	const {
		content,
		userId,
		webhookId,
		guildId,
		userRepository,
		guildRepository,
		limitConfigService,
		hasPermission,
		allowCodeBlocks = true,
	} = params;
	const escapedContexts = parseEscapedContexts(content, allowCodeBlocks);
	const isInEscapedContext = (index: number): boolean =>
		escapedContexts.some((ctx) => index >= ctx.start && index < ctx.end);
	const emojiMatches = collectEmojiMatches(content, isInEscapedContext);
	if (emojiMatches.length === 0) {
		return content;
	}
	const hasGlobalExpressions = userId
		? await checkUserGlobalExpressions(userId, userRepository, limitConfigService)
		: 0;
	const isWebhook = webhookId != null;
	const emojiLookups = await batchFetchEmojis(emojiMatches, guildId, guildRepository);
	let canUseExternalEmojis: boolean | null = null;
	if (hasGlobalExpressions > 0 && hasPermission && guildId) {
		const hasExternalEmojis = emojiLookups.some((lookup) => lookup.guildEmoji === null && lookup.globalEmoji !== null);
		if (hasExternalEmojis) {
			canUseExternalEmojis = await hasPermission(Permissions.USE_EXTERNAL_EMOJIS);
		}
	}
	const replacements = await determineReplacements({
		emojiMatches,
		emojiLookups,
		guildId,
		isWebhook,
		hasGlobalExpressions,
		canUseExternalEmojis,
	});
	return applyReplacements(content, replacements);
}

function parseEscapedContexts(content: string, allowCodeBlocks: boolean): Array<CodeBlock> {
	const contexts: Array<CodeBlock> = [];
	let index = 0;
	while (index < content.length) {
		const char = content[index];
		if (char === '\\') {
			if (content[index + 1] === '`') {
				const run = backtickRunEnd(content, index + 1) - (index + 1);
				index += 1 + (run >= CODE_FENCE_LENGTH ? run : 1);
			} else {
				index += 2;
			}
			continue;
		}
		if (char !== '`') {
			index++;
			continue;
		}
		const runEnd = backtickRunEnd(content, index);
		const runLength = runEnd - index;
		if (allowCodeBlocks && runLength >= CODE_FENCE_LENGTH) {
			const block = findCodeBlock(content, index, runLength);
			if (block === 'unresolved') break;
			if (block !== null) {
				contexts.push(block);
				index = block.end;
				continue;
			}
		}
		const end = findInlineCodeEnd(content, index, runLength);
		if (end !== null) {
			contexts.push({start: index, end});
			index = end;
		} else {
			index = runEnd;
		}
	}
	return contexts;
}

function backtickRunEnd(content: string, start: number): number {
	let end = start;
	while (content[end] === '`') end++;
	return end;
}

function isLineBreak(char: string | undefined): boolean {
	return char === '\n' || char === '\r';
}

function findInlineCodeEnd(content: string, opener: number, runLength: number): number | null {
	let index = opener + runLength;
	while (index < content.length) {
		const char = content[index];
		if (isLineBreak(char)) return null;
		if (runLength === 1 && char === '\\') {
			if (isLineBreak(content[index + 1])) return null;
			index += 2;
			continue;
		}
		if (char === '`') {
			const closerEnd = backtickRunEnd(content, index);
			const isCloser = runLength === 1 ? closerEnd - index === 1 : closerEnd - index >= runLength;
			if (isCloser) {
				return Buffer.byteLength(content.slice(opener, index)) <= MAX_LINE_LENGTH ? closerEnd : null;
			}
			index = closerEnd;
			continue;
		}
		index++;
	}
	return null;
}

function findCodeBlock(content: string, opener: number, fenceLength: number): CodeBlock | null | 'unresolved' {
	const lineStart = content.lastIndexOf('\n', opener - 1) + 1;
	const prefix = content.slice(lineStart, opener);
	if (prefix.includes('`')) return null;
	const fence = '`'.repeat(fenceLength);
	const bodyStart = opener + fenceLength;
	const openerLineEnd = lineEndIndex(content, bodyStart);
	const info = content.slice(bodyStart, openerLineEnd);
	const sameLineClose = info.indexOf(fence);
	if (sameLineClose !== -1) {
		const closeIndex = bodyStart + sameLineClose;
		return hasVisibleContent(content.slice(bodyStart, closeIndex))
			? {start: opener, end: closeIndex + fenceLength}
			: null;
	}
	const previousLine =
		lineStart === 0 ? null : content.slice(content.lastIndexOf('\n', lineStart - 2) + 1, lineStart - 1);
	const scope = fenceScope(prefix, previousLine);
	if (scope !== 'block' && scope !== 'quote') return scope === 'line' ? null : scope;
	let bodyVisible = false;
	let lineIndex = openerLineEnd + 1;
	for (let searched = 0; lineIndex <= content.length && searched <= MAX_CODE_BLOCK_SEARCH_LINES; searched++) {
		const lineEnd = lineEndIndex(content, lineIndex);
		const rawLine = content.slice(lineIndex, lineEnd);
		const line = scope === 'quote' ? quotedLine(rawLine) : rawLine;
		if (line === null) return null;
		const trimmed = trimLineStart(line);
		const fenceIndex = trimmed.indexOf(fence);
		if (fenceIndex !== -1) {
			const runEnd = backtickRunEnd(trimmed, fenceIndex);
			const after = trimmed[runEnd];
			if (after === undefined || after === ' ' || after === '\t' || trimmed.slice(runEnd).includes(fence)) {
				if (mayBeTableOrSpoiler(content, lineStart, openerLineEnd)) return 'unresolved';
				bodyVisible ||= hasVisibleContent(line.slice(0, line.indexOf(fence)));
				const closeIndex = lineIndex + (rawLine.length - trimmed.length) + fenceIndex;
				if (!bodyVisible) {
					const language = codeFenceLanguage(info);
					if (language === 'unknown') return 'unresolved';
					if (language === 'language' || !hasVisibleContent(info)) return null;
				}
				return {start: openerLineEnd, end: closeIndex + fenceLength};
			}
		}
		bodyVisible ||= hasVisibleContent(line);
		lineIndex = lineEnd + 1;
	}
	return null;
}

function mayBeTableOrSpoiler(content: string, lineStart: number, openerLineEnd: number): boolean {
	return (
		content.slice(lineStart, openerLineEnd).includes('|') || SPOILER_LINE_START.test(content.slice(0, openerLineEnd))
	);
}

function fenceScope(prefix: string, previousLine: string | null): 'block' | 'quote' | 'line' | 'unresolved' {
	const trimmed = trimLineStart(prefix);
	if (trimmed.startsWith('>>> ')) {
		const rest = trimmed.slice(4);
		return LIST_ITEM_PREFIX.test(rest) || trimLineStart(rest).startsWith('>') ? 'unresolved' : 'block';
	}
	if (LIST_ITEM_PREFIX.test(prefix)) return 'line';
	if (trimmed.startsWith('> ')) {
		const quoted = trimmed.slice(2);
		if (LIST_ITEM_PREFIX.test(quoted)) return 'line';
		if (trimLineStart(quoted).startsWith('>')) return 'unresolved';
		const previousQuoted = previousLine === null ? null : quotedLine(previousLine);
		return mayContinueList(quoted, previousQuoted) ? 'unresolved' : 'quote';
	}
	return mayContinueList(prefix, previousLine) ? 'unresolved' : 'block';
}

function mayContinueList(prefix: string, previousLine: string | null): boolean {
	if (!prefix.startsWith(' ') || previousLine === null) return false;
	if (LIST_ITEM_PREFIX.test(previousLine)) return trimLineStart(prefix) !== '';
	return previousLine.startsWith(' ') || trimLineStart(previousLine).startsWith('- ');
}

function codeFenceLanguage(info: string): 'none' | 'language' | 'text' | 'unknown' {
	if (info === '') return 'none';
	if (/^[\0\t\n\r ]/.test(info)) return 'text';
	const trimmed = info.replace(/[ \t\r\n]+$/, '');
	const primary = trimmed.split(/[ \t]/)[0];
	if (!CODE_FENCE_LANGUAGE_TOKEN.test(primary)) return 'text';
	return primary.length === trimmed.length ? 'language' : 'unknown';
}

function quotedLine(line: string): string | null {
	const trimmed = trimLineStart(line);
	return trimmed.startsWith('> ') ? trimmed.slice(2) : null;
}

function trimLineStart(value: string): string {
	return value.replace(/^[ \t\r\n]+/, '');
}

function hasVisibleContent(value: string): boolean {
	for (const char of value) {
		const codePoint = char.codePointAt(0)!;
		if (/\s/.test(char) || INVISIBLE_CODE_POINTS.has(codePoint)) continue;
		if (INVISIBLE_CODE_POINT_RANGES.some(([first, last]) => codePoint >= first && codePoint <= last)) continue;
		return true;
	}
	return false;
}

function lineEndIndex(content: string, from: number): number {
	const end = content.indexOf('\n', from);
	return end === -1 ? content.length : end;
}

function collectEmojiMatches(content: string, isInEscapedContext: (index: number) => boolean): Array<EmojiMatch> {
	const matches: Array<EmojiMatch> = [];
	CUSTOM_EMOJI_MARKDOWN_REGEX_GLOBAL.lastIndex = 0;
	let match: RegExpExecArray | null;
	while ((match = CUSTOM_EMOJI_MARKDOWN_REGEX_GLOBAL.exec(content)) !== null) {
		if (isInEscapedContext(match.index)) continue;
		const [fullMatch, , name, emojiId] = match;
		matches.push({
			fullMatch,
			name,
			emojiId: createEmojiID(BigInt(emojiId)),
			start: match.index,
			end: match.index + fullMatch.length,
		});
	}
	return matches;
}

export async function checkUserGlobalExpressions(
	userId: UserID,
	userRepository: EmojiUserRepository,
	limitConfigService: LimitConfigService,
): Promise<number> {
	const user = await userRepository.findUnique(userId);
	const ctx = createLimitMatchContext({user});
	return resolveLimitSafe(limitConfigService.getConfigSnapshot(), ctx, 'feature_global_expressions', 0);
}

interface EmojiLookupResult {
	emojiId: EmojiID;
	guildEmoji: GuildEmoji | null;
	globalEmoji: GuildEmoji | null;
}

async function batchFetchEmojis(
	matches: Array<EmojiMatch>,
	guildId: GuildID | null,
	guildRepository: EmojiGuildRepository,
): Promise<Array<EmojiLookupResult>> {
	const uniqueEmojiIds = [...new Set(matches.map((m) => m.emojiId))];
	const lookupResults = await Promise.all(
		uniqueEmojiIds.map(async (emojiId) => {
			const [guildEmoji, globalEmoji] = await Promise.all([
				guildId ? guildRepository.getEmoji(emojiId, guildId) : Promise.resolve(null),
				guildRepository.getEmojiById(emojiId),
			]);
			return {emojiId, guildEmoji, globalEmoji};
		}),
	);
	const lookupMap = new Map<EmojiID, EmojiLookupResult>();
	for (const result of lookupResults) {
		lookupMap.set(result.emojiId, result);
	}
	return matches.map((match) => lookupMap.get(match.emojiId)!);
}

interface Replacement {
	start: number;
	end: number;
	replacement: string;
}

async function determineReplacements(params: {
	emojiMatches: Array<EmojiMatch>;
	emojiLookups: Array<EmojiLookupResult>;
	guildId: GuildID | null;
	isWebhook: boolean;
	hasGlobalExpressions: number;
	canUseExternalEmojis: boolean | null;
}): Promise<Array<Replacement>> {
	const {emojiMatches, emojiLookups, guildId, isWebhook, hasGlobalExpressions, canUseExternalEmojis} = params;
	const replacements: Array<Replacement> = [];
	for (let i = 0; i < emojiMatches.length; i++) {
		const match = emojiMatches[i];
		const lookup = emojiLookups[i];
		const shouldReplace = await shouldReplaceEmoji({
			lookup,
			guildId,
			isWebhook,
			hasGlobalExpressions,
			canUseExternalEmojis,
		});
		if (shouldReplace) {
			replacements.push({
				start: match.start,
				end: match.end,
				replacement: `:${match.name}:`,
			});
		}
	}
	return replacements;
}

async function shouldReplaceEmoji(params: {
	lookup: EmojiLookupResult;
	guildId: GuildID | null;
	isWebhook: boolean;
	hasGlobalExpressions: number;
	canUseExternalEmojis: boolean | null;
}): Promise<boolean> {
	const {lookup, guildId, isWebhook, hasGlobalExpressions, canUseExternalEmojis} = params;
	if (!guildId) {
		if (!lookup.globalEmoji) return true;
		if (!isWebhook && hasGlobalExpressions === 0) return true;
		return false;
	}
	if (lookup.guildEmoji) {
		return false;
	}
	if (!lookup.globalEmoji) return true;
	if (!isWebhook && hasGlobalExpressions === 0) return true;
	if (hasGlobalExpressions > 0 && canUseExternalEmojis === false) return true;
	return false;
}

function applyReplacements(content: string, replacements: Array<Replacement>): string {
	if (replacements.length === 0) return content;
	const sorted = [...replacements].sort((a, b) => b.start - a.start);
	let result = content;
	for (const {start, end, replacement} of sorted) {
		result = result.substring(0, start) + replacement + result.substring(end);
	}
	return result;
}
