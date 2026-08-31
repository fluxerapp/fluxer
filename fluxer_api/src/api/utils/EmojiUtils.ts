// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {EmojiID, GuildID, UserID, WebhookID} from '../BrandedTypes';
import {createEmojiID} from '../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {resolveLimitSafe} from '../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import type {GuildEmoji} from '../models/GuildEmoji';
import type {IUserAccountRepository} from '../user/repositories/IUserAccountRepository';

type EmojiGuildRepository = Pick<IGuildRepositoryAggregate, 'getEmoji' | 'getEmojiById'>;
type EmojiUserRepository = Pick<IUserAccountRepository, 'findUnique'>;

const CUSTOM_EMOJI_MARKDOWN_REGEX = /<(a)?:([^:]+):(\d+)>/g;
const CUSTOM_EMOJI_MARKDOWN_REGEX_GLOBAL = new RegExp(CUSTOM_EMOJI_MARKDOWN_REGEX.source, 'g');

interface SanitizeCustomEmojisParams {
	content: string;
	userId: UserID | null;
	webhookId: WebhookID | null;
	guildId: GuildID | null;
	userRepository: EmojiUserRepository;
	guildRepository: EmojiGuildRepository;
	limitConfigService: LimitConfigService;
	hasPermission?: (permission: bigint) => Promise<boolean>;
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
	const {content, userId, webhookId, guildId, userRepository, guildRepository, limitConfigService, hasPermission} =
		params;
	const escapedContexts = parseEscapedContexts(content);
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

function parseEscapedContexts(content: string): Array<CodeBlock> {
	const contexts: Array<CodeBlock> = [];
	const blockCodeRegex = /```[\s\S]*?```/g;
	let match: RegExpExecArray | null;
	while ((match = blockCodeRegex.exec(content)) !== null) {
		contexts.push({start: match.index, end: match.index + match[0].length});
	}
	const inlineCodeRegex = /`[^`]+`/g;
	while ((match = inlineCodeRegex.exec(content)) !== null) {
		const isInsideBlock = contexts.some((ctx) => match!.index >= ctx.start && match!.index < ctx.end);
		if (!isInsideBlock) {
			contexts.push({start: match.index, end: match.index + match[0].length});
		}
	}
	return contexts;
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

async function checkUserGlobalExpressions(
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
