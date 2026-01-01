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

import {createEmojiID, type EmojiID, type GuildID, type UserID, type WebhookID} from '~/BrandedTypes';
import {Permissions} from '~/Constants';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildEmoji} from '~/Models';
import type {PackExpressionAccessResolution, PackExpressionAccessResolver} from '~/pack/PackExpressionAccessResolver';
import type {IUserAccountRepository} from '~/user/repositories/IUserAccountRepository';

type EmojiGuildRepository = Pick<IGuildRepository, 'getEmoji' | 'getEmojiById'>;
type EmojiUserRepository = Pick<IUserAccountRepository, 'findUnique'>;

const CUSTOM_EMOJI_MARKDOWN_REGEX = /<(a)?:([^:]+):(\d+)>/g;

interface SanitizeCustomEmojisParams {
	content: string;
	userId: UserID | null;
	webhookId: WebhookID | null;
	guildId: GuildID | null;
	userRepository: EmojiUserRepository;
	guildRepository: EmojiGuildRepository;
	hasPermission?: (permission: bigint) => Promise<boolean>;
	packResolver?: PackExpressionAccessResolver;
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
	const {content, userId, webhookId, guildId, userRepository, guildRepository, hasPermission, packResolver} = params;

	const escapedContexts = parseEscapedContexts(content);
	const isInEscapedContext = (index: number): boolean =>
		escapedContexts.some((ctx) => index >= ctx.start && index < ctx.end);

	const emojiMatches = collectEmojiMatches(content, isInEscapedContext);
	if (emojiMatches.length === 0) {
		return content;
	}

	const isPremium = userId ? await checkUserPremium(userId, userRepository) : false;
	const isWebhook = webhookId != null;
	const shouldSanitizeForRegularUser = userId != null && !isWebhook && !isPremium;

	if (shouldSanitizeForRegularUser) {
		return applyReplacements(
			content,
			emojiMatches.map((match) => ({
				start: match.start,
				end: match.end,
				replacement: `:${match.name}:`,
			})),
		);
	}

	const emojiLookups = await batchFetchEmojis(emojiMatches, guildId, guildRepository);

	let canUseExternalEmojis: boolean | null = null;
	if (isPremium && hasPermission && guildId) {
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
		isPremium,
		canUseExternalEmojis,
		packResolver,
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
	const emojiRegex = new RegExp(CUSTOM_EMOJI_MARKDOWN_REGEX.source, 'g');

	let match: RegExpExecArray | null;
	while ((match = emojiRegex.exec(content)) !== null) {
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

async function checkUserPremium(userId: UserID, userRepository: EmojiUserRepository): Promise<boolean> {
	const user = await userRepository.findUnique(userId);
	return user?.canUseGlobalExpressions() ?? false;
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
	isPremium: boolean;
	canUseExternalEmojis: boolean | null;
	packResolver?: PackExpressionAccessResolver;
}): Promise<Array<Replacement>> {
	const {emojiMatches, emojiLookups, guildId, isWebhook, isPremium, canUseExternalEmojis} = params;
	const replacements: Array<Replacement> = [];

	for (let i = 0; i < emojiMatches.length; i++) {
		const match = emojiMatches[i];
		const lookup = emojiLookups[i];
		const shouldReplace = await shouldReplaceEmoji({
			lookup,
			guildId,
			isWebhook,
			isPremium,
			canUseExternalEmojis,
			packResolver: params.packResolver,
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
	isPremium: boolean;
	canUseExternalEmojis: boolean | null;
	packResolver?: PackExpressionAccessResolver;
}): Promise<boolean> {
	const {lookup, guildId, isWebhook, isPremium, canUseExternalEmojis, packResolver} = params;

	if (!guildId) {
		if (!lookup.globalEmoji) return true;
		if (!isWebhook && !isPremium) return true;
		return false;
	}

	if (lookup.guildEmoji) {
		return false;
	}

	if (!lookup.globalEmoji) return true;

	const packAccess = await resolvePackAccessStatus(lookup.globalEmoji.guildId, packResolver);
	if (packAccess === 'not-accessible') {
		return true;
	}

	if (!isWebhook && !isPremium) return true;

	if (isPremium && canUseExternalEmojis === false) return true;

	return false;
}

async function resolvePackAccessStatus(
	packId: GuildID,
	packResolver?: PackExpressionAccessResolver,
): Promise<PackExpressionAccessResolution> {
	if (!packResolver) return 'not-pack';
	return await packResolver.resolve(packId);
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
