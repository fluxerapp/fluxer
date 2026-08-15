// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AutocompleteOption} from '@app/features/channel/components/AutocompleteTypes';
import type {Channel} from '@app/features/channel/models/Channel';
import Emoji from '@app/features/emoji/state/Emoji';
import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import MemesPicker from '@app/features/emoji/state/MemesPicker';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {
	filterEmojisForAutocomplete,
	filterStickersForAutocomplete,
} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {matchSorter} from 'match-sorter';

export const MEMBER_SEARCH_LIMIT = 25;
export const MENTION_RESULT_LIMIT = 10;

function firstDisplayText(...values: Array<string | null | undefined>): string {
	for (const value of values) {
		if (typeof value === 'string' && value.length > 0) {
			return value;
		}
	}
	return '';
}

function toLowerSearchText(value: string | null | undefined): string {
	return typeof value === 'string' ? value.toLowerCase() : '';
}

export function getMemberDisplayName(member: GuildMember): string {
	return firstDisplayText(
		DisplayNameUtils.getUntruncatedGuildMemberNickname(member),
		member.user.username,
		member.user.id,
	);
}

export function getUserDisplayName(user: User): string {
	return firstDisplayText(user.displayName, user.username, user.id);
}

export interface ParsedMentionQuery {
	usernameQuery: string;
	tagQuery: string | null;
	hasTagSeparator: boolean;
}

export function parseMentionQuery(query: string): ParsedMentionQuery {
	const hashIndex = query.indexOf('#');
	if (hashIndex === -1) {
		return {
			usernameQuery: query,
			tagQuery: null,
			hasTagSeparator: false,
		};
	}
	return {
		usernameQuery: query.slice(0, hashIndex),
		tagQuery: query.slice(hashIndex + 1),
		hasTagSeparator: true,
	};
}

export function filterDMUsers(
	users: Array<User>,
	parsedQuery: ParsedMentionQuery,
): Array<{
	type: 'mention';
	kind: 'user';
	user: User;
}> {
	const trimmedUsername = parsedQuery.usernameQuery.trim();
	const limit = MENTION_RESULT_LIMIT;
	let matchedUsers: typeof users;
	if (parsedQuery.hasTagSeparator) {
		const usernameQueryLower = parsedQuery.usernameQuery.toLowerCase();
		const tagQueryLower = parsedQuery.tagQuery === null ? '' : parsedQuery.tagQuery.toLowerCase();
		matchedUsers = users.filter((user) => {
			const username = toLowerSearchText(user.username);
			const display = toLowerSearchText(getUserDisplayName(user));
			const discriminator = firstDisplayText(user.discriminator).toLowerCase();
			const matchesUsername =
				usernameQueryLower.length === 0 ||
				username.startsWith(usernameQueryLower) ||
				display.startsWith(usernameQueryLower);
			const matchesTag = tagQueryLower.length === 0 || discriminator.startsWith(tagQueryLower);
			return matchesUsername && matchesTag;
		});
	} else if (trimmedUsername.length === 0) {
		matchedUsers = users;
	} else {
		matchedUsers = matchSorter(users, trimmedUsername, {
			keys: [(user) => getUserDisplayName(user), 'username', 'tag'],
		});
	}
	const sorted = [...matchedUsers].sort((a, b) =>
		getUserDisplayName(a).toLowerCase().localeCompare(getUserDisplayName(b).toLowerCase()),
	);
	return sorted.slice(0, limit).map((user) => ({
		type: 'mention' as const,
		kind: 'user' as const,
		user,
	}));
}

function matchGuildMembers(membersToUse: Array<GuildMember>, parsedQuery: ParsedMentionQuery): Array<GuildMember> {
	const trimmedUsername = parsedQuery.usernameQuery.trim();
	let matchedMembers: Array<GuildMember>;
	if (parsedQuery.hasTagSeparator) {
		const usernameQueryLower = parsedQuery.usernameQuery.toLowerCase();
		const tagQueryLower = parsedQuery.tagQuery === null ? '' : parsedQuery.tagQuery.toLowerCase();
		matchedMembers = membersToUse.filter((member) => {
			const nick = toLowerSearchText(member.nick);
			const username = toLowerSearchText(member.user.username);
			const display = toLowerSearchText(getMemberDisplayName(member));
			const discriminator = firstDisplayText(member.user.discriminator).toLowerCase();
			const matchesUsername =
				usernameQueryLower.length === 0 ||
				username.startsWith(usernameQueryLower) ||
				display.startsWith(usernameQueryLower) ||
				nick.startsWith(usernameQueryLower);
			const matchesTag = tagQueryLower.length === 0 || discriminator.startsWith(tagQueryLower);
			return matchesUsername && matchesTag;
		});
	} else if (trimmedUsername.length === 0) {
		matchedMembers = membersToUse;
	} else {
		matchedMembers = matchSorter(membersToUse, trimmedUsername, {
			keys: [(member) => getMemberDisplayName(member), 'nick', 'user.globalName', 'user.username', 'user.tag'],
		});
	}
	return matchedMembers;
}

export function searchGuildMembers(
	membersToUse: Array<GuildMember>,
	parsedQuery: ParsedMentionQuery,
	limit: number,
): Array<GuildMember> {
	const matchedMembers = matchGuildMembers(membersToUse, parsedQuery);
	return [...matchedMembers]
		.sort((a, b) => getMemberDisplayName(a).toLowerCase().localeCompare(getMemberDisplayName(b).toLowerCase()))
		.slice(0, limit);
}

export function filterGuildMembers(
	membersToUse: Array<GuildMember>,
	parsedQuery: ParsedMentionQuery,
	shouldCheckAccess: boolean,
	canViewChannel: (userId: string) => boolean,
	stableOrder?: Map<string, number>,
): Array<{
	type: 'mention';
	kind: 'member';
	member: GuildMember;
}> {
	const filteredByAccess = shouldCheckAccess
		? membersToUse.filter((member) => canViewChannel(member.user.id))
		: membersToUse;
	const matchedMembers = matchGuildMembers(filteredByAccess, parsedQuery);
	const limit = MENTION_RESULT_LIMIT;
	let sorted: Array<GuildMember>;
	if (stableOrder && stableOrder.size > 0) {
		const NEW_RANK = Number.MAX_SAFE_INTEGER;
		sorted = [...matchedMembers].sort((a, b) => {
			const aRank = stableOrder.get(a.user.id);
			const bRank = stableOrder.get(b.user.id);
			const ra = aRank === undefined ? NEW_RANK : aRank;
			const rb = bRank === undefined ? NEW_RANK : bRank;
			if (ra !== rb) return ra - rb;
			return getMemberDisplayName(a).toLowerCase().localeCompare(getMemberDisplayName(b).toLowerCase());
		});
	} else {
		sorted = [...matchedMembers].sort((a, b) =>
			getMemberDisplayName(a).toLowerCase().localeCompare(getMemberDisplayName(b).toLowerCase()),
		);
	}
	return sorted.slice(0, limit).map((member) => ({
		type: 'mention' as const,
		kind: 'member' as const,
		member,
	}));
}

export const SPECIAL_MENTIONS: ReadonlyArray<{
	type: 'mention';
	kind: '@everyone' | '@here';
}> = [
	{type: 'mention' as const, kind: '@everyone' as const},
	{type: 'mention' as const, kind: '@here' as const},
];

export interface CommandArgContext {
	channel: Channel;
	commandName: string;
	matchedText: string | null;
	memberSearchResults: Array<GuildMember>;
	canManageUser: (otherUserId: string, permission: bigint) => boolean;
	canViewChannel: (userId: string) => boolean;
	stableOrder?: Map<string, number>;
}

export function buildCommandArgOptions(ctx: CommandArgContext): Array<AutocompleteOption> {
	const parsedQuery = parseMentionQuery(ctx.matchedText === null ? '' : ctx.matchedText.trim());
	if (!ctx.channel.guildId) {
		if (ctx.commandName !== 'msg') {
			return [];
		}
		const users = ctx.channel.recipientIds.map((id) => Users.getUser(id)).filter((user): user is User => user != null);
		return filterDMUsers(users, parsedQuery);
	}
	const membersToUse = ctx.memberSearchResults;
	if (ctx.commandName === 'ban' || ctx.commandName === 'kick') {
		const permission = ctx.commandName === 'kick' ? Permissions.KICK_MEMBERS : Permissions.BAN_MEMBERS;
		const manageableMembers = membersToUse.filter((member) => ctx.canManageUser(member.user.id, permission));
		return filterGuildMembers(manageableMembers, parsedQuery, false, ctx.canViewChannel, ctx.stableOrder);
	}
	return filterGuildMembers(membersToUse, parsedQuery, true, ctx.canViewChannel, ctx.stableOrder);
}

export interface EmojiReactionContext {
	channel: Channel | null;
	matchedText: string | null;
	i18n: I18n;
}

interface EmojiReactionOptionsCache {
	query: string;
	channelId: string | null;
	i18n: I18n;
	allEmojis: ReadonlyArray<FlatEmoji>;
	rankingVersion: number;
	options: Array<AutocompleteOption>;
}

let emojiReactionOptionsCache: EmojiReactionOptionsCache | null = null;

export function buildEmojiReactionOptions(ctx: EmojiReactionContext): Array<AutocompleteOption> {
	const query = ctx.matchedText === null ? '' : ctx.matchedText.trim();
	const channel = ctx.channel;
	const channelId = channel === null ? null : channel.id;
	const allUnfilteredEmojis = Emoji.getAllEmojis(channel);
	const ranking = EmojiPicker.getRanking();
	if (
		emojiReactionOptionsCache &&
		emojiReactionOptionsCache.query === query &&
		emojiReactionOptionsCache.channelId === channelId &&
		emojiReactionOptionsCache.i18n === ctx.i18n &&
		emojiReactionOptionsCache.allEmojis === allUnfilteredEmojis &&
		emojiReactionOptionsCache.rankingVersion === ranking.version
	) {
		return emojiReactionOptionsCache.options;
	}
	const hasQuery = query.length > 0;
	const allEmojis = hasQuery ? Emoji.search(channel, query, 10) : allUnfilteredEmojis;
	const filteredEmojis = filterEmojisForAutocomplete(ctx.i18n, allEmojis, channel);
	const emojis = hasQuery ? filteredEmojis : EmojiPicker.getFrecentEmojis(filteredEmojis, 10, ranking);
	const options = emojis.map((emoji) => ({
		type: 'emoji' as const,
		emoji,
	}));
	emojiReactionOptionsCache = {
		query,
		channelId,
		i18n: ctx.i18n,
		allEmojis: allUnfilteredEmojis,
		rankingVersion: ranking.version,
		options,
	};
	return options;
}

export interface EmojiPreferences {
	showDefaultEmojis: boolean;
	showCustomEmojis: boolean;
	showStickers: boolean;
	showMemes: boolean;
}

export interface EmojiAutocompleteContext {
	channel: Channel | null;
	matchedText: string | null;
	i18n: I18n;
	prefs: EmojiPreferences;
}

interface EmojiAutocompleteOptionsCache {
	query: string;
	channelId: string | null;
	i18n: I18n;
	allEmojis: ReadonlyArray<FlatEmoji>;
	showDefaultEmojis: boolean;
	showCustomEmojis: boolean;
	rankingVersion: number;
	options: Array<AutocompleteOption>;
}

let emojiAutocompleteOptionsCache: EmojiAutocompleteOptionsCache | null = null;

function buildEmojiAutocompleteEmojiOptions(
	ctx: EmojiAutocompleteContext,
	query: string,
	hasQuery: boolean,
): Array<AutocompleteOption> {
	const {showDefaultEmojis, showCustomEmojis} = ctx.prefs;
	const channel = ctx.channel;
	const channelId = channel === null ? null : channel.id;
	const allUnfilteredEmojis = Emoji.getAllEmojis(channel);
	const ranking = EmojiPicker.getRanking();
	if (
		emojiAutocompleteOptionsCache &&
		emojiAutocompleteOptionsCache.query === query &&
		emojiAutocompleteOptionsCache.channelId === channelId &&
		emojiAutocompleteOptionsCache.i18n === ctx.i18n &&
		emojiAutocompleteOptionsCache.allEmojis === allUnfilteredEmojis &&
		emojiAutocompleteOptionsCache.showDefaultEmojis === showDefaultEmojis &&
		emojiAutocompleteOptionsCache.showCustomEmojis === showCustomEmojis &&
		emojiAutocompleteOptionsCache.rankingVersion === ranking.version
	) {
		return emojiAutocompleteOptionsCache.options;
	}
	const allEmojis =
		showDefaultEmojis || showCustomEmojis ? (hasQuery ? Emoji.search(channel, query, 10) : allUnfilteredEmojis) : [];
	const permissionFiltered = filterEmojisForAutocomplete(ctx.i18n, allEmojis, channel);
	const filteredEmojis = permissionFiltered.filter((emoji) => {
		const isCustom = !!emoji.guildId;
		return (isCustom && showCustomEmojis) || (!isCustom && showDefaultEmojis);
	});
	const emojiResults =
		hasQuery || !(showDefaultEmojis || showCustomEmojis)
			? filteredEmojis
			: EmojiPicker.getFrecentEmojis(filteredEmojis, 5, ranking);
	const options: Array<AutocompleteOption> = emojiResults.map((emoji) => ({
		type: 'emoji' as const,
		emoji,
	}));
	emojiAutocompleteOptionsCache = {
		query,
		channelId,
		i18n: ctx.i18n,
		allEmojis: allUnfilteredEmojis,
		showDefaultEmojis,
		showCustomEmojis,
		rankingVersion: ranking.version,
		options,
	};
	return options;
}

export function buildEmojiAutocompleteOptions(ctx: EmojiAutocompleteContext): Array<AutocompleteOption> {
	const query = ctx.matchedText === null ? '' : ctx.matchedText.trim();
	const hasQuery = query.length > 0;
	const {showStickers, showMemes} = ctx.prefs;
	const emojiOptions = buildEmojiAutocompleteEmojiOptions(ctx, query, hasQuery);
	const allStickers = showStickers
		? Sticker.searchWithChannel(ctx.channel, hasQuery ? query : '').slice(0, hasQuery ? 5 : undefined)
		: [];
	const filteredStickers = filterStickersForAutocomplete(ctx.i18n, allStickers, ctx.channel);
	const stickerResults = hasQuery
		? filteredStickers
		: showStickers
			? StickerPicker.getFrecentStickers(filteredStickers, 3)
			: [];
	const stickerOptions: Array<AutocompleteOption> = stickerResults.map((sticker) => ({
		type: 'sticker' as const,
		sticker,
	}));
	let memeOptions: Array<AutocompleteOption>;
	if (showMemes) {
		const allMemes = FavoriteMemes.getAllMemes();
		if (hasQuery) {
			memeOptions = matchSorter(allMemes, query, {
				keys: ['name', 'altText', 'filename', 'tags'],
				threshold: matchSorter.rankings.CONTAINS,
			})
				.slice(0, 5)
				.map((meme) => ({type: 'meme' as const, meme}));
		} else {
			memeOptions = MemesPicker.getFrecentMemes(allMemes, 3).map((meme) => ({type: 'meme' as const, meme}));
		}
	} else {
		memeOptions = [];
	}
	return [...emojiOptions, ...stickerOptions, ...memeOptions];
}
