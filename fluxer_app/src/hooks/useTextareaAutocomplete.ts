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

import {useLingui} from '@lingui/react/macro';
import {matchSorter} from 'match-sorter';
import React from 'react';
import * as HighlightActionCreators from '~/actions/HighlightActionCreators';
import * as ReactionActionCreators from '~/actions/ReactionActionCreators';
import type {TenorGif} from '~/actions/TenorActionCreators';
import * as TenorActionCreators from '~/actions/TenorActionCreators';
import {ChannelTypes, Permissions} from '~/Constants';
import type {AutocompleteOption, AutocompleteType, Command} from '~/components/channel/Autocomplete';
import {
	COMMANDS,
	isChannel,
	isCommand,
	isEmoji,
	isGif,
	isMeme,
	isMentionMember,
	isMentionRole,
	isMentionUser,
	isSpecialMention,
	isSticker,
} from '~/components/channel/Autocomplete';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import type {UserRecord} from '~/records/UserRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import EmojiStore from '~/stores/EmojiStore';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import type {SearchContext} from '~/stores/MemberSearchStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import MemesPickerStore from '~/stores/MemesPickerStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import StickerPickerStore from '~/stores/StickerPickerStore';
import StickerStore from '~/stores/StickerStore';
import UserStore from '~/stores/UserStore';
import {filterEmojisForAutocomplete, filterStickersForAutocomplete} from '~/utils/ExpressionPermissionUtils';
import {toReactionEmoji} from '~/utils/ReactionUtils';
import {detectAutocompleteTrigger, filterCommandsByQuery, getCommandInsertionText} from '~/utils/SlashCommandUtils';
import * as TenorUtils from '~/utils/TenorUtils';
import type {MentionSegment} from '~/utils/TextareaSegmentManager';

interface UseTextareaAutocompleteReturn {
	autocompleteOptions: Array<AutocompleteOption>;
	autocompleteType: AutocompleteType;
	selectedIndex: number;
	isAutocompleteAttached: boolean;
	setSelectedIndex: React.Dispatch<React.SetStateAction<number>>;
	onCursorMove: () => void;
	handleSelect: (option: AutocompleteOption) => void;
	autocompleteQuery: string;
	isMemberSearchLoading: boolean;
}

const MEMBER_SEARCH_LIMIT = 25;

interface ParsedMentionQuery {
	usernameQuery: string;
	tagQuery: string | null;
	hasTagSeparator: boolean;
}

function parseMentionQuery(query: string): ParsedMentionQuery {
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

function filterDMUsers(
	users: Array<UserRecord>,
	parsedQuery: ParsedMentionQuery,
): Array<{type: 'mention'; kind: 'user'; user: UserRecord}> {
	let matchedUsers: typeof users;
	if (parsedQuery.hasTagSeparator) {
		const usernameQueryLower = parsedQuery.usernameQuery.toLowerCase();
		const tagQueryLower = parsedQuery.tagQuery?.toLowerCase() ?? '';
		matchedUsers = users.filter(
			(user) =>
				user.username.toLowerCase().startsWith(usernameQueryLower) &&
				(tagQueryLower === '' || user.discriminator.startsWith(tagQueryLower)),
		);
	} else {
		matchedUsers = matchSorter(users, parsedQuery.usernameQuery, {
			keys: ['username', 'tag'],
		});
	}

	return matchedUsers
		.map((user) => ({
			type: 'mention' as const,
			kind: 'user' as const,
			user,
		}))
		.sort((a, b) => a.user.username.toLowerCase().localeCompare(b.user.username.toLowerCase()))
		.slice(0, 10);
}

function filterGuildMembers(
	membersToUse: Array<GuildMemberRecord>,
	parsedQuery: ParsedMentionQuery,
	shouldCheckAccess: boolean,
	canViewChannel: (userId: string) => boolean,
): Array<{type: 'mention'; kind: 'member'; member: GuildMemberRecord}> {
	let matchedMembers: typeof membersToUse;
	if (parsedQuery.hasTagSeparator) {
		const usernameQueryLower = parsedQuery.usernameQuery.toLowerCase();
		const tagQueryLower = parsedQuery.tagQuery?.toLowerCase() ?? '';
		matchedMembers = membersToUse.filter(
			(member) =>
				(member.user.username.toLowerCase().startsWith(usernameQueryLower) ||
					member.nick?.toLowerCase().startsWith(usernameQueryLower)) &&
				(tagQueryLower === '' || member.user.discriminator.startsWith(tagQueryLower)) &&
				(!shouldCheckAccess || canViewChannel(member.user.id)),
		);
	} else {
		const filteredByAccess = shouldCheckAccess
			? membersToUse.filter((member) => canViewChannel(member.user.id))
			: membersToUse;
		matchedMembers = matchSorter(filteredByAccess, parsedQuery.usernameQuery, {
			keys: ['nick', 'user.username', 'user.tag'],
		});
	}

	return matchedMembers
		.map((member) => ({
			type: 'mention' as const,
			kind: 'member' as const,
			member,
		}))
		.sort((a, b) => a.member.user.username.toLowerCase().localeCompare(b.member.user.username.toLowerCase()))
		.slice(0, 10);
}

export type TriggerType =
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

interface UseTextareaAutocompleteParams {
	channel: ChannelRecord | null;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	segmentManagerRef: React.MutableRefObject<{
		insertSegment: (
			currentText: string,
			insertPosition: number,
			displayText: string,
			actualText: string,
			type: MentionSegment['type'],
			id: string,
		) => {newText: string};
		updateSegmentsForTextChange: (changeStart: number, changeEnd: number, replacementLength: number) => void;
		setSegments: (segments: Array<MentionSegment>) => void;
		getSegments: () => Array<MentionSegment>;
	}>;
	previousValueRef: React.MutableRefObject<string>;
	allowedTriggers?: Array<TriggerType>;
}

export function useTextareaAutocomplete({
	channel,
	value,
	setValue,
	textareaRef,
	segmentManagerRef,
	previousValueRef,
	allowedTriggers,
}: UseTextareaAutocompleteParams): UseTextareaAutocompleteReturn {
	const {i18n} = useLingui();

	const [autocompleteOptions, setAutocompleteOptions] = React.useState<Array<AutocompleteOption>>([]);
	const [autocompleteType, setAutocompleteType] = React.useState<AutocompleteType>('mention');
	const [selectedIndex, setSelectedIndex] = React.useState(0);
	const [valueUpToCursor, setValueUpToCursor] = React.useState('');

	const [gifState, setGifState] = React.useState<{
		status: 'idle' | 'loading' | 'success' | 'error';
		query: string;
		results: Array<TenorGif>;
	}>({
		status: 'idle',
		query: '',
		results: [],
	});

	const [memberSearchResults, setMemberSearchResults] = React.useState<Array<GuildMemberRecord>>([]);
	const [isMemberSearchLoading, setIsMemberSearchLoading] = React.useState(false);

	const gifCacheRef = React.useRef<Map<string, Array<TenorGif>>>(new Map());
	const currentSearchRef = React.useRef<string | null>(null);
	const debounceTimerRef = React.useRef<NodeJS.Timeout | null>(null);
	const searchContextRef = React.useRef<SearchContext | null>(null);

	const autocompleteTrigger = React.useMemo(() => {
		const trigger = detectAutocompleteTrigger(valueUpToCursor);
		if (!trigger) return null;

		if (!channel && trigger.type !== 'emoji') {
			return null;
		}

		if (allowedTriggers && !allowedTriggers.includes(trigger.type)) {
			return null;
		}
		return trigger;
	}, [valueUpToCursor, allowedTriggers, channel]);

	React.useEffect(() => {
		if (debounceTimerRef.current) {
			clearTimeout(debounceTimerRef.current);
		}

		if (autocompleteTrigger?.type === 'gif') {
			const searchQuery = (autocompleteTrigger.match[3] ?? '').trim();

			if (!searchQuery) {
				currentSearchRef.current = null;
				setGifState({status: 'idle', query: '', results: []});
				return;
			}

			if (currentSearchRef.current === searchQuery) {
				return;
			}

			const cachedResults = gifCacheRef.current.get(searchQuery);
			if (cachedResults) {
				currentSearchRef.current = searchQuery;
				setGifState({status: 'success', query: searchQuery, results: cachedResults});
				return;
			}

			debounceTimerRef.current = setTimeout(() => {
				currentSearchRef.current = searchQuery;
				setGifState({status: 'loading', query: searchQuery, results: []});

				TenorActionCreators.search(searchQuery)
					.then((gifs) => {
						gifCacheRef.current.set(searchQuery, gifs);
						setGifState({status: 'success', query: searchQuery, results: gifs});
					})
					.catch((error) => {
						console.error('[AUTOCOMPLETE] GIF search failed:', error);
						setGifState({status: 'error', query: searchQuery, results: []});
					});
			}, 300);
		} else {
			currentSearchRef.current = null;
			setGifState((prev) => {
				if (prev.status === 'idle') return prev;
				return {status: 'idle', query: '', results: []};
			});
		}

		return () => {
			if (debounceTimerRef.current) {
				clearTimeout(debounceTimerRef.current);
			}
		};
	}, [autocompleteTrigger]);

	const currentGuildIdRef = React.useRef<string | null>(null);
	const memberFetchDebounceTimerRef = React.useRef<NodeJS.Timeout | null>(null);

	React.useEffect(() => {
		const context = MemberSearchStore.getSearchContext((results) => {
			const guildId = currentGuildIdRef.current;
			const guildMemberRecords: Array<GuildMemberRecord> = results
				.map((transformed) => {
					if (guildId) {
						const member = GuildMemberStore.getMember(guildId, transformed.id);
						return member ?? null;
					}
					const guilds = GuildStore.getGuilds();
					for (const guild of guilds) {
						const member = GuildMemberStore.getMember(guild.id, transformed.id);
						if (member) {
							return member;
						}
					}
					return null;
				})
				.filter((m): m is GuildMemberRecord => m !== null);

			setMemberSearchResults(guildMemberRecords);
			setIsMemberSearchLoading(false);
		}, MEMBER_SEARCH_LIMIT);

		searchContextRef.current = context;

		return () => {
			context.destroy();
			searchContextRef.current = null;
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
		};
	}, []);

	React.useEffect(() => {
		const context = searchContextRef.current;
		if (!context) return;

		const isMentionTrigger =
			autocompleteTrigger?.type === 'mention' ||
			autocompleteTrigger?.type === 'commandArgMention' ||
			autocompleteTrigger?.type === 'commandArg';

		if (!isMentionTrigger || !channel?.guildId) {
			currentGuildIdRef.current = null;
			context.clearQuery();
			setMemberSearchResults([]);
			setIsMemberSearchLoading(false);
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
			return;
		}

		const searchQuery = autocompleteTrigger?.matchedText ?? '';
		const guildId = channel.guildId;
		const isGuildFullyLoaded = GuildMemberStore.isGuildFullyLoaded(guildId);

		currentGuildIdRef.current = guildId;

		const cachedMembers = GuildMemberStore.getMembers(guildId);
		if (cachedMembers.length > 0) {
			const cachedMatches = matchSorter(cachedMembers, searchQuery, {
				keys: ['nick', 'user.username', 'user.tag'],
			}).slice(0, MEMBER_SEARCH_LIMIT);
			setMemberSearchResults(cachedMatches);
		} else {
			setMemberSearchResults([]);
		}

		if (isGuildFullyLoaded) {
			context.clearQuery();
			setMemberSearchResults([]);
			setIsMemberSearchLoading(false);
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
			return;
		}

		setIsMemberSearchLoading(true);
		context.setQuery(searchQuery);

		if (memberFetchDebounceTimerRef.current) {
			clearTimeout(memberFetchDebounceTimerRef.current);
		}

		memberFetchDebounceTimerRef.current = setTimeout(() => {
			void MemberSearchStore.fetchMembersInBackground(searchQuery, [guildId]);
			memberFetchDebounceTimerRef.current = null;
		}, 300);
	}, [autocompleteTrigger, channel?.guildId]);

	const autocompleteQuery = React.useMemo(() => {
		if (!autocompleteTrigger) return '';
		switch (autocompleteTrigger.type) {
			case 'mention':
			case 'channel':
			case 'emoji':
			case 'emojiReaction':
			case 'command':
			case 'commandArg':
			case 'commandArgMention':
				return autocompleteTrigger.matchedText ?? '';
			case 'meme':
				return (autocompleteTrigger.match[2] ?? '').trim();
			case 'gif':
				return (autocompleteTrigger.match[3] ?? '').trim();
			case 'sticker':
				return (autocompleteTrigger.match[2] ?? '').trim();
			default:
				return '';
		}
	}, [autocompleteTrigger]);

	const hasOpenCodeBlock = React.useCallback(() => {
		const textarea = textareaRef.current;
		const match = textarea?.value.slice(0, textarea.selectionStart).match(/```/g);
		return match != null && match.length > 0 && match.length % 2 !== 0;
	}, [textareaRef]);

	const isAutocompleteAttached = !!autocompleteTrigger && autocompleteOptions.length > 0;

	const matchedText = autocompleteTrigger ? autocompleteTrigger.matchedText : null;

	const onCursorMove = React.useCallback(() => {
		const position = textareaRef.current?.selectionStart ?? 0;
		const beforeCursor = value.slice(0, position);
		setValueUpToCursor(beforeCursor);
	}, [value, textareaRef]);

	React.useEffect(() => {
		const handleSelectionChange = () => {
			onCursorMove();
		};

		document.addEventListener('selectionchange', handleSelectionChange);
		return () => {
			document.removeEventListener('selectionchange', handleSelectionChange);
		};
	}, [onCursorMove]);

	React.useEffect(() => {
		const position = textareaRef.current?.selectionStart ?? value.length;
		setValueUpToCursor(value.slice(0, position));
	}, [value, textareaRef]);

	React.useEffect(() => {
		if (isAutocompleteAttached) {
			if (autocompleteTrigger?.type === 'channel') {
				const firstChannel = autocompleteOptions.find(isChannel);
				if (firstChannel) {
					HighlightActionCreators.highlightChannel(firstChannel.channel.id);
				}
			}
		} else {
			HighlightActionCreators.clearChannelHighlight();
		}
		return () => {
			HighlightActionCreators.clearChannelHighlight();
		};
	}, [isAutocompleteAttached, autocompleteTrigger, autocompleteOptions]);

	const canMentionEveryone = channel ? PermissionStore.can(Permissions.MENTION_EVERYONE, channel) : false;

	const canUseCommand = React.useCallback(
		(command: Command) => {
			if (command.type === 'simple') return true;
			if (!channel) return false;
			if (command.requiresGuild && !channel.guildId) return false;
			if (command.permission) {
				return PermissionStore.can(command.permission, channel);
			}
			return true;
		},
		[channel],
	);

	const canManageUser = React.useCallback(
		(otherUserId: string, permission: bigint) => {
			if (!channel || !channel.guildId) return false;

			const currentUserId = AuthenticationStore.currentUserId;
			if (otherUserId === currentUserId) return false;

			if (!PermissionStore.can(permission, {guildId: channel.guildId})) {
				return false;
			}

			// TODO: Restore proper role hierarchy checking
			return true;
		},
		[channel],
	);

	const canViewChannel = React.useCallback(
		(_userId: string): boolean => {
			if (!channel) return false;

			if (
				channel.type === ChannelTypes.DM ||
				channel.type === ChannelTypes.DM_PERSONAL_NOTES ||
				channel.type === ChannelTypes.GROUP_DM
			) {
				return true;
			}

			if (!channel.guildId) return true;

			return PermissionStore.can(Permissions.VIEW_CHANNEL, {guildId: channel.guildId, channelId: channel.id});
		},
		[channel],
	);

	React.useEffect(() => {
		let options: Array<AutocompleteOption> = [];

		if (!autocompleteTrigger) {
			setAutocompleteOptions([]);
			return;
		}

		switch (autocompleteTrigger.type) {
			case 'commandArgMention': {
				setAutocompleteType('mention');
				const commandName = autocompleteTrigger.match[2];

				if (!channel || (!channel.guildId && commandName !== 'msg')) {
					setAutocompleteOptions([]);
					return;
				}

				const parsedQuery = parseMentionQuery(matchedText ?? '');

				if (commandName === 'msg') {
					if (!channel.guildId) {
						const users = channel.recipientIds
							.map((id) => UserStore.getUser(id))
							.filter((user): user is NonNullable<typeof user> => user != null);

						options = filterDMUsers(users, parsedQuery);
					} else {
						const membersToUse =
							memberSearchResults.length > 0 ? memberSearchResults : GuildMemberStore.getMembers(channel.guildId ?? '');

						options = filterGuildMembers(membersToUse, parsedQuery, true, canViewChannel);
					}
				} else {
					const permission = commandName === 'kick' ? Permissions.KICK_MEMBERS : Permissions.BAN_MEMBERS;

					const membersToUse =
						memberSearchResults.length > 0 ? memberSearchResults : GuildMemberStore.getMembers(channel.guildId ?? '');

					const filteredMembers = membersToUse.filter((member) => canManageUser(member.user.id, permission));

					options = filterGuildMembers(filteredMembers, parsedQuery, false, canViewChannel);
				}
				break;
			}

			case 'commandArg': {
				setAutocompleteType('mention');
				const commandName = autocompleteTrigger.match[2];

				if (!channel || (!channel.guildId && commandName !== 'msg')) {
					setAutocompleteOptions([]);
					return;
				}

				const parsedQuery = parseMentionQuery(matchedText ?? '');

				if (commandName === 'msg') {
					if (!channel.guildId) {
						const users = channel.recipientIds
							.map((id) => UserStore.getUser(id))
							.filter((user): user is NonNullable<typeof user> => user != null);

						options = filterDMUsers(users, parsedQuery);
					} else {
						const membersToUse =
							memberSearchResults.length > 0 ? memberSearchResults : GuildMemberStore.getMembers(channel.guildId ?? '');

						options = filterGuildMembers(membersToUse, parsedQuery, true, canViewChannel);
					}
				} else {
					const permission = commandName === 'kick' ? Permissions.KICK_MEMBERS : Permissions.BAN_MEMBERS;

					const membersToUse =
						memberSearchResults.length > 0 ? memberSearchResults : GuildMemberStore.getMembers(channel.guildId ?? '');

					const filteredMembers = membersToUse.filter((member) => canManageUser(member.user.id, permission));

					options = filterGuildMembers(filteredMembers, parsedQuery, false, canViewChannel);
				}
				break;
			}

			case 'mention': {
				setAutocompleteType('mention');

				if (!channel || !channel.guildId) {
					if (!channel) {
						setAutocompleteOptions([]);
						return;
					}
					const users = channel.recipientIds
						.map((id) => UserStore.getUser(id))
						.filter((user): user is NonNullable<typeof user> => user != null);

					const parsedQuery = parseMentionQuery(matchedText ?? '');

					const userOptions = filterDMUsers(users, parsedQuery);

					const specialMentions: Array<{type: 'mention'; kind: '@everyone' | '@here'}> = [
						{type: 'mention' as const, kind: '@everyone' as const},
						{type: 'mention' as const, kind: '@here' as const},
					];

					options = [...userOptions, ...specialMentions];
				} else {
					const membersToUse =
						memberSearchResults.length > 0 ? memberSearchResults : GuildMemberStore.getMembers(channel.guildId ?? '');

					const parsedQuery = parseMentionQuery(matchedText ?? '');

					const members = filterGuildMembers(membersToUse, parsedQuery, true, canViewChannel);

					const roles = GuildStore.getGuildRoles(channel.guildId ?? '')
						.filter((role) => canMentionEveryone || role.mentionable)
						.map((role) => ({
							type: 'mention' as const,
							kind: 'role' as const,
							role,
						}))
						.sort((a, b) => b.role.position - a.role.position)
						.slice(0, 10);

					const specialMentions: Array<{type: 'mention'; kind: '@everyone' | '@here'}> = [
						{type: 'mention' as const, kind: '@everyone' as const},
						{type: 'mention' as const, kind: '@here' as const},
					];

					options = [...members, ...(canMentionEveryone ? specialMentions : []), ...roles];
				}
				break;
			}

			case 'channel': {
				setAutocompleteType('channel');
				if (!channel) {
					setAutocompleteOptions([]);
					return;
				}
				options = matchSorter(ChannelStore.getGuildChannels(channel.guildId ?? ''), matchedText ?? '', {keys: ['name']})
					.filter((channel) => !channel.isGuildCategory())
					.map((channel) => ({
						type: 'channel' as const,
						channel,
					}))
					.sort((a, b) => a.channel.position! - b.channel.position!)
					.slice(0, 10);
				break;
			}

			case 'emojiReaction': {
				setAutocompleteType('emoji');

				if (!matchedText || matchedText.trim() === '') {
					const allEmojis = EmojiStore.search(channel ?? null, '');
					const filteredEmojis = filterEmojisForAutocomplete(i18n, allEmojis, channel ?? null);
					const frecentEmojis = EmojiPickerStore.getFrecentEmojis(filteredEmojis, 10);

					options = frecentEmojis.map((emoji) => ({
						type: 'emoji' as const,
						emoji,
					}));
				} else {
					const allEmojisSearch = EmojiStore.search(channel ?? null, matchedText ?? '', 10);
					const filteredEmojisSearch = filterEmojisForAutocomplete(i18n, allEmojisSearch, channel ?? null);

					options = filteredEmojisSearch.map((emoji) => ({
						type: 'emoji' as const,
						emoji,
					}));
				}
				break;
			}

			case 'emoji': {
				setAutocompleteType('emoji');

				const showDefaultEmojis = AccessibilityStore.showDefaultEmojisInExpressionAutocomplete;
				const showCustomEmojis = AccessibilityStore.showCustomEmojisInExpressionAutocomplete;
				const showStickers = AccessibilityStore.showStickersInExpressionAutocomplete;
				const showMemes = AccessibilityStore.showMemesInExpressionAutocomplete;

				if (!matchedText || matchedText.trim() === '') {
					const allEmojis = showDefaultEmojis || showCustomEmojis ? EmojiStore.search(channel ?? null, '') : [];
					const permissionFilteredEmojis = filterEmojisForAutocomplete(i18n, allEmojis, channel ?? null);
					const filteredEmojis = permissionFilteredEmojis.filter((emoji) => {
						const isCustom = !!emoji.guildId;
						return (isCustom && showCustomEmojis) || (!isCustom && showDefaultEmojis);
					});
					const allStickers = showStickers ? StickerStore.searchWithChannel(channel ?? null, '') : [];
					const filteredStickers = filterStickersForAutocomplete(i18n, allStickers, channel ?? null);
					const allMemes = showMemes ? FavoriteMemeStore.getAllMemes() : [];

					const frecentEmojis =
						showDefaultEmojis || showCustomEmojis ? EmojiPickerStore.getFrecentEmojis(filteredEmojis, 5) : [];
					const frecentStickers = showStickers ? StickerPickerStore.getFrecentStickers(filteredStickers, 3) : [];
					const frecentMemes = showMemes ? MemesPickerStore.getFrecentMemes(allMemes, 3) : [];

					const emojiOptions = frecentEmojis.map((emoji) => ({
						type: 'emoji' as const,
						emoji,
					}));

					const stickerOptions = frecentStickers.map((sticker) => ({
						type: 'sticker' as const,
						sticker,
					}));

					const memeOptions = frecentMemes.map((meme) => ({
						type: 'meme' as const,
						meme,
					}));

					options = [...emojiOptions, ...stickerOptions, ...memeOptions];
				} else {
					const allEmojisSearch =
						showDefaultEmojis || showCustomEmojis ? EmojiStore.search(channel ?? null, matchedText ?? '', 10) : [];
					const permissionFilteredEmojisSearch = filterEmojisForAutocomplete(i18n, allEmojisSearch, channel ?? null);
					const filteredEmojisSearch = permissionFilteredEmojisSearch.filter((emoji) => {
						const isCustom = !!emoji.guildId;
						return (isCustom && showCustomEmojis) || (!isCustom && showDefaultEmojis);
					});

					const emojiOptions = filteredEmojisSearch.map((emoji) => ({
						type: 'emoji' as const,
						emoji,
					}));

					const allStickersSearch = showStickers
						? StickerStore.searchWithChannel(channel ?? null, matchedText ?? '').slice(0, 5)
						: [];
					const filteredStickersSearch = filterStickersForAutocomplete(i18n, allStickersSearch, channel ?? null);

					const stickerOptions = filteredStickersSearch.map((sticker) => ({
						type: 'sticker' as const,
						sticker,
					}));

					const memeOptions = showMemes
						? matchSorter(FavoriteMemeStore.getAllMemes(), matchedText ?? '', {
								keys: ['name', 'altText', 'filename', 'tags'],
								threshold: matchSorter.rankings.CONTAINS,
							})
								.slice(0, 5)
								.map((meme) => ({
									type: 'meme' as const,
									meme,
								}))
						: [];

					options = [...emojiOptions, ...stickerOptions, ...memeOptions];
				}
				break;
			}

			case 'command': {
				setAutocompleteType('command');
				const filteredCommands = filterCommandsByQuery(COMMANDS, matchedText ?? '').filter(canUseCommand);
				options = filteredCommands.map((command) => ({
					type: 'command' as const,
					command,
				}));
				break;
			}

			case 'meme': {
				setAutocompleteType('meme');
				const searchQuery = autocompleteTrigger.match[2].trim();
				const allMemes = FavoriteMemeStore.getAllMemes();
				if (searchQuery) {
					const filteredMemes = matchSorter(allMemes, searchQuery, {
						keys: ['name', 'altText', 'filename', 'tags'],
						threshold: matchSorter.rankings.CONTAINS,
					});
					options = filteredMemes.slice(0, 10).map((meme) => ({
						type: 'meme' as const,
						meme,
					}));
				} else {
					options = allMemes.slice(0, 10).map((meme) => ({
						type: 'meme' as const,
						meme,
					}));
				}
				break;
			}

			case 'gif': {
				setAutocompleteType('gif');
				const searchQuery = (autocompleteTrigger.match[3] ?? '').trim();

				if (!searchQuery) {
					options = [];
				} else if (gifState.status === 'success' && gifState.query === searchQuery) {
					options = gifState.results.slice(0, 10).map((gif) => ({
						type: 'gif' as const,
						gif: {
							...gif,
							title: gif.title || TenorUtils.parseTitleFromUrl(gif.url),
						},
					}));
				} else {
					options = [];
				}
				break;
			}

			case 'sticker': {
				setAutocompleteType('sticker');
				const searchQuery = (autocompleteTrigger.match[2] ?? '').trim();
				let results: ReadonlyArray<GuildStickerRecord>;

				if (!searchQuery) {
					const allStickers = StickerStore.searchWithChannel(channel ?? null, '');
					const filteredStickers = filterStickersForAutocomplete(i18n, allStickers, channel ?? null);
					results = StickerPickerStore.getFrecentStickers(filteredStickers, 10);

					if (results.length < 10) {
						const remainingCount = 10 - results.length;
						const otherStickers = filteredStickers
							.filter((sticker) => !results.some((r) => r.id === sticker.id))
							.slice(0, remainingCount);
						results = [...results, ...otherStickers];
					}
				} else {
					const allStickersSearch = StickerStore.searchWithChannel(channel ?? null, searchQuery);
					results = filterStickersForAutocomplete(i18n, allStickersSearch, channel ?? null);
				}

				options = results.slice(0, 10).map((sticker) => ({
					type: 'sticker' as const,
					sticker,
				}));
				break;
			}
		}

		if (hasOpenCodeBlock()) {
			setAutocompleteOptions([]);
		} else {
			setAutocompleteOptions(options);
		}
	}, [
		channel,
		autocompleteTrigger,
		canMentionEveryone,
		matchedText,
		hasOpenCodeBlock,
		gifState,
		canUseCommand,
		canManageUser,
		memberSearchResults,
		i18n,
	]);

	const handleSelect = React.useCallback(
		(option: AutocompleteOption) => {
			if (!textareaRef.current) {
				return;
			}

			const triggerMatch = autocompleteTrigger?.match ?? null;

			if (!triggerMatch) {
				return;
			}

			if (autocompleteTrigger?.type === 'emojiReaction' && isEmoji(option)) {
				if (channel) {
					const messages = MessageStore.getMessages(channel.id).toArray();
					const mostRecentMessage = messages[messages.length - 1];

					if (mostRecentMessage) {
						ReactionActionCreators.addReaction(i18n, channel.id, mostRecentMessage.id, toReactionEmoji(option.emoji));
					}
				}

				setValue('');
				previousValueRef.current = '';
				setSelectedIndex(0);
				return;
			}

			if (isMeme(option)) {
				ComponentDispatch.dispatch('FAVORITE_MEME_SELECT', {meme: option.meme, autoSend: true});
				setValue('');
				previousValueRef.current = '';
				setSelectedIndex(0);
				return;
			}

			if (isGif(option)) {
				ComponentDispatch.dispatch('GIF_SELECT', {gif: option.gif, autoSend: true});
				setValue('');
				previousValueRef.current = '';
				setSelectedIndex(0);
				return;
			}

			if (isSticker(option)) {
				ComponentDispatch.dispatch('STICKER_SELECT', {sticker: option.sticker});
				setValue('');
				previousValueRef.current = '';
				setSelectedIndex(0);
				return;
			}

			const matchStart = triggerMatch.index ?? 0;
			const matchEnd = matchStart + triggerMatch[0].length;

			const capturedWhitespace = triggerMatch[1] || '';
			const hasLeadingSpace = capturedWhitespace.length > 0;

			let beforeMatch = value.slice(0, matchStart + capturedWhitespace.length);
			let afterMatch = value.slice(matchEnd);

			if (autocompleteTrigger?.type === 'commandArg') {
				const commandPart = triggerMatch[0].slice(0, triggerMatch[0].length - (triggerMatch[3]?.length ?? 0));
				const cleanCommandPart = commandPart.slice(capturedWhitespace.length);
				beforeMatch = value.slice(0, matchStart + capturedWhitespace.length + cleanCommandPart.length);
				afterMatch = value.slice(matchEnd);
			}

			const guildBeforeMatch = hasLeadingSpace || beforeMatch.endsWith(' ') ? '' : ' ';
			const insertPosition = beforeMatch.length + guildBeforeMatch.length;

			let displayText = '';
			let actualText = '';
			let segmentType: MentionSegment['type'] = 'user';
			let segmentId = '';

			if (isMentionMember(option)) {
				const user = option.member.user;
				displayText = `@${user.tag}`;
				actualText = `<@${user.id}>`;
				segmentType = 'user';
				segmentId = user.id;
			} else if (isMentionUser(option)) {
				displayText = `@${option.user.tag}`;
				actualText = `<@${option.user.id}>`;
				segmentType = 'user';
				segmentId = option.user.id;
			} else if (isMentionRole(option)) {
				displayText = `@${option.role.name}`;
				actualText = `<@&${option.role.id}>`;
				segmentType = 'role';
				segmentId = option.role.id;
			} else if (isSpecialMention(option)) {
				displayText = option.kind;
				actualText = option.kind;
				segmentType = 'special';
				segmentId = option.kind;
			} else if (isChannel(option)) {
				displayText = `#${option.channel.name}`;
				actualText = `<#${option.channel.id}>`;
				segmentType = 'channel';
				segmentId = option.channel.id;
			} else if (isEmoji(option)) {
				displayText = `:${option.emoji.name}:`;
				actualText = EmojiStore.getEmojiMarkdown(option.emoji);
				segmentType = 'emoji';
				segmentId = option.emoji.id ?? option.emoji.uniqueName;
			} else if (isCommand(option)) {
				if (option.command.type === 'simple') {
					const commandText = option.command.content;

					if (option.command.name === '/me' || option.command.name === '/spoiler') {
						const newValue = `${beforeMatch}${guildBeforeMatch}${commandText}`;
						const newCursorPosition = newValue.length;

						const trimmedValue = newValue.trimStart();
						const trimmedChars = newValue.length - trimmedValue.length;

						if (trimmedChars > 0) {
							const segments = segmentManagerRef.current.getSegments();
							const adjustedSegments = segments.map((seg) => ({
								...seg,
								start: seg.start - trimmedChars,
								end: seg.end - trimmedChars,
							}));
							segmentManagerRef.current.setSegments(adjustedSegments);
						}

						setValue(trimmedValue);
						previousValueRef.current = trimmedValue;
						setSelectedIndex(0);

						setTimeout(() => {
							if (textareaRef.current) {
								textareaRef.current.setSelectionRange(newCursorPosition, newCursorPosition);
								textareaRef.current.focus();
							}
						}, 0);
						return;
					}

					const newValue = `${beforeMatch}${guildBeforeMatch}${commandText} ${afterMatch}`;
					const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + commandText.length + 1;

					const trimmedValue = newValue.trimStart();
					const trimmedChars = newValue.length - trimmedValue.length;

					if (trimmedChars > 0) {
						const segments = segmentManagerRef.current.getSegments();
						const adjustedSegments = segments.map((seg) => ({
							...seg,
							start: seg.start - trimmedChars,
							end: seg.end - trimmedChars,
						}));
						segmentManagerRef.current.setSegments(adjustedSegments);
					}

					setValue(trimmedValue);
					previousValueRef.current = trimmedValue;
					setSelectedIndex(0);

					setTimeout(() => {
						if (textareaRef.current) {
							textareaRef.current.setSelectionRange(newCursorPosition, newCursorPosition);
							textareaRef.current.focus();
						}
					}, 0);
					return;
				} else {
					const insertionText = getCommandInsertionText(option.command);
					const newValue = `${beforeMatch}${guildBeforeMatch}${insertionText}${afterMatch}`;
					const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + insertionText.length;

					const trimmedValue = newValue.trimStart();
					const trimmedChars = newValue.length - trimmedValue.length;

					if (trimmedChars > 0) {
						const segments = segmentManagerRef.current.getSegments();
						const adjustedSegments = segments.map((seg) => ({
							...seg,
							start: seg.start - trimmedChars,
							end: seg.end - trimmedChars,
						}));
						segmentManagerRef.current.setSegments(adjustedSegments);
					}

					setValue(trimmedValue);
					previousValueRef.current = trimmedValue;
					setSelectedIndex(0);

					setTimeout(() => {
						if (textareaRef.current) {
							textareaRef.current.setSelectionRange(newCursorPosition, newCursorPosition);
							textareaRef.current.focus();
						}
					}, 0);
					return;
				}
			}

			const changeStart = beforeMatch.length;
			const changeEnd = matchEnd;
			segmentManagerRef.current.updateSegmentsForTextChange(changeStart, changeEnd, guildBeforeMatch.length);

			const tempText = `${beforeMatch}${guildBeforeMatch}`;
			const {newText: updatedText} = segmentManagerRef.current.insertSegment(
				tempText,
				insertPosition,
				`${displayText} `,
				`${actualText} `,
				segmentType,
				segmentId,
			);

			const finalText = updatedText + afterMatch;
			const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + displayText.length + 1;

			const trimmedValue = finalText.trimStart();
			const trimmedChars = finalText.length - trimmedValue.length;

			if (trimmedChars > 0) {
				const segments = segmentManagerRef.current.getSegments();
				const adjustedSegments = segments.map((seg) => ({
					...seg,
					start: seg.start - trimmedChars,
					end: seg.end - trimmedChars,
				}));
				segmentManagerRef.current.setSegments(adjustedSegments);
			}

			setValue(trimmedValue);
			previousValueRef.current = trimmedValue;
			setSelectedIndex(0);
			HighlightActionCreators.clearChannelHighlight();

			setTimeout(() => {
				if (textareaRef.current) {
					textareaRef.current.setSelectionRange(newCursorPosition, newCursorPosition);
					textareaRef.current.focus();
				}
			}, 0);
		},
		[value, autocompleteTrigger, textareaRef, segmentManagerRef, previousValueRef, setValue, i18n],
	);

	return {
		autocompleteOptions,
		autocompleteType,
		selectedIndex,
		isAutocompleteAttached,
		setSelectedIndex,
		onCursorMove,
		handleSelect,
		autocompleteQuery,
		isMemberSearchLoading,
	};
}
