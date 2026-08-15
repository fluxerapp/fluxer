// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import Authentication from '@app/features/auth/state/Authentication';
import {
	type AutocompleteOption,
	type AutocompleteType,
	isChannel,
	isCommand,
	isCommandChoice,
	isCommandOptionalAdd,
	isEmoji,
	isGif,
	isMeme,
	isMentionMember,
	isMentionRole,
	isMentionUser,
	isSpecialMention,
	isSticker,
} from '@app/features/channel/components/AutocompleteTypes';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Command} from '@app/features/devtools/hooks/useCommands';
import {useCommands} from '@app/features/devtools/hooks/useCommands';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {filterStickersForAutocomplete} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import * as KlipyUtils from '@app/features/expressions/utils/KlipyUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {
	createComposerCommandReplacement,
	getComposerAutocompleteReplacementStart,
} from '@app/features/lexical/composer/ComposerAutocompleteInsertion';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {applyComposerReplacement, createComposerEmojiPayload} from '@app/features/lexical/composer/ComposerInsertion';
import type {ComposerInsertPayload} from '@app/features/lexical/composer/composerOffsets';
import {normalizeSlotAutocompleteQuery as normalizeSlotQuery} from '@app/features/lexical/composer/SlashSlotAutocompleteQuery';
import type {SlashOptionalContext, SlashSlotAutocompleteContext} from '@app/features/lexical/composer/slashSlots';
import {
	type GifAutocompleteSearchState,
	useAutocompleteGifSearch,
} from '@app/features/lexical/composer/useAutocompleteGifSearch';
import {
	isMemberSearchTrigger,
	useAutocompleteMemberSearch,
	useAutocompleteSlotMemberSearch,
} from '@app/features/lexical/composer/useAutocompleteMemberSearch';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {SearchContext} from '@app/features/member/state/MemberSearch';
import * as HighlightCommands from '@app/features/messaging/commands/HighlightCommands';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {
	filterAutocompleteMediaOptions,
	isAutocompleteMediaOption,
	isAutocompleteMediaTrigger,
} from '@app/features/messaging/utils/AutocompleteMediaOptions';
import {
	buildCommandArgOptions,
	buildEmojiAutocompleteOptions,
	buildEmojiReactionOptions,
	filterDMUsers,
	filterGuildMembers,
	MENTION_RESULT_LIMIT,
	parseMentionQuery,
	SPECIAL_MENTIONS,
} from '@app/features/messaging/utils/AutocompleteOptionBuilders';
import {isAutocompleteTriggerAllowed, type TriggerType} from '@app/features/messaging/utils/AutocompleteTriggerPolicy';
import {toReactionEmoji} from '@app/features/messaging/utils/MessageReactionUtils';
import {
	type AutocompleteTrigger,
	detectAutocompleteTrigger,
	filterCommandsByQuery,
} from '@app/features/messaging/utils/SlashCommandUtils';
import MentionFrecency from '@app/features/notification/state/MentionFrecency';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {formatUserTagForStreamerMode} from '@app/features/user/utils/DisplayNameUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {UserId} from '@fluxer/schema/src/branded/WireIds';
import type {I18n} from '@lingui/core';
import {matchSorter} from 'match-sorter';
import {
	type RefObject,
	useCallback,
	useEffect,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
	useSyncExternalStore,
} from 'react';

interface UseLexicalAutocompleteParams {
	channel: Channel | null;
	handleRef: RefObject<ComposerHandle | null>;
	allowedTriggers?: Array<TriggerType>;
	allowMediaOptions?: boolean;
	maxActualLength?: number;
	onExceedMaxLength?: () => void;
	i18n: I18n;
}

interface AutocompleteMenuState {
	type: AutocompleteType;
	options: Array<AutocompleteOption>;
	query: string;
}

interface MentionRankingSession {
	key: string;
	order: Map<string, number>;
	nextRank: number;
}

function recordMentionMembers(session: MentionRankingSession, members: ReadonlyArray<GuildMember>): void {
	for (const member of members) {
		if (!session.order.has(member.user.id)) {
			session.order.set(member.user.id, session.nextRank++);
		}
	}
}

export type {TriggerType} from '@app/features/messaging/utils/AutocompleteTriggerPolicy';

export function useLexicalAutocomplete({
	channel,
	handleRef,
	allowedTriggers,
	allowMediaOptions = true,
	maxActualLength,
	onExceedMaxLength,
	i18n,
}: UseLexicalAutocompleteParams) {
	const commands = useCommands();
	const [textUpToCursor, setTextUpToCursor] = useState('');
	const [expressionDataVersion, setExpressionDataVersion] = useState(0);
	const [gifState, setGifState] = useState<GifAutocompleteSearchState>({
		status: 'idle',
		query: '',
		results: [],
	});
	const [memberSearchResults, setMemberSearchResults] = useState<Array<GuildMember>>([]);
	const permissionVersion = useSyncExternalStore(Permission.subscribe.bind(Permission), () => Permission.version);
	const gifCacheRef = useRef<Map<string, Array<Gif>>>(new Map());
	const currentGifSearchRef = useRef<string | null>(null);
	const gifDebounceTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const memberSearchContextRef = useRef<SearchContext | null>(null);
	const currentGuildIdRef = useRef<string | null>(null);
	const memberFetchDebounceTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const mentionSessionRef = useRef<MentionRankingSession>({key: '', order: new Map(), nextRank: 0});
	const [slotAutocompleteContext, setSlotAutocompleteContext] = useState<SlashSlotAutocompleteContext | null>(null);
	const slotAutocompleteContextRef = useRef<SlashSlotAutocompleteContext | null>(null);
	slotAutocompleteContextRef.current = slotAutocompleteContext;
	const [slotOptionalContext, setSlotOptionalContext] = useState<SlashOptionalContext | null>(null);
	const slotOptionalContextRef = useRef<SlashOptionalContext | null>(null);
	slotOptionalContextRef.current = slotOptionalContext;
	const [slotMemberSearchResults, setSlotMemberSearchResults] = useState<Array<GuildMember>>([]);
	const slotSearchContextRef = useRef<SearchContext | null>(null);
	const slotCurrentGuildIdRef = useRef<string | null>(null);
	const slotMemberFetchTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

	const refreshTextUpToCursor = useCallback(() => {
		const handle = handleRef.current;
		setTextUpToCursor(handle == null ? '' : handle.getTextUpToCursor());
	}, [handleRef]);
	const refreshSlotContexts = useCallback(() => {
		const handle = handleRef.current;
		const nextSlot = handle == null ? null : handle.getActiveSlotAutocompleteContext();
		const previousSlot = slotAutocompleteContextRef.current;
		if (!slashSlotAutocompleteContextEquals(nextSlot, previousSlot)) {
			setSlotAutocompleteContext(nextSlot);
		}
		const nextOptional = handle == null ? null : handle.getActiveOptionalContext();
		const previousOptional = slotOptionalContextRef.current;
		if (!slashOptionalContextEquals(nextOptional, previousOptional)) {
			setSlotOptionalContext(nextOptional);
		}
	}, [handleRef]);
	const onCursorMove = useCallback(() => {
		refreshTextUpToCursor();
		refreshSlotContexts();
	}, [refreshSlotContexts, refreshTextUpToCursor]);

	useLayoutEffect(() => {
		onCursorMove();
	}, [onCursorMove]);

	useEffect(() => {
		function handleExpressionDataUpdated(): void {
			setExpressionDataVersion((version) => version + 1);
		}
		const unsubscribeEmoji = ComponentDispatch.subscribe('EMOJI_PICKER_RERENDER', handleExpressionDataUpdated);
		const unsubscribeSticker = ComponentDispatch.subscribe('STICKER_PICKER_RERENDER', handleExpressionDataUpdated);
		return () => {
			unsubscribeEmoji();
			unsubscribeSticker();
		};
	}, []);

	const autocompleteTrigger = useMemo(() => {
		const trigger = detectAutocompleteTrigger(textUpToCursor);
		if (trigger == null) {
			return null;
		}
		if (channel == null && trigger.type !== 'emoji') {
			return null;
		}
		if (!isAutocompleteTriggerAllowed(trigger.type, allowedTriggers)) {
			return null;
		}
		if (!allowMediaOptions && isAutocompleteMediaTrigger(trigger.type)) {
			return null;
		}
		return trigger;
	}, [allowMediaOptions, allowedTriggers, channel, textUpToCursor]);
	const autocompleteTriggerType = autocompleteTrigger == null ? null : autocompleteTrigger.type;
	const autocompleteTriggerMatchedText = autocompleteTrigger == null ? '' : autocompleteTrigger.matchedText;
	const autocompleteTriggerToken = autocompleteTrigger
		? `${autocompleteTrigger.type}:${autocompleteTrigger.match.index == null ? -1 : autocompleteTrigger.match.index}:${autocompleteTrigger.match[0]}:${autocompleteTrigger.matchedText}`
		: '';
	const autocompleteQuery = useMemo(() => getAutocompleteQuery(autocompleteTrigger), [autocompleteTrigger]);
	const hasOpenCodeBlock = useMemo(() => {
		const match = textUpToCursor.match(/```/g);
		return match != null && match.length > 0 && match.length % 2 !== 0;
	}, [textUpToCursor]);

	const mentionSessionKey =
		channel != null && channel.guildId != null && isMemberSearchTrigger(autocompleteTriggerType)
			? `${channel.guildId}:${autocompleteTriggerMatchedText}`
			: '';
	if (mentionSessionRef.current.key !== mentionSessionKey) {
		mentionSessionRef.current = {key: mentionSessionKey, order: new Map(), nextRank: 0};
	}
	const recordCurrentMentionMembers = useCallback((members: ReadonlyArray<GuildMember>): void => {
		recordMentionMembers(mentionSessionRef.current, members);
	}, []);

	useAutocompleteMemberSearch({
		triggerType: autocompleteTriggerType,
		matchedText: autocompleteTriggerMatchedText,
		guildId: channel == null ? undefined : channel.guildId,
		searchContextRef: memberSearchContextRef,
		currentGuildIdRef,
		debounceTimerRef: memberFetchDebounceTimerRef,
		setResults: setMemberSearchResults,
	});

	const autocompleteTriggerGifQuery =
		autocompleteTriggerType === 'gif'
			? autocompleteTrigger == null || autocompleteTrigger.match[3] == null
				? ''
				: autocompleteTrigger.match[3].trim()
			: '';
	useAutocompleteGifSearch({
		triggerType: autocompleteTriggerType,
		query: autocompleteTriggerGifQuery,
		cacheRef: gifCacheRef,
		currentSearchRef: currentGifSearchRef,
		debounceTimerRef: gifDebounceTimerRef,
		setState: setGifState,
	});

	const canMentionEveryone = channel != null && Permission.can(Permissions.MENTION_EVERYONE, channel);
	const canUseCommand = useCallback(
		(command: Command) => {
			if (command.type === 'simple') {
				return true;
			}
			if (channel == null) {
				return false;
			}
			if (command.requiresGuild && channel.guildId == null) {
				return false;
			}
			if (command.permission != null) {
				return Permission.can(command.permission, channel);
			}
			return true;
		},
		[channel],
	);
	const canManageUser = useCallback(
		(otherUserId: string, permission: bigint): boolean => {
			if (channel == null || channel.guildId == null) {
				return false;
			}
			if (otherUserId === Authentication.currentUserId) {
				return false;
			}
			const guild = Guilds.getGuild(channel.guildId);
			if (guild == null) {
				return false;
			}
			return Permission.canManageUser(permission, otherUserId as UserId, guild);
		},
		[channel, permissionVersion],
	);
	const canViewChannel = useCallback(
		(userId: string): boolean => {
			if (channel == null || channel.guildId == null) {
				return true;
			}
			return PermissionUtils.can(Permissions.VIEW_CHANNEL, userId as UserId, channel.toJSON());
		},
		[channel],
	);

	const baseAutocompleteMenu = useMemo<AutocompleteMenuState>(() => {
		let type: AutocompleteType = 'mention';
		let options: Array<AutocompleteOption> = [];
		if (autocompleteTrigger == null || hasOpenCodeBlock) {
			return {type, options, query: autocompleteQuery};
		}
		const matchedText = autocompleteTrigger.matchedText;
		switch (autocompleteTrigger.type) {
			case 'commandArgMention':
			case 'commandArg': {
				type = 'mention';
				const commandName = autocompleteTrigger.match[2] == null ? '' : autocompleteTrigger.match[2];
				if (channel == null || (channel.guildId == null && commandName !== 'msg')) {
					break;
				}
				options = buildCommandArgOptions({
					channel,
					commandName,
					matchedText,
					memberSearchResults,
					canManageUser,
					canViewChannel,
					stableOrder: mentionSessionRef.current.order,
				});
				recordCurrentMentionMembers(options.filter(isMentionMember).map((option) => option.member));
				break;
			}
			case 'mention': {
				type = 'mention';
				if (channel == null) {
					break;
				}
				const parsedQuery = parseMentionQuery(matchedText);
				const queryForMatching = parsedQuery.usernameQuery.trim();
				if (channel.guildId == null) {
					const users = channel.recipientIds
						.map((id) => Users.getUser(id))
						.filter((user): user is User => user != null);
					const userOptions = filterDMUsers(users, parsedQuery);
					options = channel.isPersonalNotes() ? userOptions : [...userOptions, ...SPECIAL_MENTIONS];
				} else {
					const members = filterGuildMembers(
						memberSearchResults,
						parsedQuery,
						true,
						canViewChannel,
						mentionSessionRef.current.order,
					);
					recordCurrentMentionMembers(members.map((option) => option.member));
					const mentionableRoles = Guilds.getGuildRoles(channel.guildId).filter(
						(role) => canMentionEveryone || role.mentionable,
					);
					const matchedRoles =
						queryForMatching.length > 0
							? matchSorter(mentionableRoles, queryForMatching, {
									keys: ['name'],
									threshold: matchSorter.rankings.CONTAINS,
								})
							: mentionableRoles;
					const roles = [...matchedRoles]
						.sort((a, b) => b.position - a.position)
						.slice(0, MENTION_RESULT_LIMIT)
						.map((role) => ({
							type: 'mention' as const,
							kind: 'role' as const,
							role,
						}));
					const specialMentions = canMentionEveryone
						? SPECIAL_MENTIONS.filter((mention) => {
								if (queryForMatching.length === 0) {
									return true;
								}
								return mention.kind.slice(1).toLowerCase().includes(queryForMatching.toLowerCase());
							})
						: [];
					options = [...members, ...specialMentions, ...roles];
				}
				break;
			}
			case 'channel': {
				type = 'channel';
				if (channel == null) {
					break;
				}
				options = matchSorter(Channels.getGuildChannels(channel.guildId == null ? '' : channel.guildId), matchedText, {
					keys: ['name'],
				})
					.filter((candidate) => !candidate.isGuildCategory())
					.map((candidate) => ({
						type: 'channel' as const,
						channel: candidate,
					}))
					.sort(
						(a, b) =>
							(a.channel.position == null ? 0 : a.channel.position) -
							(b.channel.position == null ? 0 : b.channel.position),
					)
					.slice(0, MENTION_RESULT_LIMIT);
				break;
			}
			case 'emojiReaction': {
				type = 'emoji';
				options = buildEmojiReactionOptions({channel: channel == null ? null : channel, matchedText, i18n});
				break;
			}
			case 'emoji': {
				type = 'emoji';
				options = buildEmojiAutocompleteOptions({
					channel: channel == null ? null : channel,
					matchedText,
					i18n,
					prefs: {
						showDefaultEmojis: Accessibility.showDefaultEmojisInExpressionAutocomplete,
						showCustomEmojis: Accessibility.showCustomEmojisInExpressionAutocomplete,
						showStickers: allowMediaOptions && Accessibility.showStickersInExpressionAutocomplete,
						showMemes: allowMediaOptions && Accessibility.showMemesInExpressionAutocomplete,
					},
				});
				break;
			}
			case 'command': {
				type = 'command';
				options = filterCommandsByQuery(commands, matchedText)
					.filter(canUseCommand)
					.map((command) => ({
						type: 'command' as const,
						command,
					}));
				break;
			}
			case 'meme': {
				type = 'meme';
				const searchQuery = (autocompleteTrigger.match[2] == null ? '' : autocompleteTrigger.match[2]).trim();
				const allMemes = FavoriteMemes.getAllMemes();
				const memes =
					searchQuery.length > 0
						? matchSorter(allMemes, searchQuery, {
								keys: ['name', 'altText', 'filename', 'tags'],
								threshold: matchSorter.rankings.CONTAINS,
							})
						: allMemes;
				options = memes.slice(0, MENTION_RESULT_LIMIT).map((meme) => ({
					type: 'meme' as const,
					meme,
				}));
				break;
			}
			case 'gif': {
				type = 'gif';
				const searchQuery = (autocompleteTrigger.match[3] == null ? '' : autocompleteTrigger.match[3]).trim();
				if (searchQuery.length > 0 && gifState.status === 'success' && gifState.query === searchQuery) {
					options = gifState.results.slice(0, MENTION_RESULT_LIMIT).map((gif) => ({
						type: 'gif' as const,
						gif: {
							...gif,
							title: gif.title || KlipyUtils.parseTitleFromUrl(gif.url),
						},
					}));
				}
				break;
			}
			case 'sticker': {
				type = 'sticker';
				const searchQuery = (autocompleteTrigger.match[2] == null ? '' : autocompleteTrigger.match[2]).trim();
				let results: ReadonlyArray<GuildSticker>;
				if (searchQuery.length === 0) {
					const allStickers = Sticker.searchWithChannel(channel == null ? null : channel, '');
					const filteredStickers = filterStickersForAutocomplete(i18n, allStickers, channel == null ? null : channel);
					results = StickerPicker.getFrecentStickers(filteredStickers, MENTION_RESULT_LIMIT);
					if (results.length < MENTION_RESULT_LIMIT) {
						const remainingCount = MENTION_RESULT_LIMIT - results.length;
						const otherStickers = filteredStickers
							.filter((sticker) => !results.some((result) => result.id === sticker.id))
							.slice(0, remainingCount);
						results = [...results, ...otherStickers];
					}
				} else {
					const allStickersSearch = Sticker.searchWithChannel(channel == null ? null : channel, searchQuery);
					results = filterStickersForAutocomplete(i18n, allStickersSearch, channel == null ? null : channel);
				}
				options = results.slice(0, MENTION_RESULT_LIMIT).map((sticker) => ({
					type: 'sticker' as const,
					sticker,
				}));
				break;
			}
		}
		return {type, options: filterAutocompleteMediaOptions(options, allowMediaOptions), query: autocompleteQuery};
	}, [
		allowMediaOptions,
		autocompleteQuery,
		autocompleteTrigger,
		autocompleteTriggerToken,
		canMentionEveryone,
		canManageUser,
		canUseCommand,
		canViewChannel,
		channel,
		commands,
		expressionDataVersion,
		gifState,
		hasOpenCodeBlock,
		i18n,
		memberSearchResults,
		permissionVersion,
		recordCurrentMentionMembers,
	]);

	useEffect(() => {
		if (baseAutocompleteMenu.options.length > 0 && autocompleteTriggerType === 'channel') {
			const firstChannel = baseAutocompleteMenu.options.find(isChannel);
			if (firstChannel != null) {
				HighlightCommands.highlightChannel(firstChannel.channel.id);
			}
		} else {
			HighlightCommands.clearChannelHighlight();
		}
		return () => {
			HighlightCommands.clearChannelHighlight();
		};
	}, [autocompleteTriggerType, baseAutocompleteMenu.options]);

	useAutocompleteSlotMemberSearch({
		guildId: channel == null ? undefined : channel.guildId,
		context: slotAutocompleteContext,
		searchContextRef: slotSearchContextRef,
		currentGuildIdRef: slotCurrentGuildIdRef,
		debounceTimerRef: slotMemberFetchTimerRef,
		setResults: setSlotMemberSearchResults,
	});

	const slotAutocompleteOptions = useMemo<Array<AutocompleteOption>>(() => {
		if (slotAutocompleteContext == null) {
			return [];
		}
		const query = normalizeSlotQuery(slotAutocompleteContext);
		switch (slotAutocompleteContext.optionType) {
			case 'choice':
				return buildSlotChoiceOptions(slotAutocompleteContext.choices, query);
			case 'boolean':
				return buildSlotChoiceOptions(
					[
						{name: 'true', value: 'true'},
						{name: 'false', value: 'false'},
					],
					query,
				);
			case 'user':
				return buildSlotUserOptions({
					channel,
					commandName: slotAutocompleteContext.commandName,
					query,
					memberSearchResults: slotMemberSearchResults,
					canManageUser,
					canViewChannel,
				});
			case 'channel':
				return buildSlotChannelOptions(channel, query);
			case 'role':
				return buildSlotRoleOptions(channel, query);
			default:
				return [];
		}
	}, [canManageUser, canViewChannel, channel, slotAutocompleteContext, slotMemberSearchResults]);
	const slotOptionalOptions = useMemo<Array<AutocompleteOption>>(() => {
		if (slotOptionalContext == null) {
			return [];
		}
		const query = slotOptionalContext.query.trim().toLowerCase();
		const filtered =
			query.length === 0
				? slotOptionalContext.options
				: slotOptionalContext.options.filter(
						(option) => option.name.toLowerCase().includes(query) || option.description.toLowerCase().includes(query),
					);
		return filtered.map((option) => ({
			type: 'commandOptionalAdd' as const,
			name: option.name,
			description: option.description,
		}));
	}, [slotOptionalContext]);
	const slotAutocompleteType = useMemo<AutocompleteType>(() => {
		const optionType = slotAutocompleteContext == null ? undefined : slotAutocompleteContext.optionType;
		switch (optionType) {
			case 'channel':
				return 'channel';
			case 'choice':
			case 'boolean':
				return 'commandChoice';
			default:
				return 'mention';
		}
	}, [slotAutocompleteContext == null ? undefined : slotAutocompleteContext.optionType]);
	const activeSlotMenu =
		slotAutocompleteContext != null
			? {
					type: slotAutocompleteType,
					options: slotAutocompleteOptions,
					query: normalizeSlotQuery(slotAutocompleteContext),
				}
			: slotOptionalContext != null
				? {type: 'commandOptionalAdd' as const, options: slotOptionalOptions, query: slotOptionalContext.query}
				: null;
	const isSlotMenu = activeSlotMenu != null;
	const autocompleteOptions = isSlotMenu ? activeSlotMenu.options : baseAutocompleteMenu.options;
	const autocompleteType = isSlotMenu ? activeSlotMenu.type : baseAutocompleteMenu.type;
	const resolvedAutocompleteQuery = isSlotMenu ? activeSlotMenu.query : baseAutocompleteMenu.query;
	const isAutocompleteAttached = autocompleteOptions.length > 0;

	const handleSelect = useCallback(
		(option: AutocompleteOption) => {
			if (!allowMediaOptions && isAutocompleteMediaOption(option)) {
				return;
			}
			const handle = handleRef.current;
			if (handle == null) {
				return;
			}
			const slotContext = slotAutocompleteContextRef.current;
			if (slotContext != null) {
				const slotPayload = optionToSlotPayload(option, slotContext.optionType, channel);
				if (slotPayload != null) {
					handle.applySlotPayload(slotPayload);
					return;
				}
				const slotText = optionToSlotText(option, slotContext.optionType);
				if (slotText != null) {
					handle.applySlotChoice(slotText);
					return;
				}
			}
			if (isCommandChoice(option)) {
				handle.applySlotChoice(option.choice.name);
				return;
			}
			if (isCommandOptionalAdd(option)) {
				handle.applyOptionalChoice(option.name);
				return;
			}
			if (isMeme(option)) {
				ComponentDispatch.dispatch('FAVORITE_MEME_SELECT', {meme: option.meme, autoSend: true});
				handle.clear();
				return;
			}
			if (isGif(option)) {
				ComponentDispatch.dispatch('GIF_SELECT', {gif: option.gif, autoSend: true});
				handle.clear();
				return;
			}
			if (isSticker(option)) {
				ComponentDispatch.dispatch('STICKER_SELECT', {sticker: option.sticker});
				handle.clear();
				return;
			}
			const currentTextUpToCursor = handle.getTextUpToCursor();
			const trigger = detectAutocompleteTrigger(currentTextUpToCursor);
			if (trigger == null) {
				return;
			}
			const caret = currentTextUpToCursor.length;
			const matchStart = getComposerAutocompleteReplacementStart(currentTextUpToCursor, trigger.type, trigger.match);
			if (trigger.type === 'emojiReaction' && isEmoji(option)) {
				if (channel != null) {
					const messages = Messages.getMessages(channel.id).toArray();
					const mostRecent = messages[messages.length - 1];
					if (mostRecent != null) {
						ReactionCommands.addReaction(i18n, channel.id, mostRecent.id, toReactionEmoji(option.emoji));
					}
				}
				handle.clear();
				return;
			}
			if (isCommand(option)) {
				const command = option.command;
				const replacement = createComposerCommandReplacement(
					handle.getDisplayValue(),
					currentTextUpToCursor,
					matchStart,
					caret,
					command,
				);
				if (command.type === 'action' && command.options != null && command.options.length > 0) {
					handle.insertSlashCommand(command.name, command.options, replacement.start, replacement.end);
					return;
				}
				handle.replaceRange(replacement.start, replacement.end, {kind: 'text', text: replacement.text});
				return;
			}
			const payload = optionToPayload(option, channel);
			if (payload == null) {
				return;
			}
			const didInsert = applyComposerReplacement(
				handle,
				{start: matchStart, end: caret},
				payload,
				{},
				{
					maxWireLength: maxActualLength,
					onExceedMaxLength,
				},
			);
			if (!didInsert) {
				return;
			}
			HighlightCommands.clearChannelHighlight();
		},
		[allowMediaOptions, channel, handleRef, i18n, maxActualLength, onExceedMaxLength],
	);

	return {
		autocompleteOptions,
		autocompleteType,
		isAutocompleteAttached,
		onCursorMove,
		handleSelect,
		autocompleteQuery: resolvedAutocompleteQuery,
		isSlotMenu,
	};
}

function getAutocompleteQuery(trigger: AutocompleteTrigger | null): string {
	if (trigger == null) {
		return '';
	}
	switch (trigger.type) {
		case 'mention':
		case 'channel':
		case 'emoji':
		case 'emojiReaction':
		case 'command':
		case 'commandArg':
		case 'commandArgMention':
			return trigger.matchedText;
		case 'meme':
		case 'sticker':
			return (trigger.match[2] == null ? '' : trigger.match[2]).trim();
		case 'gif':
			return (trigger.match[3] == null ? '' : trigger.match[3]).trim();
		default:
			return '';
	}
}

function buildSlotChoiceOptions(
	choices: ReadonlyArray<{name: string; value: string}>,
	query: string,
): Array<AutocompleteOption> {
	const lowered = query.toLowerCase();
	const filtered =
		lowered.length === 0
			? choices
			: choices.filter(
					(choice) => choice.name.toLowerCase().includes(lowered) || choice.value.toLowerCase().includes(lowered),
				);
	return filtered.map((choice) => ({
		type: 'commandChoice' as const,
		choice: {name: choice.name, value: choice.value},
		description: '',
	}));
}

interface BuildSlotUserOptionsParams {
	channel: Channel | null;
	commandName: string | null;
	query: string;
	memberSearchResults: Array<GuildMember>;
	canManageUser: (otherUserId: string, permission: bigint) => boolean;
	canViewChannel: (userId: string) => boolean;
}

function buildSlotUserOptions({
	channel,
	commandName,
	query,
	memberSearchResults,
	canManageUser,
	canViewChannel,
}: BuildSlotUserOptionsParams): Array<AutocompleteOption> {
	if (channel == null) {
		return [];
	}
	const normalizedCommandName = commandName == null ? '' : commandName;
	if (normalizedCommandName === 'ban' || normalizedCommandName === 'kick' || normalizedCommandName === 'msg') {
		return buildCommandArgOptions({
			channel,
			commandName: normalizedCommandName,
			matchedText: query,
			memberSearchResults,
			canManageUser,
			canViewChannel,
		});
	}
	const parsedQuery = parseMentionQuery(query);
	if (channel.guildId == null) {
		const users = channel.recipientIds.map((id) => Users.getUser(id)).filter((user): user is User => user != null);
		return filterDMUsers(users, parsedQuery);
	}
	return filterGuildMembers(memberSearchResults, parsedQuery, true, canViewChannel);
}

function buildSlotChannelOptions(channel: Channel | null, query: string): Array<AutocompleteOption> {
	if (channel == null || channel.guildId == null) {
		return [];
	}
	const channels = Channels.getGuildChannels(channel.guildId).filter((candidate) => !candidate.isGuildCategory());
	const matched = query.length === 0 ? channels : matchSorter(channels, query, {keys: ['name']});
	return [...matched]
		.sort((a, b) => (a.position == null ? 0 : a.position) - (b.position == null ? 0 : b.position))
		.slice(0, MENTION_RESULT_LIMIT)
		.map((channel) => ({
			type: 'channel' as const,
			channel,
		}));
}

function buildSlotRoleOptions(channel: Channel | null, query: string): Array<AutocompleteOption> {
	if (channel == null || channel.guildId == null) {
		return [];
	}
	const roles = Guilds.getGuildRoles(channel.guildId);
	const matched = query.length === 0 ? roles : matchSorter(roles, query, {keys: ['name']});
	return [...matched]
		.sort((a, b) => b.position - a.position)
		.slice(0, MENTION_RESULT_LIMIT)
		.map((role) => ({
			type: 'mention' as const,
			kind: 'role' as const,
			role,
		}));
}

function optionToSlotText(
	option: AutocompleteOption,
	optionType: SlashSlotAutocompleteContext['optionType'],
): string | null {
	if ((optionType === 'choice' || optionType === 'boolean') && isCommandChoice(option)) {
		return option.choice.name;
	}
	return null;
}

function optionToSlotPayload(
	option: AutocompleteOption,
	optionType: SlashSlotAutocompleteContext['optionType'],
	channel: Channel | null,
): ComposerInsertPayload | null {
	if (optionType === 'user' && (isMentionMember(option) || isMentionUser(option))) {
		return optionToPayload(option, channel);
	}
	if (optionType === 'channel' && isChannel(option)) {
		return optionToPayload(option, channel);
	}
	if (optionType === 'role' && isMentionRole(option)) {
		return optionToPayload(option, channel);
	}
	return null;
}

function slashSlotAutocompleteContextEquals(
	a: SlashSlotAutocompleteContext | null,
	b: SlashSlotAutocompleteContext | null,
): boolean {
	if (a == null || b == null) {
		return a === b;
	}
	if (
		a.commandName !== b.commandName ||
		a.optionName !== b.optionName ||
		a.optionType !== b.optionType ||
		a.query !== b.query ||
		a.choices.length !== b.choices.length
	) {
		return false;
	}
	return a.choices.every((choice, index) => {
		const other = b.choices[index]!;
		return choice.name === other.name && choice.value === other.value;
	});
}

function slashOptionalContextEquals(a: SlashOptionalContext | null, b: SlashOptionalContext | null): boolean {
	if (a == null || b == null) {
		return a === b;
	}
	if (a.query !== b.query || a.options.length !== b.options.length) {
		return false;
	}
	return a.options.every((option, index) => option.name === b.options[index]!.name);
}

function optionToPayload(option: AutocompleteOption, channel: Channel | null): ComposerInsertPayload | null {
	const channelGuildId: string | null = channel == null || channel.guildId == null ? null : channel.guildId;
	if (isMentionMember(option)) {
		const user = option.member.user;
		MentionFrecency.recordMention(channelGuildId, user.id);
		return {
			kind: 'mention',
			mentionType: 'user',
			id: user.id,
			display: `@${formatUserTagForStreamerMode(user)}`,
			wire: `<@${user.id}>`,
		};
	}
	if (isMentionUser(option)) {
		MentionFrecency.recordMention(channelGuildId, option.user.id);
		return {
			kind: 'mention',
			mentionType: 'user',
			id: option.user.id,
			display: `@${formatUserTagForStreamerMode(option.user)}`,
			wire: `<@${option.user.id}>`,
		};
	}
	if (isMentionRole(option)) {
		return {
			kind: 'mention',
			mentionType: 'role',
			id: option.role.id,
			display: `@${option.role.name}`,
			wire: `<@&${option.role.id}>`,
		};
	}
	if (isSpecialMention(option)) {
		return {kind: 'mention', mentionType: 'special', id: option.kind, display: option.kind, wire: option.kind};
	}
	if (isChannel(option)) {
		return {
			kind: 'mention',
			mentionType: 'channel',
			id: option.channel.id,
			display: `#${option.channel.name}`,
			wire: `<#${option.channel.id}>`,
		};
	}
	if (isEmoji(option)) {
		return createComposerEmojiPayload(option.emoji);
	}
	return null;
}
