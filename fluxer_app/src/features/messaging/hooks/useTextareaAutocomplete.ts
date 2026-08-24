// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import Authentication from '@app/features/auth/state/Authentication';
import {
	type AutocompleteOption,
	type AutocompleteType,
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
} from '@app/features/channel/components/Autocomplete';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Command} from '@app/features/devtools/hooks/useCommands';
import {useCommands} from '@app/features/devtools/hooks/useCommands';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import * as GifCommands from '@app/features/expressions/commands/GifCommands';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import {filterStickersForAutocomplete} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import * as KlipyUtils from '@app/features/expressions/utils/KlipyUtils';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {SearchContext} from '@app/features/member/state/MemberSearch';
import MemberSearch from '@app/features/member/state/MemberSearch';
import * as HighlightCommands from '@app/features/messaging/commands/HighlightCommands';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {
	buildCommandArgOptions,
	buildEmojiAutocompleteOptions,
	buildEmojiReactionOptions,
	filterDMUsers,
	filterGuildMembers,
	MEMBER_SEARCH_LIMIT,
	MENTION_RESULT_LIMIT,
	parseMentionQuery,
	SPECIAL_MENTIONS,
} from '@app/features/messaging/hooks/use_textarea_autocomplete/builders';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {
	detectAutocompleteTrigger,
	filterCommandsByQuery,
	getCommandInsertionText,
} from '@app/features/messaging/utils/SlashCommandUtils';
import {
	applyTextareaTextChange,
	type PrepareTextareaTextChange,
} from '@app/features/messaging/utils/TextareaNativeEditUtils';
import {type MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import MentionFrecency from '@app/features/notification/state/MentionFrecency';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {UserId} from '@fluxer/schema/src/branded/WireIds';
import {useLingui} from '@lingui/react/macro';
import {matchSorter} from 'match-sorter';
import {useCallback, useEffect, useMemo, useRef, useState, useSyncExternalStore} from 'react';

const logger = new Logger('useTextareaAutocomplete');

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
	channel: Channel | null;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	segmentManagerRef: React.MutableRefObject<TextareaSegmentManager>;
	previousValueRef: React.MutableRefObject<string>;
	prepareTextChange: PrepareTextareaTextChange;
	allowedTriggers?: Array<TriggerType>;
	maxActualLength?: number;
	onExceedMaxLength?: () => void;
}

export function useTextareaAutocomplete({
	channel,
	value,
	setValue,
	textareaRef,
	segmentManagerRef,
	previousValueRef,
	prepareTextChange,
	allowedTriggers,
	maxActualLength,
	onExceedMaxLength,
}: UseTextareaAutocompleteParams): UseTextareaAutocompleteReturn {
	const {i18n} = useLingui();
	const commands = useCommands();
	const [autocompleteOptions, setAutocompleteOptions] = useState<Array<AutocompleteOption>>([]);
	const [autocompleteType, setAutocompleteType] = useState<AutocompleteType>('mention');
	const [selectedIndex, setSelectedIndex] = useState(0);
	const [valueUpToCursor, setValueUpToCursor] = useState('');
	const [expressionDataVersion, setExpressionDataVersion] = useState(0);
	const [gifState, setGifState] = useState<{
		status: 'idle' | 'loading' | 'success' | 'error';
		query: string;
		results: Array<Gif>;
	}>({
		status: 'idle',
		query: '',
		results: [],
	});
	const [memberSearchResults, setMemberSearchResults] = useState<Array<GuildMember>>([]);
	const [isMemberSearchLoading, setIsMemberSearchLoading] = useState(false);
	const permissionVersion = useSyncExternalStore(Permission.subscribe.bind(Permission), () => Permission.version);
	const gifCacheRef = useRef<Map<string, Array<Gif>>>(new Map());
	const currentSearchRef = useRef<string | null>(null);
	const debounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const searchContextRef = useRef<SearchContext | null>(null);
	const hasChannel = channel != null;
	const allowedTriggersToken = useMemo(() => {
		if (!allowedTriggers || allowedTriggers.length === 0) {
			return '';
		}
		return [...allowedTriggers].sort().join('|');
	}, [allowedTriggers]);
	const allowedTriggerSet = useMemo(() => {
		if (allowedTriggersToken.length === 0) {
			return null;
		}
		return new Set<TriggerType>(allowedTriggersToken.split('|') as Array<TriggerType>);
	}, [allowedTriggersToken]);
	const autocompleteTrigger = useMemo(() => {
		const trigger = detectAutocompleteTrigger(valueUpToCursor);
		if (!trigger) return null;
		if (!hasChannel && trigger.type !== 'emoji') {
			return null;
		}
		if (allowedTriggerSet && !allowedTriggerSet.has(trigger.type)) {
			return null;
		}
		return trigger;
	}, [allowedTriggerSet, hasChannel, valueUpToCursor]);
	const autocompleteTriggerType = autocompleteTrigger?.type ?? null;
	const autocompleteTriggerMatchedText = autocompleteTrigger?.matchedText ?? '';
	const autocompleteTriggerMatch2 = autocompleteTrigger?.match[2] ?? '';
	const autocompleteTriggerMatch3 = autocompleteTrigger?.match[3] ?? '';
	const autocompleteTriggerGifQuery = autocompleteTriggerType === 'gif' ? autocompleteTriggerMatch3.trim() : '';
	const autocompleteTriggerToken = autocompleteTrigger
		? `${autocompleteTrigger.type}:${autocompleteTrigger.match.index ?? -1}:${autocompleteTrigger.match[0]}:${autocompleteTrigger.matchedText}`
		: '';
	useEffect(() => {
		function handleExpressionDataUpdated(): void {
			setExpressionDataVersion((version) => version + 1);
		}
		const unsubscribeEmoji = ComponentBus.subscribe('EMOJI_PICKER_RERENDER', handleExpressionDataUpdated);
		const unsubscribeSticker = ComponentBus.subscribe('STICKER_PICKER_RERENDER', handleExpressionDataUpdated);
		return () => {
			unsubscribeEmoji();
			unsubscribeSticker();
		};
	}, []);
	useEffect(() => {
		if (debounceTimerRef.current) {
			clearTimeout(debounceTimerRef.current);
		}
		if (autocompleteTriggerType === 'gif') {
			const searchQuery = autocompleteTriggerGifQuery;
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
				GifCommands.search(searchQuery)
					.then((gifs) => {
						gifCacheRef.current.set(searchQuery, gifs);
						setGifState({status: 'success', query: searchQuery, results: gifs});
					})
					.catch((error) => {
						logger.error('GIF search failed', error);
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
	}, [autocompleteTriggerGifQuery, autocompleteTriggerType]);
	const currentGuildIdRef = useRef<string | null>(null);
	const memberFetchDebounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const mentionSessionRef = useRef<{
		key: string;
		order: Map<string, number>;
		nextRank: number;
	}>({key: '', order: new Map(), nextRank: 0});
	const recordMentionMembers = useCallback((members: ReadonlyArray<GuildMember>) => {
		const session = mentionSessionRef.current;
		for (const member of members) {
			if (!session.order.has(member.user.id)) {
				session.order.set(member.user.id, session.nextRank++);
			}
		}
	}, []);
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			const guildId = currentGuildIdRef.current;
			const guildMemberRecords: Array<GuildMember> = results
				.map((transformed) => {
					if (guildId) {
						const member = GuildMembers.getMember(guildId, transformed.id);
						return member ?? null;
					}
					const guilds = Guilds.getGuilds();
					for (const guild of guilds) {
						const member = GuildMembers.getMember(guild.id, transformed.id);
						if (member) {
							return member;
						}
					}
					return null;
				})
				.filter((m): m is GuildMember => m !== null);
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
	useEffect(() => {
		const context = searchContextRef.current;
		if (!context) return;
		const isMentionTrigger =
			autocompleteTriggerType === 'mention' ||
			autocompleteTriggerType === 'commandArgMention' ||
			autocompleteTriggerType === 'commandArg';
		if (!isMentionTrigger || !channel?.guildId) {
			currentGuildIdRef.current = null;
			context.cancelSearch();
			setMemberSearchResults([]);
			setIsMemberSearchLoading(false);
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
			return;
		}
		const searchQuery = autocompleteTriggerMatchedText;
		const guildId = channel.guildId;
		currentGuildIdRef.current = guildId;
		const sessionKey = `${guildId}:${searchQuery}`;
		if (mentionSessionRef.current.key !== sessionKey) {
			mentionSessionRef.current = {key: sessionKey, order: new Map(), nextRank: 0};
		}
		setIsMemberSearchLoading(true);
		const boosters = MentionFrecency.getBoosters(guildId);
		context.beginSearch(searchQuery, {guild: guildId}, new Set(), new Set(), boosters);
		if (memberFetchDebounceTimerRef.current) {
			clearTimeout(memberFetchDebounceTimerRef.current);
		}
		memberFetchDebounceTimerRef.current = setTimeout(() => {
			void MemberSearch.fetchMembersInBackground(searchQuery, [guildId]);
			memberFetchDebounceTimerRef.current = null;
		}, 300);
	}, [autocompleteTriggerMatchedText, autocompleteTriggerType, channel?.guildId]);
	const autocompleteQuery = useMemo(() => {
		if (!autocompleteTrigger) return '';
		switch (autocompleteTriggerType) {
			case 'mention':
			case 'channel':
			case 'emoji':
			case 'emojiReaction':
			case 'command':
			case 'commandArg':
			case 'commandArgMention':
				return autocompleteTriggerMatchedText;
			case 'meme':
				return autocompleteTriggerMatch2.trim();
			case 'gif':
				return autocompleteTriggerMatch3.trim();
			case 'sticker':
				return autocompleteTriggerMatch2.trim();
			default:
				return '';
		}
	}, [
		autocompleteTrigger,
		autocompleteTriggerMatchedText,
		autocompleteTriggerMatch2,
		autocompleteTriggerMatch3,
		autocompleteTriggerType,
	]);
	const hasOpenCodeBlock = useCallback(() => {
		const textarea = textareaRef.current;
		const match = textarea?.value.slice(0, textarea.selectionStart).match(/```/g);
		return match != null && match.length > 0 && match.length % 2 !== 0;
	}, [textareaRef]);
	const [suppressedByArrowKey, setSuppressedByArrowKeyState] = useState(false);
	const suppressedByArrowKeyRef = useRef(false);
	const setSuppressedByArrowKey = useCallback((nextSuppressed: boolean) => {
		if (suppressedByArrowKeyRef.current === nextSuppressed) return;
		suppressedByArrowKeyRef.current = nextSuppressed;
		setSuppressedByArrowKeyState(nextSuppressed);
	}, []);
	const isAutocompleteAttachedRaw = !!autocompleteTrigger && autocompleteOptions.length > 0;
	const isAutocompleteAttached = isAutocompleteAttachedRaw && !suppressedByArrowKey;
	useEffect(() => {
		setSuppressedByArrowKey(false);
	}, [value, setSuppressedByArrowKey]);
	useEffect(() => {
		const textarea = textareaRef.current;
		if (!textarea) return;
		const handleKeyDown = (event: KeyboardEvent) => {
			if (
				event.key !== 'ArrowUp' &&
				event.key !== 'ArrowDown' &&
				event.key !== 'ArrowLeft' &&
				event.key !== 'ArrowRight'
			) {
				return;
			}
			if (!isAutocompleteAttachedRaw) {
				setSuppressedByArrowKey(true);
			}
		};
		const handleClear = () => setSuppressedByArrowKey(false);
		textarea.addEventListener('keydown', handleKeyDown);
		textarea.addEventListener('pointerdown', handleClear);
		textarea.addEventListener('beforeinput', handleClear);
		return () => {
			textarea.removeEventListener('keydown', handleKeyDown);
			textarea.removeEventListener('pointerdown', handleClear);
			textarea.removeEventListener('beforeinput', handleClear);
		};
	}, [textareaRef, isAutocompleteAttachedRaw, setSuppressedByArrowKey]);
	const matchedText = autocompleteTrigger ? autocompleteTrigger.matchedText : null;
	const onCursorMove = useCallback(() => {
		const position = textareaRef.current?.selectionStart ?? 0;
		const beforeCursor = value.slice(0, position);
		setValueUpToCursor(beforeCursor);
	}, [value, textareaRef]);
	const onCursorMoveRef = useRef(onCursorMove);
	useEffect(() => {
		onCursorMoveRef.current = onCursorMove;
	}, [onCursorMove]);
	useEffect(() => {
		const handleSelectionChange = () => {
			onCursorMoveRef.current();
		};
		document.addEventListener('selectionchange', handleSelectionChange);
		return () => {
			document.removeEventListener('selectionchange', handleSelectionChange);
		};
	}, []);
	useEffect(() => {
		const position = textareaRef.current?.selectionStart ?? value.length;
		setValueUpToCursor(value.slice(0, position));
	}, [value, textareaRef]);
	useEffect(() => {
		if (isAutocompleteAttached) {
			if (autocompleteTriggerType === 'channel') {
				const firstChannel = autocompleteOptions.find(isChannel);
				if (firstChannel) {
					HighlightCommands.highlightChannel(firstChannel.channel.id);
				}
			}
		} else {
			HighlightCommands.clearChannelHighlight();
		}
		return () => {
			HighlightCommands.clearChannelHighlight();
		};
	}, [isAutocompleteAttached, autocompleteOptions, autocompleteTriggerType]);
	const canMentionEveryone = channel ? Permission.can(Permissions.MENTION_EVERYONE, channel) : false;
	const canUseCommand = useCallback(
		(command: Command) => {
			if (command.type === 'simple') return true;
			if (!channel) return false;
			if (command.requiresGuild && !channel.guildId) return false;
			if (command.permission) {
				return Permission.can(command.permission, channel);
			}
			return true;
		},
		[channel],
	);
	const canManageUser = useCallback(
		(otherUserId: string, permission: bigint) => {
			if (!channel || !channel.guildId) return false;
			const currentUserId = Authentication.currentUserId;
			if (otherUserId === currentUserId) return false;
			const guild = Guilds.getGuild(channel.guildId);
			if (!guild) return false;
			return Permission.canManageUser(permission, otherUserId as UserId, guild);
		},
		[channel],
	);
	const canViewChannel = useCallback(
		(userId: string): boolean => {
			if (!channel || !channel.guildId) {
				return true;
			}
			return PermissionUtils.can(Permissions.VIEW_CHANNEL, userId as UserId, channel.toJSON());
		},
		[channel],
	);
	useEffect(() => {
		let options: Array<AutocompleteOption> = [];
		if (!autocompleteTrigger) {
			setAutocompleteOptions([]);
			return;
		}
		switch (autocompleteTrigger.type) {
			case 'commandArgMention':
			case 'commandArg': {
				setAutocompleteType('mention');
				const commandName = autocompleteTrigger.match[2];
				if (!channel || (!channel.guildId && commandName !== 'msg')) {
					setAutocompleteOptions([]);
					return;
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
				const memberOptions = options.filter(isMentionMember).map((o) => o.member);
				if (memberOptions.length > 0) {
					recordMentionMembers(memberOptions);
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
						.map((id) => Users.getUser(id))
						.filter((user): user is NonNullable<typeof user> => user != null);
					const parsedQuery = parseMentionQuery(matchedText ?? '');
					const userOptions = filterDMUsers(users, parsedQuery);
					options = channel.isPersonalNotes() ? userOptions : [...userOptions, ...SPECIAL_MENTIONS];
				} else {
					const parsedQuery = parseMentionQuery(matchedText ?? '');
					const queryForMatching = parsedQuery.usernameQuery.trim();
					const members = filterGuildMembers(
						memberSearchResults,
						parsedQuery,
						true,
						canViewChannel,
						mentionSessionRef.current.order,
					);
					recordMentionMembers(members.map((o) => o.member));
					const mentionableRoles = Guilds.getGuildRoles(channel.guildId ?? '').filter(
						(role) => canMentionEveryone || role.mentionable,
					);
					const matchedRoles = queryForMatching
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
								if (!queryForMatching) return true;
								return mention.kind.slice(1).toLowerCase().includes(queryForMatching.toLowerCase());
							})
						: [];
					options = [...members, ...specialMentions, ...roles];
				}
				break;
			}
			case 'channel': {
				setAutocompleteType('channel');
				if (!channel) {
					setAutocompleteOptions([]);
					return;
				}
				options = matchSorter(Channels.getGuildChannels(channel.guildId ?? ''), matchedText ?? '', {keys: ['name']})
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
				options = buildEmojiReactionOptions({channel: channel ?? null, matchedText, i18n});
				break;
			}
			case 'emoji': {
				setAutocompleteType('emoji');
				options = buildEmojiAutocompleteOptions({
					channel: channel ?? null,
					matchedText,
					i18n,
					prefs: {
						showDefaultEmojis: Accessibility.showDefaultEmojisInExpressionAutocomplete,
						showCustomEmojis: Accessibility.showCustomEmojisInExpressionAutocomplete,
						showStickers: Accessibility.showStickersInExpressionAutocomplete,
						showMemes: Accessibility.showMemesInExpressionAutocomplete,
					},
				});
				break;
			}
			case 'command': {
				setAutocompleteType('command');
				const filteredCommands = filterCommandsByQuery(commands, matchedText ?? '').filter(canUseCommand);
				options = filteredCommands.map((command) => ({
					type: 'command' as const,
					command,
				}));
				break;
			}
			case 'meme': {
				setAutocompleteType('meme');
				const searchQuery = autocompleteTrigger.match[2].trim();
				const allMemes = FavoriteMemes.getAllMemes();
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
							title: gif.title || KlipyUtils.parseTitleFromUrl(gif.url),
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
				let results: ReadonlyArray<GuildSticker>;
				if (!searchQuery) {
					const allStickers = Sticker.searchWithChannel(channel ?? null, '');
					const filteredStickers = filterStickersForAutocomplete(i18n, allStickers, channel ?? null);
					results = StickerPicker.getFrecentStickers(filteredStickers, 10);
					if (results.length < 10) {
						const remainingCount = 10 - results.length;
						const otherStickers = filteredStickers
							.filter((sticker) => !results.some((r) => r.id === sticker.id))
							.slice(0, remainingCount);
						results = [...results, ...otherStickers];
					}
				} else {
					const allStickersSearch = Sticker.searchWithChannel(channel ?? null, searchQuery);
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
		autocompleteTriggerToken,
		canMentionEveryone,
		matchedText,
		hasOpenCodeBlock,
		gifState,
		canUseCommand,
		canManageUser,
		memberSearchResults,
		i18n,
		expressionDataVersion,
		permissionVersion,
	]);
	const applyAutocompleteValue = useCallback(
		(nextValue: string, nextSegments: ReadonlyArray<MentionSegment>, selectionStart = nextValue.length) => {
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue,
				nextSegments,
				selectionStart,
			});
		},
		[textareaRef, setValue, segmentManagerRef, previousValueRef, prepareTextChange],
	);
	const handleSelect = useCallback(
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
					const messages = Messages.getMessages(channel.id).toArray();
					const mostRecentMessage = messages[messages.length - 1];
					if (mostRecentMessage) {
						ReactionCommands.addReaction(i18n, channel.id, mostRecentMessage.id, toReactionEmoji(option.emoji));
					}
				}
				applyAutocompleteValue('', [], 0);
				setSelectedIndex(0);
				return;
			}
			if (isMeme(option)) {
				ComponentBus.dispatch('FAVORITE_MEME_SELECT', {meme: option.meme, autoSend: true});
				applyAutocompleteValue('', [], 0);
				setSelectedIndex(0);
				return;
			}
			if (isGif(option)) {
				ComponentBus.dispatch('GIF_SELECT', {gif: option.gif, autoSend: true});
				applyAutocompleteValue('', [], 0);
				setSelectedIndex(0);
				return;
			}
			if (isSticker(option)) {
				ComponentBus.dispatch('STICKER_SELECT', {sticker: option.sticker});
				applyAutocompleteValue('', [], 0);
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
				displayText = `@${NicknameUtils.formatUserTagForStreamerMode(user)}`;
				actualText = `<@${user.id}>`;
				segmentType = 'user';
				segmentId = user.id;
				MentionFrecency.recordMention(channel?.guildId ?? null, user.id);
			} else if (isMentionUser(option)) {
				displayText = `@${NicknameUtils.formatUserTagForStreamerMode(option.user)}`;
				actualText = `<@${option.user.id}>`;
				segmentType = 'user';
				segmentId = option.user.id;
				MentionFrecency.recordMention(channel?.guildId ?? null, option.user.id);
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
				actualText = Emoji.getEmojiMarkdown(option.emoji);
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
						const segmentManager = new TextareaSegmentManager();
						segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
						if (trimmedChars > 0) {
							const segments = segmentManager.getSegments();
							const adjustedSegments = segments.map((seg) => ({
								...seg,
								start: seg.start - trimmedChars,
								end: seg.end - trimmedChars,
							}));
							segmentManager.setSegments(adjustedSegments);
						}
						applyAutocompleteValue(
							trimmedValue,
							segmentManager.getSegmentsCopy(),
							Math.max(0, newCursorPosition - trimmedChars),
						);
						setSelectedIndex(0);
						return;
					}
					const newValue = `${beforeMatch}${guildBeforeMatch}${commandText} ${afterMatch}`;
					const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + commandText.length + 1;
					const trimmedValue = newValue.trimStart();
					const trimmedChars = newValue.length - trimmedValue.length;
					const segmentManager = new TextareaSegmentManager();
					segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
					if (trimmedChars > 0) {
						const segments = segmentManager.getSegments();
						const adjustedSegments = segments.map((seg) => ({
							...seg,
							start: seg.start - trimmedChars,
							end: seg.end - trimmedChars,
						}));
						segmentManager.setSegments(adjustedSegments);
					}
					applyAutocompleteValue(
						trimmedValue,
						segmentManager.getSegmentsCopy(),
						Math.max(0, newCursorPosition - trimmedChars),
					);
					setSelectedIndex(0);
					return;
				} else {
					const insertionText = getCommandInsertionText(option.command);
					const newValue = `${beforeMatch}${guildBeforeMatch}${insertionText}${afterMatch}`;
					const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + insertionText.length;
					const trimmedValue = newValue.trimStart();
					const trimmedChars = newValue.length - trimmedValue.length;
					const segmentManager = new TextareaSegmentManager();
					segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
					if (trimmedChars > 0) {
						const segments = segmentManager.getSegments();
						const adjustedSegments = segments.map((seg) => ({
							...seg,
							start: seg.start - trimmedChars,
							end: seg.end - trimmedChars,
						}));
						segmentManager.setSegments(adjustedSegments);
					}
					applyAutocompleteValue(
						trimmedValue,
						segmentManager.getSegmentsCopy(),
						Math.max(0, newCursorPosition - trimmedChars),
					);
					setSelectedIndex(0);
					return;
				}
			}
			const segmentManager = new TextareaSegmentManager();
			segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
			const segmentsBeforeInsert = maxActualLength != null ? segmentManager.getSegmentsCopy() : null;
			const changeStart = beforeMatch.length;
			const changeEnd = matchEnd;
			segmentManager.updateSegmentsForTextChange(changeStart, changeEnd, guildBeforeMatch.length);
			const tempText = `${beforeMatch}${guildBeforeMatch}`;
			const {newText: updatedText} = segmentManager.insertSegment(
				tempText,
				insertPosition,
				displayText,
				actualText,
				segmentType,
				segmentId,
			);
			const spaceInsertPosition = insertPosition + displayText.length;
			segmentManager.updateSegmentsForTextChange(spaceInsertPosition, spaceInsertPosition, 1);
			const finalText = `${updatedText} ${afterMatch}`;
			const newCursorPosition = beforeMatch.length + guildBeforeMatch.length + displayText.length + 1;
			const trimmedValue = finalText.trimStart();
			const trimmedChars = finalText.length - trimmedValue.length;
			if (trimmedChars > 0) {
				const segments = segmentManager.getSegments();
				const adjustedSegments = segments.map((seg) => ({
					...seg,
					start: seg.start - trimmedChars,
					end: seg.end - trimmedChars,
				}));
				segmentManager.setSegments(adjustedSegments);
			}
			if (maxActualLength != null && segmentType === 'emoji') {
				const candidateActualText = segmentManager.displayToActual(trimmedValue);
				if (candidateActualText.length > maxActualLength) {
					if (segmentsBeforeInsert) {
						segmentManager.setSegments(segmentsBeforeInsert);
					}
					onExceedMaxLength?.();
					return;
				}
			}
			applyAutocompleteValue(
				trimmedValue,
				segmentManager.getSegmentsCopy(),
				Math.max(0, newCursorPosition - trimmedChars),
			);
			setSelectedIndex(0);
			HighlightCommands.clearChannelHighlight();
		},
		[
			value,
			autocompleteTrigger,
			textareaRef,
			segmentManagerRef,
			applyAutocompleteValue,
			i18n,
			maxActualLength,
			onExceedMaxLength,
			channel,
		],
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
