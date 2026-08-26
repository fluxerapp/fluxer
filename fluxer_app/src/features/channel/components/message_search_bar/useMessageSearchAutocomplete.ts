// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AutocompleteOption,
	AutocompleteType,
	HistoryFilterRow,
	PlaintextAutocompleteRow,
	SearchHints,
} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';
import {
	assignRef,
	buildHistoryFilterRows,
	buildUserSearchBoosters,
	filterRequiresValue,
	getChannelSuggestionSearchPlan,
	getUserGuildSearchPlan,
	isDateFilterKey,
	isSearchFilterOptionEligible,
	isSearchValueMode,
	isUserFilterKey,
	normalizeFilterKey,
	orderInlineFilterOptions,
	PLAINTEXT_SUGGESTION_FILTER_KEYS,
	PLAINTEXT_SUGGESTIONS_PER_FILTER,
	replaceSearchTokenAtCursor,
	resolveChannelSuggestionDisplayName,
	resolveMessageSearchCurrentWord,
	type SearchTokenReplacementResult,
} from '@app/features/channel/components/message_search_bar/MessageSearchBarUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelSearch from '@app/features/channel/state/ChannelSearch';
import Channels from '@app/features/channel/state/Channels';
import type {LexicalSearchInputHandle} from '@app/features/lexical/search/LexicalSearchInput';
import MemberSearch, {type SearchContext} from '@app/features/member/state/MemberSearch';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import SearchHistory, {
	SEARCH_HISTORY_DISPLAY_LIMIT,
	type SearchHistoryEntry,
} from '@app/features/search/state/SearchHistory';
import {
	buildSearchSegmentsFromHints,
	formatSearchHistoryEntryForStreamerMode,
} from '@app/features/search/utils/SearchPrivacyUtils';
import {matchSearchDateWords} from '@app/features/search/utils/SearchQueryParser';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import type {MessageSearchScope, SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import {getSearchFilterOptions} from '@app/features/search/utils/SearchUtils';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {getRelativeDayLabelCapitalized, getRelativeTimeFormat} from '@app/features/ui/utils/RelativeDayLabels';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DateTime} from 'luxon';
import {matchSorter} from 'match-sorter';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const MAX_FILTER_VALUE_SUGGESTIONS = 12;
const MAX_MEMBER_SEARCH_RESULTS = 25;
const MAX_DATE_WORD_SUGGESTIONS = 10;

function isDmSearchChannel(channel: Channel): boolean {
	return channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
}

function isGuildSearchChannel(channel: Channel): boolean {
	return (
		channel.guildId != null &&
		GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) &&
		Permission.can(Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY, channel)
	);
}

function isHistoryFilterRow(option: AutocompleteOption): option is HistoryFilterRow {
	if (typeof option !== 'object' || option === null || !('kind' in option)) {
		return false;
	}
	return option.kind === 'filter';
}

const UNNAMED_DESCRIPTOR = msg({
	message: 'Unnamed',
	comment: 'Short label in the channel and chat use message search autocomplete. Keep it concise.',
});

function resolveInputCursorPosition(
	inputRef: React.MutableRefObject<LexicalSearchInputHandle | null>,
	value: string,
): number {
	const input = inputRef.current;
	if (input == null) {
		return value.length;
	}
	const selectionStart = input.selectionStart;
	if (selectionStart == null) {
		return value.length;
	}
	return selectionStart;
}

function resolveCurrentSearchSegments(contextId: string | null): ReadonlyArray<SearchSegment> {
	if (contextId == null) {
		return [];
	}
	const context = ChannelSearch.getContext(contextId);
	if (context == null) {
		return [];
	}
	return context.searchSegments;
}

interface UseMessageSearchAutocompleteParams {
	channel: Channel | undefined;
	value: string;
	onChange: (value: string, segments: Array<SearchSegment>) => void;
	onSearch: () => void;
	activeScope: MessageSearchScope;
	currentGuildIdForScope: string | undefined;
	channelGuildId: string | undefined;
	routeGuildId: string | undefined;
	isInGuildChannel: boolean;
	contextId: string | null;
	inputRefExternal?: React.Ref<LexicalSearchInputHandle>;
	isResultsOpen?: boolean;
	onCloseResults?: () => void;
}

export function useMessageSearchAutocomplete({
	channel,
	value,
	onChange,
	onSearch,
	activeScope,
	currentGuildIdForScope,
	channelGuildId,
	routeGuildId,
	isInGuildChannel,
	contextId,
	inputRefExternal,
	isResultsOpen = false,
	onCloseResults,
}: UseMessageSearchAutocompleteParams) {
	const {i18n} = useLingui();
	const [isFocused, setIsFocused] = useState(false);
	const [autocompleteType, setAutocompleteType] = useState<AutocompleteType>(null);
	const [selectedIndex, setSelectedIndex] = useState(-1);
	const [hoverIndex, setHoverIndex] = useState(-1);
	const [hasNavigated, setHasNavigated] = useState(false);
	const [hasInteracted, setHasInteracted] = useState(false);
	const [currentFilter, setCurrentFilter] = useState<SearchFilterOption | null>(null);
	const inputRef = useRef<LexicalSearchInputHandle | null>(null);
	const [suppressAutoOpen, setSuppressAutoOpen] = useState(false);
	const suppressAutoOpenRef = useRef(false);
	const hintsRef = useRef<SearchHints>({usersByTag: {}, channelsByName: {}});
	const searchContextRef = useRef<SearchContext | null>(null);
	const [memberSearchResults, setMemberSearchResults] = useState<Array<User>>([]);
	const memberFetchDebounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const memberFetchQueryRef = useRef<string>('');
	const filterOptions = useMemo(() => [...getSearchFilterOptions(i18n)], [i18n.locale]);
	const hidePersonalInformation = StreamerMode.shouldHidePersonalInformation;
	const filterEligibility = useMemo(
		() => ({isInGuildChannel, hidePersonalInformation}),
		[isInGuildChannel, hidePersonalInformation],
	);
	const historyKey = contextId ?? undefined;
	const listboxId = useMemo(() => `message-search-listbox-${channel?.id ?? 'global'}`, [channel?.id]);
	const openSearchInput = useCallback(() => {
		const el = inputRef.current;
		if (!el) return;
		if (!isInGuildChannel || !channel?.name) {
			el.focus();
			const pos = el.value.length;
			try {
				el.setSelectionRange(pos, pos);
			} catch {}
			return;
		}
		const name = channel.name;
		const display = /\s/.test(name) ? `in:"${name}"` : `in:${name}`;
		const currentSegments = resolveCurrentSearchSegments(contextId);
		const existingIn = currentSegments.find((s) => s.type === 'channel' && s.filterKey === 'in') ?? null;
		let newValue: string;
		let newSegments: Array<SearchSegment>;
		let cursorPos: number;
		if (existingIn && existingIn.id === channel.id) {
			el.focus();
			cursorPos = el.value.length;
			try {
				el.setSelectionRange(cursorPos, cursorPos);
			} catch {}
			return;
		}
		if (existingIn) {
			const before = value.slice(0, existingIn.start);
			const after = value.slice(existingIn.end);
			newValue = `${before}${display}${after}`;
			const lengthDelta = display.length - (existingIn.end - existingIn.start);
			const replacement: SearchSegment = {
				type: 'channel',
				filterKey: 'in',
				id: channel.id,
				displayText: display,
				start: existingIn.start,
				end: existingIn.start + display.length,
			};
			newSegments = currentSegments
				.filter((s) => s !== existingIn)
				.map((s) => (s.start >= existingIn.end ? {...s, start: s.start + lengthDelta, end: s.end + lengthDelta} : s));
			newSegments.push(replacement);
			cursorPos = newValue.length;
		} else if (value.length === 0) {
			newValue = `${display} `;
			newSegments = [
				{
					type: 'channel',
					filterKey: 'in',
					id: channel.id,
					displayText: display,
					start: 0,
					end: display.length,
				},
			];
			cursorPos = newValue.length;
		} else {
			const prefix = `${display} `;
			newValue = `${prefix}${value}`;
			const offset = prefix.length;
			const prepended: SearchSegment = {
				type: 'channel',
				filterKey: 'in',
				id: channel.id,
				displayText: display,
				start: 0,
				end: display.length,
			};
			newSegments = [prepended, ...currentSegments.map((s) => ({...s, start: s.start + offset, end: s.end + offset}))];
			cursorPos = newValue.length;
		}
		hintsRef.current.channelsByName[name] = channel.id;
		onChange(newValue, newSegments);
		setSuppressAutoOpen(true);
		const node = inputRef.current;
		if (node != null) {
			node.focus();
			node.setSelectionRange(cursorPos, cursorPos);
		}
	}, [channel?.id, channel?.name, contextId, isInGuildChannel, onChange, value]);
	useEffect(() => {
		return ComponentBus.subscribe('MESSAGE_SEARCH_OPEN', () => {
			openSearchInput();
		});
	}, [openSearchInput]);
	useEffect(() => {
		if (contextId == null || channelGuildId == null || contextId !== channelGuildId) return;
		SearchHistory.adoptLegacyEntries(
			contextId,
			Channels.getGuildChannels(channelGuildId).map((guildChannel) => guildChannel.id),
		);
	}, [contextId, channelGuildId]);
	useEffect(() => {
		const handleGlobalKeydown = (event: KeyboardEvent) => {
			const isFind = (event.key === 'f' || event.key === 'F') && (event.metaKey || event.ctrlKey);
			if (!isFind) return;
			event.preventDefault();
			event.stopPropagation();
			openSearchInput();
		};
		document.addEventListener('keydown', handleGlobalKeydown, true);
		return () => document.removeEventListener('keydown', handleGlobalKeydown, true);
	}, [openSearchInput]);
	useEffect(() => {
		suppressAutoOpenRef.current = suppressAutoOpen;
	}, [suppressAutoOpen]);
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			const users = results.map((result) => Users.getUser(result.id)).filter((u): u is User => u != null);
			setMemberSearchResults(users);
		}, MAX_MEMBER_SEARCH_RESULTS);
		searchContextRef.current = context;
		return () => {
			context.destroy();
			searchContextRef.current = null;
		};
	}, []);
	useEffect(() => {
		if (memberFetchDebounceTimerRef.current) {
			clearTimeout(memberFetchDebounceTimerRef.current);
			memberFetchDebounceTimerRef.current = null;
		}
		const isPlaintextSuggestionMode = autocompleteType === 'plaintext';
		const isUserFilterMode =
			autocompleteType === 'users' && currentFilter != null && isUserFilterKey(currentFilter.key);
		if (!isPlaintextSuggestionMode && !isUserFilterMode) {
			memberFetchQueryRef.current = '';
			const context = searchContextRef.current;
			if (context) {
				context.cancelSearch();
			}
			setMemberSearchResults([]);
			return;
		}
		const plan = getUserGuildSearchPlan(activeScope, currentGuildIdForScope);
		if (plan.mode === 'none') {
			memberFetchQueryRef.current = '';
			const context = searchContextRef.current;
			if (context) {
				context.cancelSearch();
			}
			setMemberSearchResults([]);
			return;
		}
		const cursorPos = resolveInputCursorPosition(inputRef, value);
		const currentWord = resolveMessageSearchCurrentWord({value, cursorPosition: cursorPos});
		const searchQuery = isPlaintextSuggestionMode
			? currentWord.trim()
			: currentWord.slice(currentFilter?.syntax.length ?? 0).trim();
		const context = searchContextRef.current;
		if (searchQuery.length === 0) {
			memberFetchQueryRef.current = '';
			if (context) {
				context.cancelSearch();
			}
			setMemberSearchResults([]);
			return;
		}
		const boosters = buildUserSearchBoosters(channel, currentGuildIdForScope, plan.mode);
		if (context) {
			context.beginSearch(searchQuery, plan.workerFilters, new Set(), new Set(), boosters);
		}
		if (!plan.guildsToSearch || plan.guildsToSearch.length === 0) {
			memberFetchQueryRef.current = searchQuery;
			return;
		}
		memberFetchQueryRef.current = searchQuery;
		const scheduledQuery = searchQuery;
		memberFetchDebounceTimerRef.current = setTimeout(() => {
			memberFetchDebounceTimerRef.current = null;
			if (!isPlaintextSuggestionMode && !isUserFilterMode) {
				return;
			}
			if (memberFetchQueryRef.current !== scheduledQuery) {
				return;
			}
			const guildIds = plan.guildsToSearch == null ? [] : plan.guildsToSearch.map((guild) => guild.id);
			const priorityGuildId = plan.priorityGuildId;
			void MemberSearch.fetchMembersInBackground(scheduledQuery, guildIds, priorityGuildId);
		}, 300);
		return () => {
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
		};
	}, [autocompleteType, currentFilter, value, activeScope, channel, currentGuildIdForScope]);
	useEffect(() => {
		if (autocompleteType !== 'plaintext' && (autocompleteType !== 'users' || !currentFilter)) {
			const context = searchContextRef.current;
			if (context) {
				context.cancelSearch();
			}
			setMemberSearchResults([]);
			memberFetchQueryRef.current = '';
			if (memberFetchDebounceTimerRef.current) {
				clearTimeout(memberFetchDebounceTimerRef.current);
				memberFetchDebounceTimerRef.current = null;
			}
		}
	}, [autocompleteType, currentFilter]);
	const getAutocompleteTypeForFilter = useCallback(
		(filter: SearchFilterOption): AutocompleteType => {
			const keyBase = normalizeFilterKey(filter.key);
			switch (keyBase) {
				case 'before':
				case 'after':
				case 'during':
				case 'on':
					return 'date';
				case 'from':
				case 'mentions':
					return 'users';
				case 'in':
					return 'channels';
				default:
					return 'values';
			}
		},
		[isInGuildChannel],
	);
	const resolveUserSuggestions = useCallback(
		(searchTerm: string, limit: number): Array<User> => {
			const plan = getUserGuildSearchPlan(activeScope, currentGuildIdForScope);
			if (plan.mode !== 'none') {
				return memberSearchResults.slice(0, limit);
			}
			if (channel) {
				const users = channel.recipientIds.map((id) => Users.getUser(id)).filter((u): u is User => u != null);
				return matchSorter(users, searchTerm, {
					keys: ['username', 'tag'],
				}).slice(0, limit);
			}
			return [];
		},
		[activeScope, currentGuildIdForScope, memberSearchResults, channel],
	);
	const resolveChannelSuggestions = useCallback(
		(searchTerm: string, limit: number): Array<Channel> => {
			const currentGuildId = channelGuildId ?? (routeGuildId === ME ? undefined : routeGuildId);
			const plan = getChannelSuggestionSearchPlan(activeScope, currentGuildId);
			const candidatesById = new Map<string, Channel>();
			const addCandidate = (candidate: Channel | undefined) => {
				if (candidate) {
					candidatesById.set(candidate.id, candidate);
				}
			};
			if (!hidePersonalInformation) {
				switch (plan.dmMode) {
					case 'none':
						break;
					case 'current':
						if (channel && channel.guildId == null) {
							addCandidate(channel);
						}
						break;
					case 'open':
					case 'all':
						for (const privateChannel of Channels.getPrivateChannels()) {
							addCandidate(privateChannel);
						}
						if (channel && isDmSearchChannel(channel)) {
							addCandidate(channel);
						}
						break;
					default: {
						const exhaustiveDmMode: never = plan.dmMode;
						throw new Error(`Unsupported DM channel suggestion mode: ${exhaustiveDmMode}`);
					}
				}
			}
			switch (plan.guildMode) {
				case 'none':
					break;
				case 'current_guild':
					if (!plan.currentGuildId) {
						throw new Error('Current guild channel suggestion mode requires a guild ID');
					}
					for (const guildChannel of Channels.getGuildChannels(plan.currentGuildId)) {
						if (isGuildSearchChannel(guildChannel)) {
							addCandidate(guildChannel);
						}
					}
					break;
				case 'all_guilds':
					for (const guildChannels of Channels.channelGroups.byGuild.values()) {
						for (const guildChannel of guildChannels) {
							if (isGuildSearchChannel(guildChannel)) {
								addCandidate(guildChannel);
							}
						}
					}
					break;
				default: {
					const exhaustiveGuildMode: never = plan.guildMode;
					throw new Error(`Unsupported guild channel suggestion mode: ${exhaustiveGuildMode}`);
				}
			}
			const candidates = Array.from(candidatesById.values()).filter(
				(candidate) => resolveChannelSuggestionDisplayName(candidate) !== '',
			);
			const recencyRank = new Map<string, number>();
			SelectedChannel.sortedRecentVisits.forEach((visit, index) => {
				if (!recencyRank.has(visit.channelId)) {
					recencyRank.set(visit.channelId, index);
				}
			});
			const currentChannelId = channel?.id;
			const matches = matchSorter(candidates, searchTerm, {
				keys: [(candidate: Channel) => resolveChannelSuggestionDisplayName(candidate)],
			});
			const matchRank = new Map(matches.map((candidate, index) => [candidate.id, index]));
			const orderedMatches = [...matches].sort((a, b) => {
				const resolveRank = (ch: Channel) => {
					if (ch.id === currentChannelId) return -1;
					return recencyRank.get(ch.id) ?? Number.MAX_SAFE_INTEGER;
				};
				const rankDifference = resolveRank(a) - resolveRank(b);
				if (rankDifference !== 0) {
					return rankDifference;
				}
				return (matchRank.get(a.id) ?? Number.MAX_SAFE_INTEGER) - (matchRank.get(b.id) ?? Number.MAX_SAFE_INTEGER);
			});
			return orderedMatches.slice(0, limit);
		},
		[activeScope, channelGuildId, routeGuildId, channel, hidePersonalInformation],
	);
	const getPlaintextRows = useCallback((): Array<PlaintextAutocompleteRow> => {
		const cursorPosition = resolveInputCursorPosition(inputRef, value);
		const currentWord = resolveMessageSearchCurrentWord({value, cursorPosition});
		const rows: Array<PlaintextAutocompleteRow> = [];
		const searchTerm = currentWord.trim();
		if (searchTerm === '') return rows;
		let suggestionCount = 0;
		for (const filterKey of PLAINTEXT_SUGGESTION_FILTER_KEYS) {
			const filter = filterOptions.find((opt) => opt.key === filterKey);
			if (filter == null) continue;
			if (!isSearchFilterOptionEligible(filter, filterEligibility)) continue;
			if (filterKey === 'in') {
				for (const suggestion of resolveChannelSuggestions(searchTerm, PLAINTEXT_SUGGESTIONS_PER_FILTER)) {
					rows.push({kind: 'channel-suggestion', filterKey, channel: suggestion});
					suggestionCount += 1;
				}
				continue;
			}
			for (const suggestion of resolveUserSuggestions(searchTerm, PLAINTEXT_SUGGESTIONS_PER_FILTER)) {
				rows.push({kind: 'user-suggestion', filterKey, user: suggestion});
				suggestionCount += 1;
			}
		}
		if (suggestionCount === 0) {
			for (const filter of orderInlineFilterOptions(filterOptions, filterEligibility)) {
				rows.push({kind: 'filter-key', filter});
			}
		}
		return rows;
	}, [value, filterOptions, filterEligibility, resolveChannelSuggestions, resolveUserSuggestions]);
	const resolveAutocompleteOptions = useCallback((): Array<AutocompleteOption> => {
		const cursorPos = resolveInputCursorPosition(inputRef, value);
		const currentWord = resolveMessageSearchCurrentWord({value, cursorPosition: cursorPos});
		switch (autocompleteType) {
			case 'filters': {
				const filtered = filterOptions.filter((opt) => {
					if (!isSearchFilterOptionEligible(opt, filterEligibility)) return false;
					if (!currentWord) {
						return !opt.key.startsWith('-');
					}
					const currentWordLower = currentWord.toLowerCase();
					if (currentWordLower.startsWith('-')) {
						return (
							(opt.key.startsWith('-') && currentWordLower === '-') ||
							currentWordLower.startsWith(opt.syntax.toLowerCase())
						);
					}
					if (opt.key.startsWith('-')) {
						return false;
					}
					return opt.syntax.toLowerCase().includes(currentWordLower);
				});
				const MAX_TYPED_FILTERS = 15;
				return currentWord ? filtered.slice(0, MAX_TYPED_FILTERS) : filtered;
			}
			case 'history': {
				return [...SearchHistory.search(currentWord, historyKey)].slice(0, SEARCH_HISTORY_DISPLAY_LIMIT);
			}
			case 'users': {
				if (!currentFilter) return [];
				const searchTerm = currentWord.slice(currentFilter.syntax.length);
				return resolveUserSuggestions(searchTerm, MAX_FILTER_VALUE_SUGGESTIONS);
			}
			case 'channels': {
				if (!currentFilter) return [];
				const searchTerm = currentWord.slice(currentFilter.syntax.length);
				return resolveChannelSuggestions(searchTerm, MAX_FILTER_VALUE_SUGGESTIONS);
			}
			case 'values': {
				if (!currentFilter?.values) return [];
				const searchTerm = currentWord.slice(currentFilter.syntax.length);
				const matches = matchSorter(currentFilter.values, searchTerm, {
					keys: ['value', 'label', 'description'],
				});
				const matchValues = new Set(matches.map((option) => option.value));
				return currentFilter.values.filter((option) => matchValues.has(option.value));
			}
			case 'date': {
				const dateTerm = currentFilter == null ? '' : currentWord.slice(currentFilter.syntax.length).trim();
				if (dateTerm !== '') {
					return matchSearchDateWords(dateTerm, MAX_DATE_WORD_SUGGESTIONS).map((word) => ({
						label: word,
						value: word,
					}));
				}
				const now = DateTime.local();
				const fmtDate = (dt: DateTime) => dt.toFormat('yyyy-MM-dd');
				const fmtDateTime = (dt: DateTime) => dt.toFormat("yyyy-MM-dd'T'HH:mm");
				const locale = getCurrentLocale();
				return [
					{label: getRelativeDayLabelCapitalized(locale, 0), value: fmtDate(now)},
					{label: getRelativeDayLabelCapitalized(locale, -1), value: fmtDate(now.minus({days: 1}))},
					{label: getRelativeTimeFormat(locale).format(0, 'second'), value: fmtDateTime(now)},
				];
			}
			case 'plaintext':
				return getPlaintextRows();
			default:
				return [];
		}
	}, [
		autocompleteType,
		value,
		filterOptions,
		isInGuildChannel,
		currentFilter,
		channel,
		i18n,
		getPlaintextRows,
		resolveChannelSuggestions,
		resolveUserSuggestions,
	]);
	const historyFilterRows = useMemo(
		() => buildHistoryFilterRows(filterOptions, filterEligibility),
		[filterOptions, filterEligibility],
	);
	const autocompleteOptions = resolveAutocompleteOptions();
	const totalOptions = autocompleteType
		? autocompleteOptions.length + (autocompleteType === 'history' ? historyFilterRows.length : 0)
		: 0;
	const getAutocompleteOptions = useCallback(
		(): Array<AutocompleteOption> => autocompleteOptions,
		[autocompleteOptions],
	);
	const getTotalOptions = useCallback((): number => {
		return totalOptions;
	}, [totalOptions]);
	const hasAnyOptions = useCallback((): boolean => {
		return getTotalOptions() > 0;
	}, [getTotalOptions]);
	const getSelectedOption = useCallback((): AutocompleteOption | null => {
		if (selectedIndex < 0) return null;
		if (autocompleteType === 'history') {
			if (selectedIndex < historyFilterRows.length) {
				return historyFilterRows[selectedIndex] ?? null;
			}
			const historyIndex = selectedIndex - historyFilterRows.length;
			return autocompleteOptions[historyIndex] ?? null;
		}
		return autocompleteOptions[selectedIndex] ?? null;
	}, [selectedIndex, autocompleteType, autocompleteOptions, historyFilterRows]);
	useEffect(() => {
		if (!isFocused || suppressAutoOpen) {
			setAutocompleteType(null);
			setCurrentFilter(null);
			setSelectedIndex(-1);
			setHoverIndex(-1);
			setHasNavigated(false);
			setHasInteracted(false);
			return;
		}
		const cursorPos = resolveInputCursorPosition(inputRef, value);
		const currentWord = resolveMessageSearchCurrentWord({value, cursorPosition: cursorPos});
		const matchingFilter = filterOptions.find((opt) => currentWord.startsWith(opt.syntax));
		if (matchingFilter) {
			const afterColon = currentWord.slice(matchingFilter.syntax.length);
			const filterKeyBase = normalizeFilterKey(matchingFilter.key);
			if (!isSearchFilterOptionEligible(matchingFilter, filterEligibility)) {
				setAutocompleteType(null);
				setCurrentFilter(null);
				return;
			}
			if (isDateFilterKey(filterKeyBase)) {
				setAutocompleteType('date');
				setCurrentFilter(matchingFilter);
				setSelectedIndex(0);
				setHasNavigated(false);
				return;
			}
			if (matchingFilter.values && afterColon.length === 0) {
				setAutocompleteType('values');
				setCurrentFilter(matchingFilter);
				setSelectedIndex(0);
				setHasNavigated(false);
				return;
			}
			if (filterKeyBase === 'from' || filterKeyBase === 'mentions') {
				setAutocompleteType('users');
				setCurrentFilter(matchingFilter);
				setSelectedIndex(0);
				setHasNavigated(false);
				return;
			}
			if (filterKeyBase === 'in') {
				setAutocompleteType('channels');
				setCurrentFilter(matchingFilter);
				setSelectedIndex(0);
				setHasNavigated(false);
				return;
			}
			if (matchingFilter.values) {
				setAutocompleteType('values');
				setCurrentFilter(matchingFilter);
				setSelectedIndex(0);
				setHasNavigated(false);
				return;
			}
			setAutocompleteType(null);
			setCurrentFilter(null);
			return;
		}
		if (currentWord === '') {
			setAutocompleteType('history');
			setCurrentFilter(null);
			return;
		}
		const partialMatch = filterOptions.some((opt) => {
			return opt.syntax.includes(currentWord) || opt.key.includes(currentWord);
		});
		setAutocompleteType(partialMatch ? 'filters' : 'plaintext');
		setCurrentFilter(null);
	}, [value, isFocused, isInGuildChannel, suppressAutoOpen, filterOptions, filterEligibility]);
	useEffect(() => {
		if (totalOptions > 0 && (selectedIndex >= totalOptions || selectedIndex < -1)) {
			setSelectedIndex(-1);
		}
	}, [selectedIndex, totalOptions]);
	const handleOptionMouseEnter = (index: number) => {
		setHoverIndex(index);
		setHasInteracted(true);
	};
	const handleOptionMouseLeave = () => {
		setHoverIndex(-1);
	};
	const shouldShowKeyboardFocus = hasNavigated || isSearchValueMode(autocompleteType);
	const shouldShowHover = hasInteracted;
	const keyboardFocusIndex = shouldShowKeyboardFocus ? selectedIndex : -1;
	const hoverIndexForRender = shouldShowHover ? hoverIndex : -1;
	const getAriaActiveDescendant = useCallback((): string | undefined => {
		if (!isFocused || autocompleteType === null) return undefined;
		const totalOptions = getTotalOptions();
		if (totalOptions <= 0) return undefined;
		const showFocus = shouldShowKeyboardFocus || shouldShowHover;
		if (!showFocus) return undefined;
		if (selectedIndex < 0) return undefined;
		if (getSelectedOption() == null) return undefined;
		return `${listboxId}-opt-${selectedIndex}`;
	}, [
		isFocused,
		autocompleteType,
		getTotalOptions,
		shouldShowKeyboardFocus,
		shouldShowHover,
		selectedIndex,
		listboxId,
		getSelectedOption,
	]);
	const handleAutocompleteSelect = (option: AutocompleteOption) => {
		const cursorPosition = resolveInputCursorPosition(inputRef, value);
		const currentSegments = resolveCurrentSearchSegments(contextId);
		let replacement: SearchTokenReplacementResult;
		let shouldSubmit = false;
		const requireCurrentFilter = (): SearchFilterOption => {
			if (currentFilter == null) {
				let filterContext = 'unknown';
				if (autocompleteType != null) {
					filterContext = autocompleteType;
				}
				throw new Error(`Missing active filter for ${filterContext} message search autocomplete`);
			}
			return currentFilter;
		};
		switch (autocompleteType) {
			case 'filters': {
				const filter = option as SearchFilterOption;
				const requiresValue = filterRequiresValue(filter);
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: '',
					addSpaceAfter: !requiresValue,
					replacementSegment: null,
				});
				shouldSubmit = !requiresValue;
				break;
			}
			case 'users': {
				const user = option as User;
				const filter = requireCurrentFilter();
				const tag = NicknameUtils.formatUserTagForStreamerMode(user);
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: tag,
					addSpaceAfter: true,
					replacementSegment: {type: 'user', filterKey: filter.key, id: user.id},
				});
				hintsRef.current.usersByTag[tag] = user.id;
				shouldSubmit = true;
				break;
			}
			case 'channels': {
				const ch = option as Channel;
				const filter = requireCurrentFilter();
				const displayName = resolveChannelSuggestionDisplayName(ch);
				const name = displayName === '' ? i18n._(UNNAMED_DESCRIPTOR) : displayName;
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: name,
					addSpaceAfter: true,
					replacementSegment: {type: 'channel', filterKey: filter.key, id: ch.id},
				});
				hintsRef.current.channelsByName[name] = ch.id;
				shouldSubmit = true;
				break;
			}
			case 'values': {
				const valueOption = option as {
					value: string;
					label: string;
				};
				const filter = requireCurrentFilter();
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: valueOption.value,
					addSpaceAfter: true,
					replacementSegment: null,
				});
				shouldSubmit = true;
				break;
			}
			case 'date': {
				const dateOption = option as {
					value: string;
					label: string;
				};
				const filter = requireCurrentFilter();
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: dateOption.value,
					addSpaceAfter: true,
					replacementSegment: null,
				});
				shouldSubmit = true;
				break;
			}
			case 'plaintext': {
				const row = option as PlaintextAutocompleteRow;
				const filter = filterOptions.find(
					(opt) => opt.key === (row.kind === 'filter-key' ? row.filter.key : row.filterKey),
				);
				if (filter == null) return;
				if (row.kind === 'filter-key') {
					const requiresValue = filterRequiresValue(filter);
					replacement = replaceSearchTokenAtCursor({
						value,
						cursorPosition,
						currentSegments,
						syntax: filter.syntax,
						tokenValue: '',
						addSpaceAfter: !requiresValue,
						replacementSegment: null,
					});
					shouldSubmit = !requiresValue;
					break;
				}
				if (row.kind === 'user-suggestion') {
					const tag = NicknameUtils.formatUserTagForStreamerMode(row.user);
					replacement = replaceSearchTokenAtCursor({
						value,
						cursorPosition,
						currentSegments,
						syntax: filter.syntax,
						tokenValue: tag,
						addSpaceAfter: true,
						replacementSegment: {type: 'user', filterKey: filter.key, id: row.user.id},
					});
					hintsRef.current.usersByTag[tag] = row.user.id;
					shouldSubmit = true;
					break;
				}
				const rowDisplayName = resolveChannelSuggestionDisplayName(row.channel);
				const channelName = rowDisplayName === '' ? i18n._(UNNAMED_DESCRIPTOR) : rowDisplayName;
				replacement = replaceSearchTokenAtCursor({
					value,
					cursorPosition,
					currentSegments,
					syntax: filter.syntax,
					tokenValue: channelName,
					addSpaceAfter: true,
					replacementSegment: {type: 'channel', filterKey: filter.key, id: row.channel.id},
				});
				hintsRef.current.channelsByName[channelName] = row.channel.id;
				shouldSubmit = true;
				break;
			}
			case 'history': {
				const entry = formatSearchHistoryEntryForStreamerMode(option as SearchHistoryEntry);
				const newText = entry.query;
				const newCursorPos = newText.length;
				const segments = buildSearchSegmentsFromHints(newText, entry.hints);
				onChange(newText, segments);
				SearchHistory.add(newText, historyKey, entry.hints);
				const historyInput = inputRef.current;
				if (historyInput != null) {
					historyInput.focus();
					historyInput.setSelectionRange(newCursorPos, newCursorPos);
				}
				setSelectedIndex(-1);
				setAutocompleteType(null);
				setCurrentFilter(null);
				setSuppressAutoOpen(true);
				setTimeout(() => onSearch(), 0);
				return;
			}
			default:
				return;
		}
		onChange(replacement.newText, replacement.newSegments);
		const replacementInput = inputRef.current;
		if (replacementInput != null) {
			replacementInput.focus();
			replacementInput.setSelectionRange(replacement.newCursorPosition, replacement.newCursorPosition);
		}
		setSelectedIndex(-1);
		setAutocompleteType(null);
		setCurrentFilter(null);
		if (shouldSubmit && replacement.newText.trim().length > 0) {
			SearchHistory.add(replacement.newText, historyKey, hintsRef.current);
			setSuppressAutoOpen(true);
			setTimeout(() => onSearch(), 0);
		}
	};
	const applyHistoryFilterRow = (row: HistoryFilterRow) => {
		const filter = row.option;
		const cursorPos = resolveInputCursorPosition(inputRef, value);
		const requiresValue = filterRequiresValue(filter);
		const currentSegments = resolveCurrentSearchSegments(contextId);
		const replacement = replaceSearchTokenAtCursor({
			value,
			cursorPosition: cursorPos,
			currentSegments,
			syntax: filter.syntax,
			tokenValue: '',
			addSpaceAfter: !requiresValue,
			replacementSegment: null,
		});
		onChange(replacement.newText, replacement.newSegments);
		const filterInput = inputRef.current;
		if (filterInput != null) {
			filterInput.focus();
			filterInput.setSelectionRange(replacement.newCursorPosition, replacement.newCursorPosition);
		}
		if (!requiresValue) {
			setTimeout(() => {
				SearchHistory.add(replacement.newText, historyKey, hintsRef.current);
				setSuppressAutoOpen(true);
				setTimeout(() => onSearch(), 0);
				setAutocompleteType(null);
				setCurrentFilter(null);
			}, 10);
			return;
		}
		setCurrentFilter(filter);
		setAutocompleteType(getAutocompleteTypeForFilter(filter));
		setSelectedIndex(-1);
		setHasNavigated(false);
	};
	const handleKeyDown = (e: React.KeyboardEvent<HTMLElement>) => {
		if (isIMEComposing(e)) {
			return;
		}
		if (e.key === '?' && autocompleteType === null) {
			e.preventDefault();
			return;
		}
		if (e.key === 'Enter' && (autocompleteType === null || (autocompleteType === 'plaintext' && !hasNavigated))) {
			e.preventDefault();
			SearchHistory.add(value, historyKey, hintsRef.current);
			setAutocompleteType(null);
			setCurrentFilter(null);
			setHasNavigated(false);
			setSuppressAutoOpen(true);
			suppressAutoOpenRef.current = true;
			onSearch();
			return;
		}
		if (e.key === 'Escape') {
			e.preventDefault();
			if (autocompleteType !== null && getTotalOptions() > 0) {
				setAutocompleteType(null);
				setCurrentFilter(null);
				setSelectedIndex(-1);
				setHasNavigated(false);
				setSuppressAutoOpen(true);
				suppressAutoOpenRef.current = true;
				return;
			}
			if (value.length > 0) {
				onChange('', []);
				setAutocompleteType(null);
				setCurrentFilter(null);
				setSelectedIndex(-1);
				setHasNavigated(false);
				setSuppressAutoOpen(true);
				suppressAutoOpenRef.current = true;
				return;
			}
			if (isResultsOpen) {
				setAutocompleteType(null);
				setCurrentFilter(null);
				onCloseResults?.();
				return;
			}
			setAutocompleteType(null);
			setCurrentFilter(null);
			inputRef.current?.blur();
			return;
		}
		if (!autocompleteType) {
			return;
		}
		const totalOptions = getTotalOptions();
		if (totalOptions <= 0) {
			if (e.key === 'Enter') {
				e.preventDefault();
				SearchHistory.add(value, historyKey, hintsRef.current);
				setAutocompleteType(null);
				setCurrentFilter(null);
				setHasNavigated(false);
				setSuppressAutoOpen(true);
				suppressAutoOpenRef.current = true;
				onSearch();
			}
			return;
		}
		switch (e.key) {
			case 'ArrowDown': {
				e.preventDefault();
				setSelectedIndex((prev) => (prev + 1) % totalOptions);
				setHasNavigated(true);
				return;
			}
			case 'ArrowUp': {
				e.preventDefault();
				setSelectedIndex((prev) => {
					if (prev === -1) return totalOptions - 1;
					return (prev - 1 + totalOptions) % totalOptions;
				});
				setHasNavigated(true);
				return;
			}
			case 'Tab': {
				setAutocompleteType(null);
				setCurrentFilter(null);
				setHasNavigated(false);
				return;
			}
			case 'Enter': {
				e.preventDefault();
				const shouldAutoSelect = hasNavigated || isSearchValueMode(autocompleteType);
				if (!shouldAutoSelect) {
					SearchHistory.add(value, historyKey, hintsRef.current);
					setAutocompleteType(null);
					setCurrentFilter(null);
					setHasNavigated(false);
					setSuppressAutoOpen(true);
					suppressAutoOpenRef.current = true;
					setTimeout(() => onSearch(), 0);
					return;
				}
				const selected = getSelectedOption();
				if (selected) {
					if (autocompleteType === 'history' && isHistoryFilterRow(selected)) {
						applyHistoryFilterRow(selected);
						return;
					}
					handleAutocompleteSelect(selected);
				}
				return;
			}
			default:
				return;
		}
	};
	const handleInputValueChange = (nextValue: string) => {
		onChange(nextValue, []);
		setHasNavigated(false);
		setSuppressAutoOpen(false);
		setHasInteracted(false);
	};
	const handleHistoryClear = () => {
		SearchHistory.clear(historyKey);
		setAutocompleteType('history');
		setSelectedIndex(-1);
	};
	const handleFilterSelect = (row: HistoryFilterRow, index: number) => {
		setSelectedIndex(index);
		applyHistoryFilterRow(row);
	};
	const setInputRefs = useCallback(
		(node: LexicalSearchInputHandle | null) => {
			inputRef.current = node;
			assignRef(inputRefExternal, node);
		},
		[inputRefExternal],
	);
	return {
		i18n,
		isFocused,
		setIsFocused,
		autocompleteType,
		inputRef,
		setInputRefs,
		suppressAutoOpen,
		setSuppressAutoOpen,
		filterOptions,
		historyFilterRows,
		listboxId,
		keyboardFocusIndex,
		hoverIndexForRender,
		isInGuildChannel,
		currentGuildIdForScope,
		getAutocompleteOptions,
		hasAnyOptions,
		getTotalOptions,
		getAriaActiveDescendant,
		handleAutocompleteSelect,
		handleKeyDown,
		handleInputValueChange,
		handleHistoryClear,
		handleFilterSelect,
		handleOptionMouseEnter,
		handleOptionMouseLeave,
		resetSelectedIndex: () => setSelectedIndex(-1),
		setHoverIndex,
		setHasInteracted,
	};
}
