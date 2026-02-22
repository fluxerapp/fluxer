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

import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import {ChannelsSection} from '@app/components/channel/message_search_bar/ChannelsSection';
import {DateSection} from '@app/components/channel/message_search_bar/DateSection';
import {FiltersSection} from '@app/components/channel/message_search_bar/FilterOption';
import {HistorySection} from '@app/components/channel/message_search_bar/HistorySection';
import styles from '@app/components/channel/message_search_bar/MessageSearchBar.module.css';
import {UsersSection} from '@app/components/channel/message_search_bar/UsersSection';
import {ValuesSection} from '@app/components/channel/message_search_bar/ValuesSection';
import {DEFAULT_SCOPE_VALUE, getScopeOptionsForChannel} from '@app/components/channel/SearchScopeOptions';
import {ContextMenuCloseProvider} from '@app/components/uikit/context_menu/ContextMenu';
import {MenuGroup} from '@app/components/uikit/context_menu/MenuGroup';
import {MenuItemRadio} from '@app/components/uikit/context_menu/MenuItemRadio';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/lib/PasswordManagerAutocomplete';
import {useParams} from '@app/lib/router/React';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {GuildMemberRecord} from '@app/records/GuildMemberRecord';
import type {GuildRecord} from '@app/records/GuildRecord';
import type {UserRecord} from '@app/records/UserRecord';
import ChannelSearchStore, {getChannelSearchContextId} from '@app/stores/ChannelSearchStore';
import ChannelStore from '@app/stores/ChannelStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import KeyboardModeStore from '@app/stores/KeyboardModeStore';
import MemberSearchStore, {type SearchContext} from '@app/stores/MemberSearchStore';
import type {SearchHistoryEntry} from '@app/stores/SearchHistoryStore';
import SearchHistoryStore from '@app/stores/SearchHistoryStore';
import SelectedChannelStore from '@app/stores/SelectedChannelStore';
import SelectedGuildStore from '@app/stores/SelectedGuildStore';
import UserStore from '@app/stores/UserStore';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import type {SearchSegment} from '@app/utils/SearchSegmentManager';
import type {MessageSearchScope, SearchFilterOption} from '@app/utils/SearchUtils';
import {getSearchFilterOptions} from '@app/utils/SearchUtils';
import {autoUpdate, FloatingPortal, flip, offset, size, useFloating} from '@floating-ui/react';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {
	ChatCenteredDotsIcon,
	EnvelopeSimpleIcon,
	GlobeIcon,
	HashIcon,
	MagnifyingGlassIcon,
	UsersIcon,
	XIcon,
} from '@phosphor-icons/react';
import {DateTime} from 'luxon';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const SCOPE_ICON_COMPONENTS: Record<MessageSearchScope, React.ComponentType<IconProps>> = {
	current: HashIcon,
	all_dms: EnvelopeSimpleIcon,
	open_dms: ChatCenteredDotsIcon,
	all_guilds: GlobeIcon,
	all: UsersIcon,
	open_dms_and_all_guilds: UsersIcon,
};

export interface SearchBarProps {
	channel?: ChannelRecord;
	value: string;
	onChange: (value: string, segments: Array<SearchSegment>) => void;
	onSearch: () => void;
	onClear: () => void;
	isResultsOpen?: boolean;
	onCloseResults?: () => void;
	inputRefExternal?: React.Ref<HTMLInputElement>;
	highContrast?: boolean;
}

type AutocompleteType = 'filters' | 'users' | 'channels' | 'values' | 'date' | 'history' | null;

interface SearchHints {
	usersByTag: Record<string, string>;
	channelsByName: Record<string, string>;
}

type AutocompleteOption =
	| SearchFilterOption
	| UserRecord
	| ChannelRecord
	| {value: string; label: string}
	| string
	| SearchHistoryEntry;

const filterRequiresValue = (filter: SearchFilterOption): boolean => {
	return Boolean(filter.requiresValue) || (filter.values?.length ?? 0) > 0;
};

function deduplicateMembers(members: Array<GuildMemberRecord>): Array<GuildMemberRecord> {
	const seen = new Set<string>();
	const result: Array<GuildMemberRecord> = [];
	for (const member of members) {
		if (!seen.has(member.user.id)) {
			seen.add(member.user.id);
			result.push(member);
		}
	}
	return result;
}

function assignRef<T>(ref: React.Ref<T> | undefined, value: T | null): void {
	if (!ref) {
		return;
	}

	if (typeof ref === 'function') {
		ref(value);
		return;
	}

	(ref as React.MutableRefObject<T | null>).current = value;
}

function normalizeFilterKey(filterKey: string): string {
	return filterKey.replace(/^-/, '');
}

function isDateFilterKey(filterKey: string): boolean {
	switch (normalizeFilterKey(filterKey)) {
		case 'before':
		case 'after':
		case 'during':
		case 'on':
			return true;
		default:
			return false;
	}
}

function isUserFilterKey(filterKey: string): boolean {
	switch (normalizeFilterKey(filterKey)) {
		case 'from':
		case 'mentions':
			return true;
		default:
			return false;
	}
}

type GuildSearchMode = 'none' | 'current_guild' | 'all_guilds';

interface UserGuildSearchPlan {
	mode: GuildSearchMode;
	guildsToSearch: Array<GuildRecord> | null;
	priorityGuildId?: string;
	workerFilters: {friends?: boolean; guild?: string};
}

function getUserGuildSearchPlan(scope: MessageSearchScope, currentGuildId: string | undefined): UserGuildSearchPlan {
	const SCOPES_WITH_GUILDS = new Set<MessageSearchScope>(['current', 'all_guilds', 'all', 'open_dms_and_all_guilds']);
	const ALL_GUILDS_SCOPES = new Set<MessageSearchScope>(['all_guilds', 'all', 'open_dms_and_all_guilds']);

	if (!SCOPES_WITH_GUILDS.has(scope)) {
		return {
			mode: 'none',
			guildsToSearch: null,
			priorityGuildId: undefined,
			workerFilters: {},
		};
	}

	if (scope === 'current') {
		if (!currentGuildId) {
			return {
				mode: 'none',
				guildsToSearch: null,
				priorityGuildId: undefined,
				workerFilters: {},
			};
		}

		const guild = GuildStore.getGuild(currentGuildId);
		return {
			mode: 'current_guild',
			guildsToSearch: guild ? [guild] : [],
			priorityGuildId: currentGuildId,
			workerFilters: {guild: currentGuildId},
		};
	}

	if (ALL_GUILDS_SCOPES.has(scope)) {
		return {
			mode: 'all_guilds',
			guildsToSearch: GuildStore.getGuilds(),
			priorityGuildId: currentGuildId,
			workerFilters: {},
		};
	}

	return {
		mode: 'none',
		guildsToSearch: null,
		priorityGuildId: undefined,
		workerFilters: {},
	};
}

type MemberSearchBoosters = Record<string, number>;

function buildUserSearchBoosters(
	channel: ChannelRecord | undefined,
	currentGuildId: string | undefined,
	mode: GuildSearchMode,
) {
	const boosters: MemberSearchBoosters = {};

	if (
		channel &&
		(channel.type === ChannelTypes.DM ||
			channel.type === ChannelTypes.GROUP_DM ||
			channel.type === ChannelTypes.DM_PERSONAL_NOTES)
	) {
		for (const id of channel.recipientIds) {
			boosters[id] = Math.max(boosters[id] ?? 1, 3);
		}
	}

	if (mode === 'all_guilds' && currentGuildId) {
		const members = GuildMemberStore.getMembers(currentGuildId);
		const MAX_BOOSTED_MEMBERS = 300;
		for (let i = 0; i < members.length && i < MAX_BOOSTED_MEMBERS; i += 1) {
			const id = members[i]!.user.id;
			boosters[id] = Math.max(boosters[id] ?? 1, 2);
		}
	}

	return boosters;
}

export const MessageSearchBar = observer(
	({
		channel,
		value,
		onChange,
		onSearch,
		onClear,
		isResultsOpen = false,
		onCloseResults,
		inputRefExternal,
		highContrast = false,
	}: SearchBarProps) => {
		const {i18n} = useLingui();
		const {guildId: routeGuildId} = useParams() as {guildId?: string};

		const [isFocused, setIsFocused] = useState(false);
		const [autocompleteType, setAutocompleteType] = useState<AutocompleteType>(null);
		const [selectedIndex, setSelectedIndex] = useState(-1);
		const [hoverIndex, setHoverIndex] = useState(-1);
		const [hasNavigated, setHasNavigated] = useState(false);
		const [hasInteracted, setHasInteracted] = useState(false);
		const [currentFilter, setCurrentFilter] = useState<SearchFilterOption | null>(null);

		const inputRef = useRef<HTMLInputElement | null>(null);
		const [suppressAutoOpen, setSuppressAutoOpen] = useState(false);
		const suppressAutoOpenRef = useRef(false);
		const hintsRef = useRef<SearchHints>({usersByTag: {}, channelsByName: {}});

		const searchContextRef = useRef<SearchContext | null>(null);
		const [memberSearchResults, setMemberSearchResults] = useState<Array<UserRecord>>([]);

		const memberFetchDebounceTimerRef = useRef<NodeJS.Timeout | null>(null);
		const memberFetchQueryRef = useRef<string>('');

		const selectedGuildId = SelectedGuildStore.selectedGuildId;

		const channelGuildId = channel?.guildId ?? undefined;
		const isInGuildChannel = Boolean(channelGuildId);

		const currentGuildIdForScope = isInGuildChannel
			? (channelGuildId ?? routeGuildId ?? selectedGuildId ?? undefined)
			: undefined;

		const filterOptions = useMemo(() => [...getSearchFilterOptions(i18n)], [i18n]);

		const contextId = useMemo(
			() => getChannelSearchContextId(channel ?? null, selectedGuildId),
			[channel?.guildId, channel?.id, selectedGuildId],
		);

		const searchContext = contextId ? ChannelSearchStore.getContext(contextId) : null;
		const activeScope = searchContext?.scope ?? DEFAULT_SCOPE_VALUE;

		const scopeOptions = useMemo(
			() => getScopeOptionsForChannel(i18n, channel),
			[i18n, channel?.id, channel?.type, channel?.guildId],
		);
		const scopeOptionValues = useMemo(() => new Set(scopeOptions.map((option) => option.value)), [scopeOptions]);

		useEffect(() => {
			if (!scopeOptions.length || !contextId) {
				return;
			}

			const fallbackScope = scopeOptions[0].value;
			const currentScope: MessageSearchScope = activeScope ?? fallbackScope;

			if (!scopeOptionValues.has(currentScope)) {
				ChannelSearchStore.setScope(contextId, fallbackScope);
			}
		}, [scopeOptions, scopeOptionValues, activeScope, contextId]);

		const handleScopeSelect = useCallback(
			(scope: MessageSearchScope) => {
				if (!contextId) {
					return;
				}
				ChannelSearchStore.setScope(contextId, scope);
			},
			[contextId],
		);

		const handleScopeMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuActionCreators.openFromElementBottomRight(event, ({onClose}) => (
					<ContextMenuCloseProvider value={onClose}>
						<MenuGroup>
							{scopeOptions.map((option) => (
								<MenuItemRadio
									key={option.value}
									selected={activeScope === option.value}
									closeOnSelect
									onSelect={() => handleScopeSelect(option.value)}
									icon={React.createElement(SCOPE_ICON_COMPONENTS[option.value] ?? HashIcon, {
										size: 16,
										weight: 'bold',
									})}
								>
									{option.label}
								</MenuItemRadio>
							))}
						</MenuGroup>
					</ContextMenuCloseProvider>
				));
			},
			[handleScopeSelect, scopeOptions, activeScope],
		);

		const activeScopeOption = useMemo(() => {
			if (!scopeOptions.length) {
				return null;
			}
			return scopeOptions.find((opt) => opt.value === activeScope) ?? scopeOptions[0];
		}, [scopeOptions, activeScope]);

		const ScopeIconComponent = useMemo(() => {
			if (!activeScope) {
				return HashIcon;
			}
			return SCOPE_ICON_COMPONENTS[activeScope] ?? HashIcon;
		}, [activeScope]);

		const scopeTooltipText = useMemo(() => {
			if (activeScopeOption?.label) {
				return i18n._(msg`Search scope: ${activeScopeOption.label}`);
			}
			return i18n._(msg`Search scope`);
		}, [i18n, activeScopeOption?.label]);

		useEffect(() => {
			const handleGlobalKeydown = (event: KeyboardEvent) => {
				const isFind = (event.key === 'f' || event.key === 'F') && (event.metaKey || event.ctrlKey);
				if (!isFind) return;

				event.preventDefault();
				event.stopPropagation();

				const el = inputRef.current;
				if (!el) return;

				el.focus();
				const pos = el.value.length;
				try {
					el.setSelectionRange(pos, pos);
				} catch {}
			};

			document.addEventListener('keydown', handleGlobalKeydown, true);
			return () => document.removeEventListener('keydown', handleGlobalKeydown, true);
		}, []);

		useEffect(() => {
			suppressAutoOpenRef.current = suppressAutoOpen;
		}, [suppressAutoOpen]);

		useEffect(() => {
			const context = MemberSearchStore.getSearchContext((results) => {
				const users = results.map((result) => UserStore.getUser(result.id)).filter((u): u is UserRecord => u != null);
				setMemberSearchResults(users);
			}, 25);

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

			if (autocompleteType !== 'users' || !currentFilter || !isUserFilterKey(currentFilter.key)) {
				memberFetchQueryRef.current = '';
				const context = searchContextRef.current;
				if (context) {
					context.clearQuery();
				}
				setMemberSearchResults([]);
				return;
			}

			const plan = getUserGuildSearchPlan(activeScope, currentGuildIdForScope);

			if (plan.mode === 'none') {
				memberFetchQueryRef.current = '';
				const context = searchContextRef.current;
				if (context) {
					context.clearQuery();
				}
				setMemberSearchResults([]);
				return;
			}

			const cursorPos = inputRef.current?.selectionStart ?? value.length;
			const textBeforeCursor = value.slice(0, cursorPos);
			const words = textBeforeCursor.split(/\s+/);
			const currentWord = words[words.length - 1] || '';
			const searchQuery = currentWord.slice(currentFilter.syntax.length).trim();

			const context = searchContextRef.current;

			if (searchQuery.length === 0) {
				memberFetchQueryRef.current = '';
				if (context) {
					context.clearQuery();
				}
				setMemberSearchResults([]);
				return;
			}

			const fallbackGuildId = currentGuildIdForScope;
			if (fallbackGuildId) {
				const cachedMembers = deduplicateMembers(GuildMemberStore.getMembers(fallbackGuildId));
				if (cachedMembers.length > 0) {
					const localResults = matchSorter(cachedMembers, searchQuery, {
						keys: [
							(member) => NicknameUtils.getNickname(member.user, fallbackGuildId),
							(member) => member.user.username,
							(member) => member.user.tag,
						],
					})
						.slice(0, 12)
						.map((m) => m.user);

					setMemberSearchResults(localResults);
				} else {
					setMemberSearchResults([]);
				}
			}

			const boosters = buildUserSearchBoosters(channel, currentGuildIdForScope, plan.mode);
			if (context) {
				context.setQuery(searchQuery, plan.workerFilters, new Set(), new Set(), boosters);
			}

			if (!plan.guildsToSearch || plan.guildsToSearch.length === 0) {
				memberFetchQueryRef.current = searchQuery;
				return;
			}

			memberFetchQueryRef.current = searchQuery;
			const scheduledQuery = searchQuery;

			memberFetchDebounceTimerRef.current = setTimeout(() => {
				memberFetchDebounceTimerRef.current = null;

				if (autocompleteType !== 'users' || !currentFilter || !isUserFilterKey(currentFilter.key)) {
					return;
				}

				if (memberFetchQueryRef.current !== scheduledQuery) {
					return;
				}

				const guildIds = plan.guildsToSearch?.map((g) => g.id) ?? [];

				const priorityGuildId = plan.priorityGuildId;

				void MemberSearchStore.fetchMembersInBackground(scheduledQuery, guildIds, priorityGuildId);
			}, 300);

			return () => {
				if (memberFetchDebounceTimerRef.current) {
					clearTimeout(memberFetchDebounceTimerRef.current);
					memberFetchDebounceTimerRef.current = null;
				}
			};
		}, [autocompleteType, currentFilter, value, activeScope, channel, currentGuildIdForScope]);

		useEffect(() => {
			if (autocompleteType !== 'users' || !currentFilter) {
				const context = searchContextRef.current;
				if (context) {
					context.clearQuery();
				}
				setMemberSearchResults([]);
				memberFetchQueryRef.current = '';
				if (memberFetchDebounceTimerRef.current) {
					clearTimeout(memberFetchDebounceTimerRef.current);
					memberFetchDebounceTimerRef.current = null;
				}
			}
		}, [autocompleteType, currentFilter]);

		const listboxId = useMemo(() => `message-search-listbox-${channel?.id ?? 'global'}`, [channel?.id]);

		const {refs, floatingStyles, isPositioned} = useFloating({
			placement: 'bottom-start',
			open: isFocused && autocompleteType !== null,
			whileElementsMounted: autoUpdate,
			middleware: [
				offset(8),
				flip({padding: 16}),
				size({
					apply({rects, elements}) {
						const minWidth = 380;
						const maxWidth = Math.min(window.innerWidth - 32, 480);
						const width = Math.min(maxWidth, Math.max(rects.reference.width, minWidth));
						Object.assign(elements.floating.style, {
							width: `${width}px`,
						});
					},
					padding: 16,
				}),
			],
		});

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
						return isInGuildChannel ? 'channels' : 'values';

					default:
						return 'values';
				}
			},
			[isInGuildChannel],
		);

		const getAutocompleteOptions = useCallback((): Array<AutocompleteOption> => {
			const cursorPos = inputRef.current?.selectionStart ?? value.length;
			const textBeforeCursor = value.slice(0, cursorPos);
			const words = textBeforeCursor.split(/\s+/);
			const currentWord = words[words.length - 1] || '';

			switch (autocompleteType) {
				case 'filters': {
					const filtered = filterOptions.filter((opt) => {
						if (opt.requiresGuild && !isInGuildChannel) return false;

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
					return SearchHistoryStore.search(currentWord, channel?.id).slice(0, 5);
				}

				case 'users': {
					if (!currentFilter) return [];
					const searchTerm = currentWord.slice(currentFilter.syntax.length);

					const plan = getUserGuildSearchPlan(activeScope, currentGuildIdForScope);

					if (plan.mode !== 'none') {
						if (memberSearchResults.length > 0) {
							return memberSearchResults.slice(0, 12);
						}

						const fallbackGuildId = currentGuildIdForScope;

						if (fallbackGuildId) {
							const isGuildFullyLoaded = GuildMemberStore.isGuildFullyLoaded(fallbackGuildId);
							if (isGuildFullyLoaded) {
								const cachedMembers = GuildMemberStore.getMembers(fallbackGuildId);
								return matchSorter(cachedMembers, searchTerm, {
									keys: [
										(member) => NicknameUtils.getNickname(member.user, fallbackGuildId),
										(member) => member.user.username,
										(member) => member.user.tag,
									],
								})
									.slice(0, 12)
									.map((m) => m.user);
							}
						}

						return [];
					}

					if (channel) {
						const users = channel.recipientIds
							.map((id) => UserStore.getUser(id))
							.filter((u): u is UserRecord => u != null);

						return matchSorter(users, searchTerm, {keys: ['username', 'tag']}).slice(0, 12);
					}

					return [];
				}

				case 'channels': {
					if (!currentFilter) return [];
					const guildIdForChannels = channelGuildId ?? routeGuildId;
					if (!guildIdForChannels) return [];

					const searchTerm = currentWord.slice(currentFilter.syntax.length);
					const channels = ChannelStore.getGuildChannels(guildIdForChannels).filter(
						(c) => c.type === ChannelTypes.GUILD_TEXT,
					);

					const recentVisitsForGuild = SelectedChannelStore.recentlyVisitedChannels
						.filter((visit) => visit.guildId === guildIdForChannels)
						.sort((a, b) => b.timestamp - a.timestamp);

					const recencyRank = new Map<string, number>();
					recentVisitsForGuild.forEach((visit, index) => {
						if (!recencyRank.has(visit.channelId)) {
							recencyRank.set(visit.channelId, index);
						}
					});

					const currentChannelId = channel?.id;
					const matches = matchSorter(channels, searchTerm, {keys: ['name']});
					const orderedMatches = [...matches].sort((a, b) => {
						const resolveRank = (ch: ChannelRecord) => {
							if (ch.id === currentChannelId) return -1;
							return recencyRank.get(ch.id) ?? Number.MAX_SAFE_INTEGER;
						};

						const rankDifference = resolveRank(a) - resolveRank(b);
						if (rankDifference !== 0) {
							return rankDifference;
						}

						return (a.name ?? '').localeCompare(b.name ?? '');
					});

					return orderedMatches.slice(0, 12);
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
					const now = DateTime.local();
					const fmtDate = (dt: DateTime) => dt.toFormat('yyyy-MM-dd');
					const fmtDateTime = (dt: DateTime) => dt.toFormat("yyyy-MM-dd'T'HH:mm");

					return [
						{label: i18n._(msg`Today`), value: fmtDate(now)},
						{label: i18n._(msg`Yesterday`), value: fmtDate(now.minus({days: 1}))},
						{label: i18n._(msg`Now`), value: fmtDateTime(now)},
					];
				}

				default:
					return [];
			}
		}, [
			autocompleteType,
			value,
			filterOptions,
			isInGuildChannel,
			currentFilter,
			channelGuildId,
			routeGuildId,
			channel,
			memberSearchResults,
			i18n,
			activeScope,
			currentGuildIdForScope,
		]);

		const getHistoryCommonFilters = useCallback(() => {
			return filterOptions
				.filter((opt) => !opt.requiresGuild || isInGuildChannel)
				.filter((opt) => !opt.key.startsWith('-'));
		}, [filterOptions, isInGuildChannel]);

		const getTotalOptions = useCallback((): number => {
			if (!autocompleteType) return 0;

			if (autocompleteType === 'history') {
				return getHistoryCommonFilters().length + getAutocompleteOptions().length;
			}

			return getAutocompleteOptions().length;
		}, [autocompleteType, getAutocompleteOptions, getHistoryCommonFilters]);

		const hasAnyOptions = useCallback((): boolean => {
			return getTotalOptions() > 0;
		}, [getTotalOptions]);

		const getSelectedOption = useCallback((): AutocompleteOption | null => {
			if (selectedIndex < 0) return null;

			if (autocompleteType === 'history') {
				const commonFilters = getHistoryCommonFilters();
				if (selectedIndex < commonFilters.length) {
					return commonFilters[selectedIndex] ?? null;
				}

				const historyOptions = getAutocompleteOptions();
				const historyIndex = selectedIndex - commonFilters.length;
				return historyOptions[historyIndex] ?? null;
			}

			const options = getAutocompleteOptions();
			return options[selectedIndex] ?? null;
		}, [selectedIndex, autocompleteType, getAutocompleteOptions, getHistoryCommonFilters]);

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

			const cursorPos = inputRef.current?.selectionStart ?? value.length;
			const textBeforeCursor = value.slice(0, cursorPos);
			const words = textBeforeCursor.split(/\s+/);
			const currentWord = words[words.length - 1] || '';

			const matchingFilter = filterOptions.find((opt) => currentWord.startsWith(opt.syntax));

			if (matchingFilter) {
				const afterColon = currentWord.slice(matchingFilter.syntax.length);
				const filterKeyBase = normalizeFilterKey(matchingFilter.key);

				if (matchingFilter.requiresGuild && !isInGuildChannel) {
					setAutocompleteType(null);
					setCurrentFilter(null);
					return;
				}

				if (isDateFilterKey(filterKeyBase)) {
					setAutocompleteType('date');
					setCurrentFilter(matchingFilter);
					return;
				}

				if (matchingFilter.values && afterColon.length === 0) {
					setAutocompleteType('values');
					setCurrentFilter(matchingFilter);
					return;
				}

				if (filterKeyBase === 'from' || filterKeyBase === 'mentions') {
					setAutocompleteType('users');
					setCurrentFilter(matchingFilter);
					setSelectedIndex(0);
					setHasNavigated(false);
					return;
				}

				if (filterKeyBase === 'in' && isInGuildChannel) {
					setAutocompleteType('channels');
					setCurrentFilter(matchingFilter);
					setSelectedIndex(0);
					setHasNavigated(false);
					return;
				}

				if (matchingFilter.values) {
					setAutocompleteType('values');
					setCurrentFilter(matchingFilter);
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
				return opt.syntax.includes(currentWord) || currentWord.includes(opt.key) || opt.key.includes(currentWord);
			});

			setAutocompleteType(partialMatch ? 'filters' : null);
			setCurrentFilter(null);
		}, [value, isFocused, isInGuildChannel, suppressAutoOpen, filterOptions]);

		useEffect(() => {
			const totalOptions = getTotalOptions();
			if (totalOptions > 0 && (selectedIndex >= totalOptions || selectedIndex < -1)) {
				setSelectedIndex(-1);
			}
		}, [autocompleteType, selectedIndex, getTotalOptions]);

		const setInputRefs = (node: HTMLInputElement | null) => {
			inputRef.current = node;
			assignRef(inputRefExternal, node);
		};

		const handleOptionMouseEnter = (index: number) => {
			setHoverIndex(index);
			setHasInteracted(true);
		};

		const handleOptionMouseLeave = () => {
			setHoverIndex(-1);
		};

		const shouldShowKeyboardFocus = hasNavigated || autocompleteType === 'users' || autocompleteType === 'channels';
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

			return `${listboxId}-opt-${selectedIndex}`;
		}, [
			isFocused,
			autocompleteType,
			getTotalOptions,
			shouldShowKeyboardFocus,
			shouldShowHover,
			selectedIndex,
			listboxId,
		]);

		const handleAutocompleteSelect = (option: AutocompleteOption) => {
			const cursorPos = inputRef.current?.selectionStart ?? value.length;
			const textBeforeCursor = value.slice(0, cursorPos);
			const textAfterCursor = value.slice(cursorPos);
			const words = textBeforeCursor.split(/\s+/);
			const currentWord = words[words.length - 1] || '';
			const lastWordStart = textBeforeCursor.length - currentWord.length;

			let newText = '';
			let newCursorPos = 0;
			let shouldSubmit = false;

			const insertToken = (syntax: string, tokenValue: string, addSpaceAfter = true) => {
				const needsQuotes = /\s/.test(tokenValue);
				const display = needsQuotes ? `${syntax}"${tokenValue}"` : `${syntax}${tokenValue}`;
				const before = textBeforeCursor.slice(0, lastWordStart);
				const space = addSpaceAfter ? ' ' : '';
				newText = `${before}${display}${space}${textAfterCursor}`;
				newCursorPos = (before + display).length + space.length;
			};

			switch (autocompleteType) {
				case 'filters': {
					const filter = option as SearchFilterOption;
					const requiresValue = filterRequiresValue(filter);
					insertToken(filter.syntax, '', !requiresValue);
					shouldSubmit = !requiresValue;
					break;
				}

				case 'users': {
					const user = option as UserRecord;
					const tag = `${user.username}#${user.discriminator}`;
					insertToken(currentFilter!.syntax, tag);
					hintsRef.current.usersByTag[tag] = user.id;
					shouldSubmit = true;
					break;
				}

				case 'channels': {
					const ch = option as ChannelRecord;
					const name = ch.name || i18n._(msg`Unnamed`);
					insertToken(currentFilter!.syntax, name);
					hintsRef.current.channelsByName[name] = ch.id;
					shouldSubmit = true;
					break;
				}

				case 'values': {
					const valueOption = option as {value: string; label: string};
					const before = textBeforeCursor.slice(0, lastWordStart);
					const display = `${currentFilter!.syntax}${valueOption.value}`;
					newText = `${before}${display} ${textAfterCursor}`;
					newCursorPos = (before + display).length + 1;
					shouldSubmit = true;
					break;
				}

				case 'date': {
					const dateOption = option as {value: string; label: string};
					const before = textBeforeCursor.slice(0, lastWordStart);
					const display = `${currentFilter!.syntax}${dateOption.value}`;
					newText = `${before}${display} ${textAfterCursor}`;
					newCursorPos = (before + display).length + 1;
					shouldSubmit = true;
					break;
				}

				case 'history': {
					const entry = option as SearchHistoryEntry;
					newText = entry.query;
					newCursorPos = newText.length;

					const segments: Array<SearchSegment> = [];
					if (entry.hints?.usersByTag) {
						for (const [tag, userId] of Object.entries(entry.hints.usersByTag) as Array<[string, string]>) {
							segments.push({
								type: 'user',
								filterKey: 'from',
								id: userId,
								displayText: `from:${tag}`,
								start: 0,
								end: 0,
							});
							segments.push({
								type: 'user',
								filterKey: 'mentions',
								id: userId,
								displayText: `mentions:${tag}`,
								start: 0,
								end: 0,
							});
						}
					}

					if (entry.hints?.channelsByName) {
						for (const [name, channelId] of Object.entries(entry.hints.channelsByName) as Array<[string, string]>) {
							const disp = /\s/.test(name) ? `in:"${name}"` : `in:${name}`;
							segments.push({type: 'channel', filterKey: 'in', id: channelId, displayText: disp, start: 0, end: 0});
						}
					}

					onChange(newText, segments);
					SearchHistoryStore.add(newText, channel?.id, entry.hints);

					setTimeout(() => {
						inputRef.current?.focus();
						inputRef.current?.setSelectionRange(newCursorPos, newCursorPos);
					}, 0);

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

			onChange(newText, []);

			setTimeout(() => {
				inputRef.current?.focus();
				inputRef.current?.setSelectionRange(newCursorPos, newCursorPos);
			}, 0);

			setSelectedIndex(-1);
			setAutocompleteType(null);
			setCurrentFilter(null);

			if (shouldSubmit && newText.trim().length > 0) {
				SearchHistoryStore.add(newText, channel?.id, hintsRef.current);
				setSuppressAutoOpen(true);
				setTimeout(() => onSearch(), 0);
			}
		};

		const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
			if (e.key === '?' && autocompleteType === null) {
				e.preventDefault();
				return;
			}

			if (e.key === 'Enter' && autocompleteType === null) {
				e.preventDefault();
				SearchHistoryStore.add(value, channel?.id, hintsRef.current);
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

				if (value.trim().length > 0) {
					onChange('', []);
					setSelectedIndex(-1);
					setHasNavigated(false);
					setSuppressAutoOpen(false);
					return;
				}

				if (isResultsOpen) {
					setAutocompleteType(null);
					setCurrentFilter(null);
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
					SearchHistoryStore.add(value, channel?.id, hintsRef.current);
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

				case 'Home': {
					e.preventDefault();
					setSelectedIndex(0);
					setHasNavigated(true);
					return;
				}

				case 'End': {
					e.preventDefault();
					setSelectedIndex(totalOptions - 1);
					setHasNavigated(true);
					return;
				}

				case 'Enter':
				case 'Tab': {
					e.preventDefault();

					let shouldAutoSelect = hasNavigated;
					if (autocompleteType === 'users' || autocompleteType === 'channels') {
						shouldAutoSelect = true;
					}

					const cursorPos = inputRef.current?.selectionStart ?? value.length;
					const textBeforeCursor = value.slice(0, cursorPos);
					const words = textBeforeCursor.split(/\s+/);
					const currentWord = words[words.length - 1] || '';
					const matchingFilter = filterOptions.find((opt) => currentWord.startsWith(opt.syntax));
					const afterColon = matchingFilter ? currentWord.slice(matchingFilter.syntax.length) : '';

					if (matchingFilter) {
						const requiresValue = filterRequiresValue(matchingFilter);
						if (!shouldAutoSelect && requiresValue && afterColon.length === 0) {
							return;
						}
					}

					if (!shouldAutoSelect) {
						SearchHistoryStore.add(value, channel?.id, hintsRef.current);
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
						const isFilterOptionInHistory =
							autocompleteType === 'history' && typeof selected === 'object' && selected !== null && 'key' in selected;

						if (isFilterOptionInHistory) {
							const filter = selected as SearchFilterOption;

							const cursorPosInner = inputRef.current?.selectionStart ?? value.length;
							const textBeforeCursorInner = value.slice(0, cursorPosInner);
							const textAfterCursorInner = value.slice(cursorPosInner);

							const wordsInner = textBeforeCursorInner.split(/\s+/);
							const currentWordInner = wordsInner[wordsInner.length - 1] || '';
							const lastWordStartInner = textBeforeCursorInner.length - currentWordInner.length;

							const display = filter.syntax;
							const before = textBeforeCursorInner.slice(0, lastWordStartInner);
							const requiresValue = filterRequiresValue(filter);
							const space = requiresValue ? '' : ' ';
							const newText = `${before}${display}${space}${textAfterCursorInner}`;
							const newCursorPos = (before + display).length + space.length;

							onChange(newText, []);

							setTimeout(() => {
								inputRef.current?.setSelectionRange(newCursorPos, newCursorPos);
							}, 0);

							if (!requiresValue) {
								setTimeout(() => {
									SearchHistoryStore.add(newText, channel?.id, hintsRef.current);
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

		const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
			onChange(e.target.value, []);
			setHasNavigated(false);
			setSuppressAutoOpen(false);
			setHasInteracted(false);
		};

		const handleHistoryClear = () => {
			SearchHistoryStore.clear(channel?.id);
			setAutocompleteType('filters');
			setSelectedIndex(-1);
		};

		const handleFilterSelect = (filter: SearchFilterOption, index: number) => {
			setSelectedIndex(index);

			const cursorPos = inputRef.current?.selectionStart ?? value.length;
			const textBeforeCursor = value.slice(0, cursorPos);
			const textAfterCursor = value.slice(cursorPos);

			const words = textBeforeCursor.split(/\s+/);
			const currentWord = words[words.length - 1] || '';
			const lastWordStart = textBeforeCursor.length - currentWord.length;

			const display = filter.syntax;
			const before = textBeforeCursor.slice(0, lastWordStart);
			const requiresValue = filterRequiresValue(filter);
			const space = requiresValue ? '' : ' ';
			const newText = `${before}${display}${space}${textAfterCursor}`;
			const newCursorPos = (before + display).length + space.length;

			onChange(newText, []);

			setTimeout(() => {
				inputRef.current?.setSelectionRange(newCursorPos, newCursorPos);
			}, 0);

			if (!requiresValue) {
				setTimeout(() => {
					SearchHistoryStore.add(newText, channel?.id, hintsRef.current);
					setSuppressAutoOpen(true);
					setTimeout(() => onSearch(), 0);
					setAutocompleteType(null);
					setCurrentFilter(null);
				}, 10);
				return;
			}

			setCurrentFilter(filter);
			setAutocompleteType(getAutocompleteTypeForFilter(filter));
		};

		const renderAutocompleteContent = () => {
			switch (autocompleteType) {
				case 'filters':
					return (
						<FiltersSection
							options={getAutocompleteOptions() as Array<SearchFilterOption>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
						/>
					);

				case 'history':
					return (
						<HistorySection
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							isInGuild={isInGuildChannel}
							channelId={channel?.id}
							onHistoryClear={handleHistoryClear}
							onFilterSelect={handleFilterSelect}
							onFilterMouseEnter={(index) => {
								setHoverIndex(index);
								setHasInteracted(true);
							}}
							onFilterMouseLeave={handleOptionMouseLeave}
							filterOptions={filterOptions}
						/>
					);

				case 'users':
					return (
						<UsersSection
							options={getAutocompleteOptions() as Array<UserRecord>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							guildId={currentGuildIdForScope}
							isInGuild={isInGuildChannel}
						/>
					);

				case 'channels':
					return (
						<ChannelsSection
							options={getAutocompleteOptions() as Array<ChannelRecord>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
						/>
					);

				case 'values':
					return (
						<ValuesSection
							options={getAutocompleteOptions() as Array<{value: string; label: string}>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
						/>
					);

				case 'date':
					return (
						<DateSection
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
						/>
					);

				default:
					return null;
			}
		};

		const hasValue = value.length > 0;
		const ariaActiveDescendant = getAriaActiveDescendant();

		return (
			<>
				<div ref={refs.setReference} className={styles.anchor}>
					<div
						className={highContrast ? `${styles.inputContainer} ${styles.inputContainerOnCall}` : styles.inputContainer}
					>
						<Tooltip text={scopeTooltipText} position="bottom">
							<button
								type="button"
								className={styles.scopeButton}
								onClick={handleScopeMenuOpen}
								aria-label={scopeTooltipText}
							>
								<MagnifyingGlassIcon className={styles.searchIcon} weight="bold" />
								<span className={styles.scopeBadge}>
									<ScopeIconComponent size={8} weight="bold" />
								</span>
							</button>
						</Tooltip>

						<input
							ref={setInputRefs}
							type="text"
							{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
							value={value}
							onChange={handleInputChange}
							onMouseDown={() => setSuppressAutoOpen(false)}
							onKeyDown={handleKeyDown}
							onFocus={() => {
								setIsFocused(true);
								if (KeyboardModeStore.keyboardModeEnabled) {
									setSuppressAutoOpen(false);
								}
							}}
							onBlur={() => {
								setIsFocused(false);
								if (isResultsOpen && value.trim().length === 0) {
									onCloseResults?.();
								}
							}}
							role="combobox"
							aria-autocomplete="list"
							aria-expanded={isFocused && autocompleteType !== null}
							aria-controls={isFocused && autocompleteType !== null ? listboxId : undefined}
							aria-activedescendant={ariaActiveDescendant}
							placeholder={i18n._(msg`Search messages`)}
							className={styles.input}
						/>

						{hasValue && (
							<button
								type="button"
								onMouseDown={(ev) => ev.preventDefault()}
								onClick={() => {
									onClear();
									setSelectedIndex(-1);
									inputRef.current?.focus();
								}}
								className={styles.clearButton}
								aria-label={i18n._(msg`Clear search`)}
							>
								<XIcon weight="bold" className={styles.optionMetaIcon} />
							</button>
						)}
					</div>
				</div>

				{isFocused && autocompleteType && hasAnyOptions() && (
					<FloatingPortal>
						{/* biome-ignore lint/a11y/noStaticElementInteractions: This wrapper intercepts mousedown to preserve input focus for the listbox. */}
						<div
							ref={refs.setFloating}
							style={{...floatingStyles, visibility: isPositioned ? 'visible' : 'hidden'}}
							className={styles.popoutContainer}
							onMouseDown={(e) => {
								if (e.button === 0) e.preventDefault();
							}}
						>
							<div className={styles.popoutInner}>
								<div className={`${styles.flex} ${styles.flexCol}`}>
									<div
										id={listboxId}
										role="listbox"
										aria-label={i18n._(msg`Search suggestions`)}
										className={styles.list}
									>
										{renderAutocompleteContent()}
									</div>
								</div>
							</div>
						</div>
					</FloatingPortal>
				)}
			</>
		);
	},
);
