// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AutocompleteType,
	HistoryFilterRow,
} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {resolveSearchChannelDisplayName} from '@app/features/search/utils/SearchQueryParser';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import type {MessageSearchScope, SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {IconProps} from '@phosphor-icons/react';
import {ChatCenteredDotsIcon, ChatCircleIcon, GlobeIcon, HashIcon, UsersIcon} from '@phosphor-icons/react';

export const SCOPE_ICON_COMPONENTS: Record<MessageSearchScope, React.ComponentType<IconProps>> = {
	current: HashIcon,
	all_dms: ChatCircleIcon,
	open_dms: ChatCenteredDotsIcon,
	all_guilds: GlobeIcon,
	all: UsersIcon,
	open_dms_and_all_guilds: UsersIcon,
};

export function resolveChannelSuggestionDisplayName(channel: Channel): string {
	if (channel.type === ChannelTypes.GROUP_DM || channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
		return ChannelUtils.getDMDisplayName(channel);
	}
	return resolveSearchChannelDisplayName(channel);
}

export function filterRequiresValue(filter: SearchFilterOption): boolean {
	return Boolean(filter.requiresValue) || (filter.values?.length ?? 0) > 0;
}

export const PLAINTEXT_SUGGESTION_FILTER_KEYS: ReadonlyArray<string> = ['from', 'in', 'mentions'];
export const PLAINTEXT_SUGGESTIONS_PER_FILTER = 3;
export const INLINE_FILTER_KEYS: ReadonlyArray<string> = ['from', 'in', 'has', 'mentions'];

export interface SearchFilterEligibility {
	isInGuildChannel: boolean;
	hidePersonalInformation: boolean;
}

export function isSearchFilterOptionEligible(
	option: SearchFilterOption,
	{isInGuildChannel, hidePersonalInformation}: SearchFilterEligibility,
): boolean {
	if (!option.requiresGuild) return true;
	if (isInGuildChannel) return true;
	return option.dmEligible === true && !hidePersonalInformation;
}

export function orderInlineFilterOptions(
	options: ReadonlyArray<SearchFilterOption>,
	eligibility: SearchFilterEligibility,
): Array<SearchFilterOption> {
	const eligible = options.filter((option) => isSearchFilterOptionEligible(option, eligibility));
	const byKey = new Map(eligible.map((option) => [option.key, option]));
	const ordered: Array<SearchFilterOption> = [];
	for (const key of INLINE_FILTER_KEYS) {
		const option = byKey.get(key);
		if (option != null) {
			ordered.push(option);
		}
	}
	return ordered;
}

export function listDiscoverableFilterOptions(
	options: ReadonlyArray<SearchFilterOption>,
	eligibility: SearchFilterEligibility,
): Array<SearchFilterOption> {
	return options.filter((option) => !option.key.startsWith('-') && isSearchFilterOptionEligible(option, eligibility));
}

export function buildHistoryFilterRows(
	options: ReadonlyArray<SearchFilterOption>,
	eligibility: SearchFilterEligibility,
): Array<HistoryFilterRow> {
	const promoted = orderInlineFilterOptions(options, eligibility);
	const promotedKeys = new Set(promoted.map((option) => option.key));
	const rest = listDiscoverableFilterOptions(options, eligibility).filter((option) => !promotedKeys.has(option.key));
	return [...promoted, ...rest].map((option) => ({kind: 'filter', option}));
}

const SEARCH_VALUE_MODES = new Set<AutocompleteType>(['users', 'channels', 'values', 'date']);

export function isSearchValueMode(autocompleteType: AutocompleteType): boolean {
	return SEARCH_VALUE_MODES.has(autocompleteType);
}

export interface MessageSearchCurrentWordRequest {
	value: string;
	cursorPosition: number;
}

const SEARCH_TOKEN_WHITESPACE = /\s/;

export function resolveMessageSearchCurrentWord({value, cursorPosition}: MessageSearchCurrentWordRequest): string {
	const boundedCursorPosition = Math.max(0, Math.min(cursorPosition, value.length));
	let wordStart = boundedCursorPosition;
	while (wordStart > 0) {
		const character = value.charAt(wordStart - 1);
		if (SEARCH_TOKEN_WHITESPACE.test(character)) {
			break;
		}
		wordStart -= 1;
	}
	return value.slice(wordStart, boundedCursorPosition);
}

export interface TokenInsertionResult {
	newText: string;
	newCursorPos: number;
	insertedDisplay: string;
	insertedLength: number;
}

export interface TokenInsertionInput {
	textBeforeCursor: string;
	textAfterCursor: string;
	lastWordStart: number;
	syntax: string;
	tokenValue: string;
	addSpaceAfter: boolean;
}

const SEARCH_TOKEN_QUOTE_TRIGGER = /[\\" ]/;
const SEARCH_TOKEN_ESCAPE = /[\\"]/g;

export function quoteSearchTokenValue(value: string): string {
	if (!SEARCH_TOKEN_QUOTE_TRIGGER.test(value)) return value;
	return `"${value.replace(SEARCH_TOKEN_ESCAPE, (match) => `\\${match}`)}"`;
}

export function computeTokenInsertion({
	textBeforeCursor,
	textAfterCursor,
	lastWordStart,
	syntax,
	tokenValue,
	addSpaceAfter,
}: TokenInsertionInput): TokenInsertionResult {
	const display = `${syntax}${quoteSearchTokenValue(tokenValue)}`;
	const before = textBeforeCursor.slice(0, lastWordStart);
	const isAlreadyFollowedBySpace = SEARCH_TOKEN_WHITESPACE.test(textAfterCursor.charAt(0));
	let space = '';
	if (!isAlreadyFollowedBySpace && (addSpaceAfter || textAfterCursor.length > 0)) {
		space = ' ';
	}
	let caretAdvance = 0;
	if (addSpaceAfter) {
		caretAdvance = 1;
	}
	return {
		newText: [before, display, space, textAfterCursor].join(''),
		newCursorPos: (before + display).length + caretAdvance,
		insertedDisplay: display,
		insertedLength: display.length + space.length,
	};
}

export interface SearchTokenReplacementInput {
	value: string;
	cursorPosition: number;
	currentSegments: ReadonlyArray<SearchSegment>;
	syntax: string;
	tokenValue: string;
	addSpaceAfter: boolean;
	replacementSegment: Omit<SearchSegment, 'start' | 'end' | 'displayText'> | null;
}

export interface SearchTokenReplacementResult {
	newText: string;
	newCursorPosition: number;
	newSegments: Array<SearchSegment>;
}

interface SearchSegmentInsertionShift {
	replacementStart: number;
	replacementEnd: number;
	lengthDelta: number;
}

function shiftSearchSegmentForInsertion(
	segment: SearchSegment,
	{replacementStart, replacementEnd, lengthDelta}: SearchSegmentInsertionShift,
): SearchSegment | null {
	if (segment.end <= replacementStart) {
		return segment;
	}
	if (segment.start >= replacementEnd) {
		return {...segment, start: segment.start + lengthDelta, end: segment.end + lengthDelta};
	}
	return null;
}

export function replaceSearchTokenAtCursor(input: SearchTokenReplacementInput): SearchTokenReplacementResult {
	const textBeforeCursor = input.value.slice(0, input.cursorPosition);
	const textAfterCursor = input.value.slice(input.cursorPosition);
	const currentWord = resolveMessageSearchCurrentWord({value: input.value, cursorPosition: input.cursorPosition});
	const replacementStart = textBeforeCursor.length - currentWord.length;
	const replacementEnd = input.cursorPosition;
	const insertion = computeTokenInsertion({
		textBeforeCursor,
		textAfterCursor,
		lastWordStart: replacementStart,
		syntax: input.syntax,
		tokenValue: input.tokenValue,
		addSpaceAfter: input.addSpaceAfter,
	});
	const lengthDelta = insertion.insertedLength - (replacementEnd - replacementStart);
	const segmentShift: SearchSegmentInsertionShift = {replacementStart, replacementEnd, lengthDelta};
	const newSegments = input.currentSegments
		.map((segment) => shiftSearchSegmentForInsertion(segment, segmentShift))
		.filter((segment): segment is SearchSegment => segment !== null);
	if (input.replacementSegment !== null) {
		newSegments.push({
			...input.replacementSegment,
			displayText: insertion.insertedDisplay,
			start: replacementStart,
			end: replacementStart + insertion.insertedDisplay.length,
		});
	}
	return {
		newText: insertion.newText,
		newCursorPosition: insertion.newCursorPos,
		newSegments: newSegments.sort((left, right) => left.start - right.start),
	};
}

export function deduplicateMembers(members: Array<GuildMember>): Array<GuildMember> {
	const seen = new Set<string>();
	const result: Array<GuildMember> = [];
	for (const member of members) {
		if (!seen.has(member.user.id)) {
			seen.add(member.user.id);
			result.push(member);
		}
	}
	return result;
}

export function assignRef<T>(ref: React.Ref<T> | undefined, value: T | null): void {
	if (!ref) {
		return;
	}
	if (typeof ref === 'function') {
		ref(value);
		return;
	}
	(ref as React.MutableRefObject<T | null>).current = value;
}

export function normalizeFilterKey(filterKey: string): string {
	return filterKey.replace(/^-/, '');
}

export function isDateFilterKey(filterKey: string): boolean {
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

export function isUserFilterKey(filterKey: string): boolean {
	switch (normalizeFilterKey(filterKey)) {
		case 'from':
		case 'mentions':
			return true;
		default:
			return false;
	}
}

export type DmChannelSuggestionMode = 'none' | 'current' | 'open' | 'all';
export type GuildChannelSuggestionMode = 'none' | 'current_guild' | 'all_guilds';

export interface ChannelSuggestionSearchPlan {
	dmMode: DmChannelSuggestionMode;
	guildMode: GuildChannelSuggestionMode;
	currentGuildId?: string;
}

export function getChannelSuggestionSearchPlan(
	scope: MessageSearchScope,
	currentGuildId: string | undefined,
): ChannelSuggestionSearchPlan {
	switch (scope) {
		case 'current':
			return currentGuildId
				? {dmMode: 'none', guildMode: 'current_guild', currentGuildId}
				: {dmMode: 'current', guildMode: 'none'};
		case 'all_dms':
			return {dmMode: 'all', guildMode: 'none'};
		case 'open_dms':
			return {dmMode: 'open', guildMode: 'none'};
		case 'all':
			return {dmMode: 'all', guildMode: 'all_guilds'};
		case 'open_dms_and_all_guilds':
			return {dmMode: 'open', guildMode: 'all_guilds'};
		case 'all_guilds':
			return {dmMode: 'none', guildMode: 'all_guilds'};
		default: {
			const exhaustiveScope: never = scope;
			throw new Error(`Unsupported message search scope: ${exhaustiveScope}`);
		}
	}
}

export type GuildSearchMode = 'none' | 'current_guild' | 'all_guilds';

export interface UserGuildSearchPlan {
	mode: GuildSearchMode;
	guildsToSearch: Array<Guild> | null;
	priorityGuildId?: string;
	workerFilters: {
		friends?: boolean;
		guild?: string;
	};
}

export function getUserGuildSearchPlan(
	scope: MessageSearchScope,
	currentGuildId: string | undefined,
): UserGuildSearchPlan {
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
		const guild = Guilds.getGuild(currentGuildId);
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
			guildsToSearch: Guilds.getGuilds(),
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

export type MemberSearchBoosters = Record<string, number>;

export function buildUserSearchBoosters(
	channel: Channel | undefined,
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
		const members = GuildMembers.getMembers(currentGuildId);
		const MAX_BOOSTED_MEMBERS = 300;
		for (let i = 0; i < members.length && i < MAX_BOOSTED_MEMBERS; i += 1) {
			const id = members[i]!.user.id;
			boosters[id] = Math.max(boosters[id] ?? 1, 2);
		}
	}
	return boosters;
}
