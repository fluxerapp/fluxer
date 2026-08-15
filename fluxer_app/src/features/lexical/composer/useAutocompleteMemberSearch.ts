// SPDX-License-Identifier: AGPL-3.0-or-later

import Guilds from '@app/features/guild/state/Guilds';
import {normalizeSlotAutocompleteQuery} from '@app/features/lexical/composer/SlashSlotAutocompleteQuery';
import type {SlashSlotAutocompleteContext} from '@app/features/lexical/composer/slashSlots';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch, {type SearchContext} from '@app/features/member/state/MemberSearch';
import {MEMBER_SEARCH_LIMIT} from '@app/features/messaging/utils/AutocompleteOptionBuilders';
import MentionFrecency from '@app/features/notification/state/MentionFrecency';
import {useEffect} from 'react';

interface MutableValue<T> {
	current: T;
}

interface MemberSearchLifecycle {
	triggerType: string | null;
	matchedText: string;
	guildId: string | null | undefined;
	searchContextRef: MutableValue<SearchContext | null>;
	currentGuildIdRef: MutableValue<string | null>;
	debounceTimerRef: MutableValue<ReturnType<typeof setTimeout> | null>;
	setResults: (results: Array<GuildMember>) => void;
}

interface SlotMemberSearchLifecycle {
	guildId: string | null | undefined;
	context: SlashSlotAutocompleteContext | null;
	searchContextRef: MutableValue<SearchContext | null>;
	currentGuildIdRef: MutableValue<string | null>;
	debounceTimerRef: MutableValue<ReturnType<typeof setTimeout> | null>;
	setResults: (results: Array<GuildMember>) => void;
}

export function isMemberSearchTrigger(triggerType: string | null): boolean {
	return triggerType === 'mention' || triggerType === 'commandArgMention' || triggerType === 'commandArg';
}

export function useAutocompleteMemberSearch({
	triggerType,
	matchedText,
	guildId,
	searchContextRef,
	currentGuildIdRef,
	debounceTimerRef,
	setResults,
}: MemberSearchLifecycle): void {
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			const currentGuildId = currentGuildIdRef.current;
			const guildMemberRecords: Array<GuildMember> = results
				.map((transformed) => {
					if (currentGuildId != null) {
						const member = GuildMembers.getMember(currentGuildId, transformed.id);
						return member == null ? null : member;
					}
					const guilds = Guilds.getGuilds();
					for (const guild of guilds) {
						const member = GuildMembers.getMember(guild.id, transformed.id);
						if (member != null) {
							return member;
						}
					}
					return null;
				})
				.filter((member): member is GuildMember => member != null);
			setResults(guildMemberRecords);
		}, MEMBER_SEARCH_LIMIT);
		searchContextRef.current = context;
		return () => {
			context.destroy();
			searchContextRef.current = null;
			if (debounceTimerRef.current != null) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
		};
	}, []);

	useEffect(() => {
		const context = searchContextRef.current;
		if (context == null) {
			return;
		}
		if (!isMemberSearchTrigger(triggerType) || guildId == null) {
			currentGuildIdRef.current = null;
			context.clearQuery();
			setResults([]);
			if (debounceTimerRef.current != null) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
			return;
		}
		currentGuildIdRef.current = guildId;
		context.setQuery(matchedText, {guild: guildId}, new Set(), new Set(), MentionFrecency.getBoosters(guildId));
		if (debounceTimerRef.current != null) {
			clearTimeout(debounceTimerRef.current);
		}
		debounceTimerRef.current = setTimeout(() => {
			void MemberSearch.fetchMembersInBackground(matchedText, [guildId]);
			debounceTimerRef.current = null;
		}, 300);
	}, [matchedText, triggerType, guildId]);
}

export function useAutocompleteSlotMemberSearch({
	guildId,
	context: slotContext,
	searchContextRef,
	currentGuildIdRef,
	debounceTimerRef,
	setResults,
}: SlotMemberSearchLifecycle): void {
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			const currentGuildId = currentGuildIdRef.current;
			if (currentGuildId == null) {
				setResults([]);
				return;
			}
			const members = results
				.map((result) => GuildMembers.getMember(currentGuildId, result.id))
				.filter((member): member is GuildMember => member != null);
			setResults(members);
		}, MEMBER_SEARCH_LIMIT);
		searchContextRef.current = context;
		return () => {
			context.destroy();
			searchContextRef.current = null;
			if (debounceTimerRef.current != null) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
		};
	}, []);

	useEffect(() => {
		const context = searchContextRef.current;
		if (context == null) {
			return;
		}
		if (debounceTimerRef.current != null) {
			clearTimeout(debounceTimerRef.current);
			debounceTimerRef.current = null;
		}
		if (slotContext == null || slotContext.optionType !== 'user' || guildId == null) {
			currentGuildIdRef.current = null;
			context.clearQuery();
			setResults([]);
			return;
		}
		const query = normalizeSlotAutocompleteQuery(slotContext);
		currentGuildIdRef.current = guildId;
		context.setQuery(query, {guild: guildId}, new Set(), new Set(), MentionFrecency.getBoosters(guildId));
		debounceTimerRef.current = setTimeout(() => {
			void MemberSearch.fetchMembersInBackground(query, [guildId]);
			debounceTimerRef.current = null;
		}, 300);
	}, [guildId, slotContext]);
}
