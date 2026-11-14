// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch, {type SearchContext} from '@app/features/member/state/MemberSearch';
import {MEMBER_SEARCH_LIMIT} from '@app/features/messaging/utils/AutocompleteOptionBuilders';
import {useEffect, useRef} from 'react';

const MEMBER_FETCH_DEBOUNCE_MS = 200;
interface GuildMemberAutocompleteSearchOptions {
	guildId: string | null | undefined;
	query: string | null;
	setResults: (results: Array<GuildMember>) => void;
}
export function isMemberSearchTrigger(triggerType: string | null): boolean {
	return triggerType === 'mention' || triggerType === 'commandArgMention' || triggerType === 'commandArg';
}
export function useGuildMemberAutocompleteSearch({
	guildId,
	query,
	setResults,
}: GuildMemberAutocompleteSearchOptions): void {
	const searchContextRef = useRef<SearchContext | null>(null);
	const currentGuildIdRef = useRef<string | null>(null);
	const debounceTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			const currentGuildId = currentGuildIdRef.current;
			if (currentGuildId === null) {
				setResults([]);
				return;
			}
			setResults(
				results
					.map((result) => GuildMembers.getMember(currentGuildId, result.id))
					.filter((member): member is GuildMember => member != null),
			);
		}, MEMBER_SEARCH_LIMIT);
		searchContextRef.current = context;
		return () => {
			context.destroy();
			searchContextRef.current = null;
			if (debounceTimerRef.current !== null) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
		};
	}, []);
	useEffect(() => {
		const context = searchContextRef.current;
		if (context === null) return;
		if (debounceTimerRef.current !== null) {
			clearTimeout(debounceTimerRef.current);
			debounceTimerRef.current = null;
		}
		if (query === null || guildId == null) {
			currentGuildIdRef.current = null;
			context.cancelSearch();
			setResults([]);
			return;
		}
		currentGuildIdRef.current = guildId;
		context.beginSearch(query, {guild: guildId}, new Set(), new Set());
		debounceTimerRef.current = setTimeout(() => {
			void MemberSearch.fetchMembersInBackground(query, [guildId]);
			debounceTimerRef.current = null;
		}, MEMBER_FETCH_DEBOUNCE_MS);
	}, [query, guildId]);
}
