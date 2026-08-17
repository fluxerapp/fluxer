// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ForwardChannelSearchValues {
	readonly channelNameSearchValue: string;
	readonly displayNameSearchValue: string;
	readonly guildNameSearchValue: string;
	readonly isPersonalNotes: boolean;
	readonly searchAliasValues: ReadonlyArray<string>;
}

interface MatchesForwardChannelSearchRequest {
	readonly normalizedQuery: string;
	readonly personalNotesSearchValue: string;
	readonly values: ForwardChannelSearchValues;
}

export function matchesForwardChannelSearch({
	normalizedQuery,
	personalNotesSearchValue,
	values,
}: MatchesForwardChannelSearchRequest): boolean {
	if (values.isPersonalNotes && personalNotesSearchValue.includes(normalizedQuery)) return true;
	if (values.displayNameSearchValue.includes(normalizedQuery)) return true;
	if (values.channelNameSearchValue.includes(normalizedQuery)) return true;
	if (values.searchAliasValues.some((alias) => alias.includes(normalizedQuery))) return true;
	return values.guildNameSearchValue.includes(normalizedQuery);
}
