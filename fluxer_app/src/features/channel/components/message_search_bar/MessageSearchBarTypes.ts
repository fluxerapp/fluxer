// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {LexicalSearchInputHandle} from '@app/features/lexical/search/LexicalSearchInput';
import type {SearchHistoryEntry} from '@app/features/search/state/SearchHistory';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import type {SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import type {User} from '@app/features/user/models/User';

export interface SearchBarProps {
	channel?: Channel;
	value: string;
	onChange: (value: string, segments: Array<SearchSegment>) => void;
	onSearch: () => void;
	onClear: () => void;
	isResultsOpen?: boolean;
	onCloseResults?: () => void;
	inputRefExternal?: React.Ref<LexicalSearchInputHandle>;
	highContrast?: boolean;
}

export type AutocompleteType = 'filters' | 'users' | 'channels' | 'values' | 'date' | 'history' | null;

export interface SearchHints {
	usersByTag: Record<string, string>;
	channelsByName: Record<string, string>;
}

export type AutocompleteOption =
	| SearchFilterOption
	| User
	| Channel
	| {
			value: string;
			label: string;
	  }
	| string
	| SearchHistoryEntry;
