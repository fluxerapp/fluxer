// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyChannelSearchHighlight,
	clearChannelSearchHighlight,
} from '@app/features/messaging/utils/ChannelSearchHighlight';
import {tokenize} from '@app/features/search/utils/SearchQueryParser';
import {useLayoutEffect} from 'react';

interface UseChannelSearchHighlightRequest {
	isSuccess: boolean;
	searchQuery: string;
	resultsRevision: unknown;
	getHighlightRoot: () => HTMLElement | null;
}

export function useChannelSearchHighlight({
	isSuccess,
	searchQuery,
	resultsRevision,
	getHighlightRoot,
}: UseChannelSearchHighlightRequest): void {
	useLayoutEffect(() => {
		if (!isSuccess || !searchQuery.trim()) {
			clearChannelSearchHighlight();
			return;
		}
		const root = getHighlightRoot();
		if (root == null) return;
		const {content, exactPhrases} = tokenize(searchQuery);
		const terms = [...content.split(' ').filter(Boolean), ...exactPhrases];
		if (terms.length > 0) {
			applyChannelSearchHighlight(root, terms);
		}
		return () => {
			clearChannelSearchHighlight();
		};
	}, [isSuccess, searchQuery, resultsRevision]);
}
