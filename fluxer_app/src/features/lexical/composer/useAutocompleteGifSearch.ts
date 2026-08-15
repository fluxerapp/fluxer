// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';
import * as GifCommands from '@app/features/expressions/commands/GifCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useEffect} from 'react';

const logger = new Logger('useLexicalAutocomplete');

export interface GifAutocompleteSearchState {
	status: 'idle' | 'loading' | 'success' | 'error';
	query: string;
	results: Array<Gif>;
}

interface MutableValue<T> {
	current: T;
}

interface GifAutocompleteSearchLifecycle {
	triggerType: string | null;
	query: string;
	cacheRef: MutableValue<Map<string, Array<Gif>>>;
	currentSearchRef: MutableValue<string | null>;
	debounceTimerRef: MutableValue<ReturnType<typeof setTimeout> | null>;
	setState: (
		state: GifAutocompleteSearchState | ((previous: GifAutocompleteSearchState) => GifAutocompleteSearchState),
	) => void;
}

export function useAutocompleteGifSearch({
	triggerType,
	query,
	cacheRef,
	currentSearchRef,
	debounceTimerRef,
	setState,
}: GifAutocompleteSearchLifecycle): void {
	useEffect(() => {
		let disposed = false;
		if (debounceTimerRef.current != null) {
			clearTimeout(debounceTimerRef.current);
			debounceTimerRef.current = null;
		}
		if (triggerType !== 'gif') {
			currentSearchRef.current = null;
			setState((previous) => (previous.status === 'idle' ? previous : {status: 'idle', query: '', results: []}));
			return;
		}
		if (query.length === 0) {
			currentSearchRef.current = null;
			setState({status: 'idle', query: '', results: []});
			return;
		}
		if (currentSearchRef.current === query) {
			return;
		}
		const cachedResults = cacheRef.current.get(query);
		if (cachedResults != null) {
			currentSearchRef.current = query;
			setState({status: 'success', query, results: cachedResults});
			return;
		}
		debounceTimerRef.current = setTimeout(() => {
			debounceTimerRef.current = null;
			currentSearchRef.current = query;
			setState({status: 'loading', query, results: []});
			GifCommands.search(query)
				.then((gifs) => {
					cacheRef.current.set(query, gifs);
					if (disposed || currentSearchRef.current !== query) {
						return;
					}
					setState({status: 'success', query, results: gifs});
				})
				.catch((error) => {
					if (disposed || currentSearchRef.current !== query) {
						return;
					}
					logger.error('GIF search failed', error);
					setState({status: 'error', query, results: []});
				});
		}, 300);
		return () => {
			disposed = true;
			if (debounceTimerRef.current != null) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
		};
	}, [query, triggerType]);
}
