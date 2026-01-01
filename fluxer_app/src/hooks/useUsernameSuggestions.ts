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

import {useCallback, useEffect, useRef, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';

interface UseUsernameSuggestionsOptions {
	globalName: string;
	username: string;
	debounceMs?: number;
}

export function useUsernameSuggestions({globalName, username, debounceMs = 300}: UseUsernameSuggestionsOptions) {
	const [suggestions, setSuggestions] = useState<Array<string>>([]);
	const debounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const abortControllerRef = useRef<AbortController | null>(null);

	const fetchSuggestions = useCallback(async (displayName: string) => {
		if (abortControllerRef.current) {
			abortControllerRef.current.abort();
		}

		if (!displayName || displayName.trim().length === 0) {
			setSuggestions([]);
			return;
		}

		try {
			abortControllerRef.current = new AbortController();
			const fetchedSuggestions = await AuthenticationActionCreators.getUsernameSuggestions(displayName);
			setSuggestions(fetchedSuggestions);
		} catch (error) {
			if (error instanceof Error && error.name !== 'AbortError') {
				console.error('Failed to fetch username suggestions:', error);
			}
			setSuggestions([]);
		}
	}, []);

	useEffect(() => {
		if (username && username.trim().length > 0) {
			setSuggestions([]);
			return;
		}

		if (debounceTimerRef.current) {
			clearTimeout(debounceTimerRef.current);
		}

		debounceTimerRef.current = setTimeout(() => {
			fetchSuggestions(globalName);
		}, debounceMs);

		return () => {
			if (debounceTimerRef.current) {
				clearTimeout(debounceTimerRef.current);
			}
		};
	}, [globalName, username, debounceMs, fetchSuggestions]);

	return {suggestions};
}
