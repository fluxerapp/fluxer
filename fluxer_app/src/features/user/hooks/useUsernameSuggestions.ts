// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useUsernameSuggestions');

interface UseUsernameSuggestionsOptions {
	globalName: string;
	username: string;
	target: InstanceHTTPTarget;
	enabled?: boolean;
}

export function useUsernameSuggestions({globalName, username, target, enabled = true}: UseUsernameSuggestionsOptions) {
	const [suggestions, setSuggestions] = useState<Array<string>>([]);
	const debounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const abortControllerRef = useRef<AbortController | null>(null);
	const fetchSuggestions = useCallback(
		async (displayName: string) => {
			if (abortControllerRef.current) {
				abortControllerRef.current.abort();
			}
			if (!displayName || displayName.trim().length === 0) {
				setSuggestions([]);
				return;
			}
			try {
				abortControllerRef.current = new AbortController();
				const fetchedSuggestions = await AuthenticationCommands.getUsernameSuggestions(displayName, target);
				setSuggestions(fetchedSuggestions);
			} catch (error) {
				if (error instanceof Error && error.name !== 'AbortError') {
					logger.error('Failed to fetch username suggestions', error);
				}
				setSuggestions([]);
			}
		},
		[target],
	);
	useEffect(() => {
		if (!enabled) {
			return;
		}
		if (username && username.trim().length > 0) {
			setSuggestions([]);
			return;
		}
		if (debounceTimerRef.current) {
			clearTimeout(debounceTimerRef.current);
		}
		debounceTimerRef.current = setTimeout(() => {
			fetchSuggestions(globalName);
		}, 300);
		return () => {
			if (debounceTimerRef.current) {
				clearTimeout(debounceTimerRef.current);
			}
		};
	}, [enabled, globalName, username, fetchSuggestions]);
	return {suggestions};
}
