// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useUsernameSuggestions');
const SUGGESTION_DEBOUNCE_MS = 300;

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
			if (!displayName || displayName.trim().length === 0) {
				setSuggestions([]);
				return;
			}
			let controller: AbortController | undefined;
			try {
				controller = new AbortController();
				abortControllerRef.current = controller;
				const fetchedSuggestions = await AuthenticationCommands.getUsernameSuggestions(displayName, target);
				if (controller.signal.aborted) {
					return;
				}
				setSuggestions(fetchedSuggestions);
			} catch (error) {
				if (controller?.signal.aborted) {
					return;
				}
				if (!isAccountTransitionAbortError(error) && !(error instanceof Error && error.name === 'AbortError')) {
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
		}, SUGGESTION_DEBOUNCE_MS);
		return () => {
			if (debounceTimerRef.current) {
				clearTimeout(debounceTimerRef.current);
			}
			abortControllerRef.current?.abort();
		};
	}, [enabled, globalName, username, fetchSuggestions]);
	return {suggestions};
}
