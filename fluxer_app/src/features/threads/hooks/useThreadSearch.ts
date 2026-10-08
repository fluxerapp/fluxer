// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {searchRetryDelayMs} from '@app/features/forum/state/ForumPosts';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {useCallback, useEffect, useState} from 'react';

export const THREAD_SEARCH_DEBOUNCE_MS = 300;
const MAX_INDEX_RETRIES = 6;

export type ThreadSearchState =
	| {status: 'idle'}
	| {status: 'loading'}
	| {status: 'indexing'}
	| {status: 'failed'}
	| {status: 'done'; threadIds: ReadonlyArray<string>};

export function useThreadSearch(
	parentId: string,
	query: string,
	archived: boolean,
): {
	state: ThreadSearchState;
	retry: () => void;
} {
	const [state, setState] = useState<ThreadSearchState>({status: 'idle'});
	const [attempt, setAttempt] = useState(0);
	const trimmed = query.trim();
	useEffect(() => {
		if (!trimmed) {
			setState({status: 'idle'});
			return;
		}
		let cancelled = false;
		let timer: ReturnType<typeof setTimeout> | null = null;
		setState((previous) => (previous.status === 'indexing' ? previous : {status: 'loading'}));
		const run = (indexRetries: number) => {
			const parent: Channel | undefined = Channels.getChannel(parentId);
			if (!parent) return;
			ThreadCommands.searchThreads(parent, trimmed, archived)
				.then((result) => {
					if (cancelled) return;
					if (result.status === 'ok') {
						setState({status: 'done', threadIds: result.threadIds});
						return;
					}
					if (indexRetries >= MAX_INDEX_RETRIES) {
						setState({status: 'failed'});
						return;
					}
					setState({status: 'indexing'});
					timer = setTimeout(() => run(indexRetries + 1), searchRetryDelayMs(result.retryAfterSeconds, indexRetries));
				})
				.catch(() => {
					if (!cancelled) setState({status: 'failed'});
				});
		};
		timer = setTimeout(() => run(0), THREAD_SEARCH_DEBOUNCE_MS);
		return () => {
			cancelled = true;
			if (timer) clearTimeout(timer);
		};
	}, [parentId, trimmed, archived, attempt]);
	const retry = useCallback(() => setAttempt((value) => value + 1), []);
	return {state, retry};
}
