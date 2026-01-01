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

import {reaction} from 'mobx';
import {useCallback, useEffect, useRef} from 'react';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import WindowStore from '~/stores/WindowStore';

const UNFOCUS_UNSUBSCRIBE_DELAY_MS = 60000;
const INITIAL_MEMBER_RANGE: [number, number] = [0, 99];

interface UseMemberListSubscriptionOptions {
	guildId: string;
	channelId: string;
	enabled: boolean;
	allowInitialUnfocusedLoad?: boolean;
}

interface UseMemberListSubscriptionResult {
	subscribe: (ranges: Array<[number, number]>) => void;
	unsubscribe: () => void;
}

export function useMemberListSubscription({
	guildId,
	channelId,
	enabled,
	allowInitialUnfocusedLoad = false,
}: UseMemberListSubscriptionOptions): UseMemberListSubscriptionResult {
	const lastRangesRef = useRef<Array<[number, number]>>([INITIAL_MEMBER_RANGE]);
	const unfocusTimeoutRef = useRef<number | null>(null);
	const isSubscribedRef = useRef(false);
	const lastSessionVersionRef = useRef(MemberSidebarStore.sessionVersion);
	const lastGuildListVersionRef = useRef(MemberSidebarStore.lists[guildId]);
	const initialUnfocusedLoadAttemptedRef = useRef(false);

	const clearUnfocusTimeout = useCallback(() => {
		if (unfocusTimeoutRef.current !== null) {
			window.clearTimeout(unfocusTimeoutRef.current);
			unfocusTimeoutRef.current = null;
		}
	}, []);

	useEffect(() => {
		lastRangesRef.current = [INITIAL_MEMBER_RANGE];
		unfocusTimeoutRef.current = null;
		isSubscribedRef.current = false;
		lastSessionVersionRef.current = MemberSidebarStore.sessionVersion;
		lastGuildListVersionRef.current = MemberSidebarStore.lists[guildId];
		initialUnfocusedLoadAttemptedRef.current = false;
		clearUnfocusTimeout();
	}, [guildId, channelId, clearUnfocusTimeout]);

	const attemptSubscribe = useCallback(
		(ranges: Array<[number, number]>) => {
			if (!enabled) {
				return;
			}

			const windowFocused = WindowStore.focused;
			const allowUnfocusedLoad = allowInitialUnfocusedLoad && !initialUnfocusedLoadAttemptedRef.current;

			if (!windowFocused && !allowUnfocusedLoad) {
				return;
			}

			if (!windowFocused) {
				initialUnfocusedLoadAttemptedRef.current = true;
			}

			MemberSidebarStore.subscribeToChannel(guildId, channelId, ranges);
			isSubscribedRef.current = true;
		},
		[guildId, channelId, enabled, allowInitialUnfocusedLoad],
	);

	const subscribe = useCallback(
		(ranges: Array<[number, number]>) => {
			lastRangesRef.current = ranges;
			attemptSubscribe(ranges);
		},
		[attemptSubscribe],
	);

	const unsubscribe = useCallback(() => {
		clearUnfocusTimeout();
		if (isSubscribedRef.current) {
			MemberSidebarStore.unsubscribeFromChannel(guildId, channelId);
			isSubscribedRef.current = false;
		}
	}, [guildId, channelId, clearUnfocusTimeout]);

	const resubscribe = useCallback(() => {
		if (lastRangesRef.current.length > 0) {
			attemptSubscribe(lastRangesRef.current);
		}
	}, [attemptSubscribe]);

	useEffect(() => {
		if (!enabled) {
			unsubscribe();
			return;
		}

		if (WindowStore.focused) {
			resubscribe();
		}

		const disposeFocusReaction = reaction(
			() => WindowStore.focused,
			(focused) => {
				if (focused) {
					clearUnfocusTimeout();
					resubscribe();
				} else {
					clearUnfocusTimeout();
					unfocusTimeoutRef.current = window.setTimeout(() => {
						unfocusTimeoutRef.current = null;
						if (!WindowStore.focused && isSubscribedRef.current) {
							MemberSidebarStore.unsubscribeFromChannel(guildId, channelId);
							isSubscribedRef.current = false;
						}
					}, UNFOCUS_UNSUBSCRIBE_DELAY_MS);
				}
			},
		);

		const disposeSessionReaction = reaction(
			() => MemberSidebarStore.sessionVersion,
			(newVersion) => {
				if (newVersion !== lastSessionVersionRef.current) {
					lastSessionVersionRef.current = newVersion;
					isSubscribedRef.current = false;
					if (WindowStore.focused) {
						resubscribe();
					}
				}
			},
		);

		const disposeGuildListReaction = reaction(
			() => MemberSidebarStore.lists[guildId],
			(newGuildLists) => {
				const hadLists = lastGuildListVersionRef.current !== undefined;
				const hasLists = newGuildLists !== undefined;
				lastGuildListVersionRef.current = newGuildLists;

				if (hadLists && !hasLists) {
					isSubscribedRef.current = false;
				}

				if (!hasLists && WindowStore.focused && enabled) {
					resubscribe();
				}
			},
		);

		return () => {
			disposeFocusReaction();
			disposeSessionReaction();
			disposeGuildListReaction();
			clearUnfocusTimeout();
			unsubscribe();
		};
	}, [guildId, channelId, enabled, resubscribe, unsubscribe, clearUnfocusTimeout]);

	useEffect(() => {
		if (enabled && !isSubscribedRef.current) {
			attemptSubscribe(lastRangesRef.current);
		}
	}, [enabled, attemptSubscribe]);

	return {subscribe, unsubscribe};
}
