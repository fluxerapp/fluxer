// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useMemo, useRef, useState} from 'react';

export const UNREAD_REFREEZE_COOLDOWN_MS = 10_000;

export interface FrozenUnreadOrder {
	order: Map<string, number>;
}

export function selectFrozenUnreadChannels<T extends {id: string}>(
	order: ReadonlyMap<string, number>,
	live: ReadonlyArray<T>,
): Array<T> {
	const held: Array<{index: number; channel: T}> = [];
	for (const channel of live) {
		const index = order.get(channel.id);
		if (index == null) continue;
		held.push({index, channel});
	}
	held.sort((a, b) => a.index - b.index);
	return held.map((entry) => entry.channel);
}

export function useFrozenUnreadOrder<S extends FrozenUnreadOrder, T extends {id: string}>(
	takeSnapshot: () => S,
	live: ReadonlyArray<T>,
): {snapshot: S; channels: Array<T>} {
	const [snapshot, setSnapshot] = useState(takeSnapshot);
	const [cooldownAttempt, setCooldownAttempt] = useState(0);
	const snapshotTakenAtRef = useRef(Date.now());
	const hasShownFrozenListRef = useRef(false);
	const channels = useMemo(() => selectFrozenUnreadChannels(snapshot.order, live), [snapshot, live]);
	useEffect(() => {
		if (channels.length > 0) {
			hasShownFrozenListRef.current = true;
			return undefined;
		}
		const next = takeSnapshot();
		if (next.order.size === 0) {
			if (snapshot.order.size > 0) {
				setSnapshot(next);
			}
			return undefined;
		}
		if (hasShownFrozenListRef.current) {
			const heldFor = Date.now() - snapshotTakenAtRef.current;
			if (heldFor < UNREAD_REFREEZE_COOLDOWN_MS) {
				const timer = setTimeout(
					() => setCooldownAttempt((attempt) => attempt + 1),
					UNREAD_REFREEZE_COOLDOWN_MS - heldFor,
				);
				return () => clearTimeout(timer);
			}
		}
		snapshotTakenAtRef.current = Date.now();
		hasShownFrozenListRef.current = false;
		setSnapshot(next);
		return undefined;
	}, [channels, snapshot, cooldownAttempt]);
	return {snapshot, channels};
}
