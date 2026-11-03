// SPDX-License-Identifier: AGPL-3.0-or-later

import Presence from '@app/features/presence/state/Presence';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {isCustomStatusExpired} from '@app/features/user/state/CustomStatus';
import {CustomStatusEmitter} from '@app/features/user/state/CustomStatusEmitter';
import {useCallback, useEffect, useRef, useState, useSyncExternalStore} from 'react';

interface UsePresenceCustomStatusOptions {
	userId: string;
	enabled?: boolean;
}

export function filterExpiredStatus<T extends CustomStatusResult>(status: T): T | null {
	if (status === undefined) {
		return status;
	}
	if (status === null || isCustomStatusExpired(status)) {
		return null;
	}
	return status;
}

export function usePresenceCustomStatus({userId, enabled = true}: UsePresenceCustomStatusOptions): CustomStatus | null {
	const expiryTick = useCustomStatusExpiryTick(enabled ? Presence.getCustomStatus(userId)?.expiresAt : undefined);
	const subscribe = useCallback(
		(onChange: () => void) => {
			if (!enabled) {
				return () => {};
			}
			return CustomStatusEmitter.subscribeToPresence(userId, onChange);
		},
		[userId, enabled],
	);
	const getSnapshot = useCallback((): CustomStatus | null => {
		if (!enabled) {
			return null;
		}
		return filterExpiredStatus(Presence.getCustomStatus(userId));
	}, [userId, enabled, expiryTick]);
	const customStatus = useSyncExternalStore(subscribe, getSnapshot, getSnapshot);

	return customStatus;
}

type CustomStatusResult = CustomStatus | null | undefined;

export function useCustomStatusExpiryTick(expiresAt: string | null | undefined): number {
	const [expiryTick, setExpiryTick] = useState(0);
	const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const clearTimer = useCallback(() => {
		if (timerRef.current !== null) {
			clearTimeout(timerRef.current);
			timerRef.current = null;
		}
	}, []);
	useEffect(() => {
		clearTimer();
		if (!expiresAt) return;
		const expiresAtMs = Date.parse(expiresAt);
		if (Number.isNaN(expiresAtMs)) return;
		const delay = expiresAtMs - Date.now();
		if (delay <= 0) return;
		timerRef.current = setTimeout(() => setExpiryTick((t) => t + 1), delay);
		return clearTimer;
	}, [expiresAt, clearTimer]);
	return expiryTick;
}
