// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {ActivityEmitter} from '@app/features/presence/state/ActivityEmitter';
import ActivityManager from '@app/features/presence/state/ActivityManager';
import Presence from '@app/features/presence/state/Presence';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {useCallback, useSyncExternalStore} from 'react';

interface UsePresenceActivitiesOptions {
	userId: string;
	enabled?: boolean;
}

const EMPTY_ACTIVITIES: Array<UserActivity> = [];

let cachedLocalVersion = -1;
let cachedLocalActivities: Array<UserActivity> = EMPTY_ACTIVITIES;

function getLocalActivitiesSnapshot(): Array<UserActivity> {
	const version = ActivityManager.activityVersion;
	if (version === cachedLocalVersion) {
		return cachedLocalActivities;
	}
	cachedLocalVersion = version;
	cachedLocalActivities = ActivityManager.getActivities();
	return cachedLocalActivities;
}

function getCurrentUserActivitiesSnapshot(userId: string): Array<UserActivity> {
	const localActivities = getLocalActivitiesSnapshot();
	if (localActivities.length > 0) {
		return localActivities;
	}
	return Presence.getActivities(userId);
}

export function usePresenceActivities({userId, enabled = true}: UsePresenceActivitiesOptions): Array<UserActivity> {
	const subscribe = useCallback(
		(onChange: () => void) => {
			if (!enabled) return () => {};
			if (userId === Authentication.currentUserId && userId) {
				const unsubscribeLocal = ActivityEmitter.subscribeToPresence('local', onChange);
				const unsubscribeRemote = ActivityEmitter.subscribeToPresence(userId, onChange);
				return () => {
					unsubscribeLocal();
					unsubscribeRemote();
				};
			}
			return ActivityEmitter.subscribeToPresence(userId, onChange);
		},
		[userId, enabled],
	);
	const getSnapshot = useCallback((): Array<UserActivity> => {
		if (!enabled) return EMPTY_ACTIVITIES;
		if (userId === Authentication.currentUserId && userId) {
			return getCurrentUserActivitiesSnapshot(userId);
		}
		return Presence.getActivities(userId);
	}, [userId, enabled]);
	return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}
