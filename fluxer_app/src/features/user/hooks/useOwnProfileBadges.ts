// SPDX-License-Identifier: AGPL-3.0-or-later

import {useDebouncedAsyncResource} from '@app/features/platform/hooks/useDebouncedAsyncResource';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	profileBadgeSettingsKey,
	savedProfileBadgeSettingsKey,
} from '@app/features/user/commands/OwnProfileBadgesCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import OwnProfileBadges from '@app/features/user/state/OwnProfileBadges';
import UserProfile from '@app/features/user/state/UserProfile';
import Users from '@app/features/user/state/Users';
import type {
	ProfileBadgePreviewRequest,
	ProfileBadgePreviewResponse,
	ProfileBadgeResponse,
} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import {useCallback, useEffect, useMemo, useState} from 'react';

const logger = new Logger('useOwnProfileBadges');
const PREVIEW_DEBOUNCE_MS = 250;
const NO_BADGES: ReadonlyArray<ProfileBadgeResponse> = [];

export function useOwnProfileBadges(userId: string | null): ReadonlyArray<ProfileBadgeResponse> {
	const storedBadges = OwnProfileBadges.valueFor(userId)?.profile_badges;
	const cachedBadges = userId ? UserProfile.getProfile(userId)?.profileBadges : undefined;
	const needsProfile = userId !== null && storedBadges === undefined && cachedBadges === undefined;
	useEffect(() => {
		if (!userId || !needsProfile) {
			return;
		}
		UserProfileCommands.fetch(userId).catch((error: unknown) => {
			logger.warn('Failed to load profile badges', error);
		});
	}, [userId, needsProfile]);
	return storedBadges ?? cachedBadges ?? NO_BADGES;
}

export interface ProfileBadgePreviewState {
	status: 'pending' | 'failed' | 'ready';
	value: ProfileBadgePreviewResponse | undefined;
	retry: () => void;
}

export function useProfileBadgePreview(settings: ProfileBadgePreviewRequest | null): ProfileBadgePreviewState {
	const [retryAttempt, setRetryAttempt] = useState(0);
	const user = Users.currentUser;
	const stored = OwnProfileBadges.valueFor(user?.id ?? null);
	const savedKey = user ? savedProfileBadgeSettingsKey(user) : null;
	const settingsKey = settings ? profileBadgeSettingsKey(settings) : null;
	const storeServes =
		settingsKey !== null &&
		settingsKey === savedKey &&
		(stored !== null ? OwnProfileBadges.settingsKey === savedKey : OwnProfileBadges.loading);
	const request = useMemo(
		() => (settings && !storeServes ? {input: settings, load: UserProfileCommands.previewBadges, retryAttempt} : null),
		[settings, storeServes, retryAttempt],
	);
	const preview = useDebouncedAsyncResource(request, PREVIEW_DEBOUNCE_MS, (error) => {
		logger.warn('Failed to preview profile badges', error);
	});
	const retry = useCallback(() => setRetryAttempt((attempt) => attempt + 1), []);
	if (storeServes) {
		return {status: stored ? 'ready' : 'pending', value: stored ?? undefined, retry};
	}
	const value = preview?.value ?? stored ?? undefined;
	const status = preview?.status ?? (value ? 'ready' : 'pending');
	return {status, value, retry};
}
