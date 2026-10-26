// SPDX-License-Identifier: AGPL-3.0-or-later

import {useDebouncedAsyncResource} from '@app/features/platform/hooks/useDebouncedAsyncResource';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import UserProfile from '@app/features/user/state/UserProfile';
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
	const cachedBadges = userId ? UserProfile.getProfile(userId)?.profileBadges : undefined;
	const hasCachedProfile = cachedBadges !== undefined;
	useEffect(() => {
		if (!userId || hasCachedProfile) {
			return;
		}
		UserProfileCommands.fetch(userId).catch((error: unknown) => {
			logger.warn('Failed to load profile badges', error);
		});
	}, [userId, hasCachedProfile]);
	return cachedBadges ?? NO_BADGES;
}

export interface ProfileBadgePreviewState {
	status: 'pending' | 'failed' | 'ready';
	value: ProfileBadgePreviewResponse | undefined;
	retry: () => void;
}

export function useProfileBadgePreview(settings: ProfileBadgePreviewRequest | null): ProfileBadgePreviewState {
	const [retryAttempt, setRetryAttempt] = useState(0);
	const request = useMemo(
		() => (settings ? {input: settings, load: UserProfileCommands.previewBadges, retryAttempt} : null),
		[settings, retryAttempt],
	);
	const preview = useDebouncedAsyncResource(request, PREVIEW_DEBOUNCE_MS, (error) => {
		logger.warn('Failed to preview profile badges', error);
	});
	const retry = useCallback(() => setRetryAttempt((attempt) => attempt + 1), []);
	return {status: preview?.status ?? 'pending', value: preview?.value, retry};
}
