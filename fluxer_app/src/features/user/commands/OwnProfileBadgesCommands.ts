// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import OwnProfileBadges from '@app/features/user/state/OwnProfileBadges';
import Users from '@app/features/user/state/Users';
import {warmProfileBadgeImages} from '@app/features/user/utils/ProfileUtils';
import {isProfileBadgeKey} from '@fluxer/constants/src/BadgeConstants';
import type {ProfileBadgePreviewRequest} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';

const logger = new Logger('OwnProfileBadges');

let pendingRefresh: Promise<void> | null = null;
let refreshAgain = false;

export function profileBadgeSettingsKey(settings: ProfileBadgePreviewRequest): string {
	return JSON.stringify([
		(settings.hidden_profile_badges ?? []).filter(isProfileBadgeKey).sort(),
		settings.premium_badge_timestamp_hidden ?? false,
		settings.premium_badge_masked ?? false,
		settings.operator_pass_badge_timestamp_hidden ?? false,
		settings.operator_pass_badge_count_hidden ?? false,
	]);
}

export function savedProfileBadgeSettingsKey(user: User): string {
	return profileBadgeSettingsKey({
		hidden_profile_badges: (user.hiddenProfileBadges ?? []).filter(isProfileBadgeKey),
		premium_badge_timestamp_hidden: user.premiumBadgeTimestampHidden,
		premium_badge_masked: user.premiumBadgeMasked,
		operator_pass_badge_timestamp_hidden: user.operatorPassBadgeTimestampHidden,
		operator_pass_badge_count_hidden: user.operatorPassBadgeCountHidden,
	});
}

async function load(): Promise<void> {
	const user = Users.currentUser;
	if (!user) return;
	const userId = user.id;
	const settingsKey = savedProfileBadgeSettingsKey(user);
	OwnProfileBadges.beginLoad(userId);
	try {
		const value = await UserProfileCommands.previewBadges({});
		if (Users.currentUser?.id !== userId) {
			OwnProfileBadges.finishLoad();
			return;
		}
		warmProfileBadgeImages(value.own_badges);
		OwnProfileBadges.setValue(userId, settingsKey, value);
	} catch (error) {
		OwnProfileBadges.finishLoad();
		if (!isAbortError(error)) {
			logger.warn('Failed to load your badges', error);
		}
	}
}

export function refresh(): Promise<void> {
	if (pendingRefresh !== null) {
		refreshAgain = true;
		return pendingRefresh;
	}
	const request = (async () => {
		do {
			refreshAgain = false;
			await load();
		} while (refreshAgain);
	})().finally(() => {
		if (pendingRefresh === request) pendingRefresh = null;
	});
	pendingRefresh = request;
	return request;
}

export function refreshIfSettingsChanged(): void {
	const user = Users.currentUser;
	if (!user || OwnProfileBadges.loadedForUserId !== user.id) return;
	if (OwnProfileBadges.settingsKey !== savedProfileBadgeSettingsKey(user)) {
		void refresh();
	}
}
