// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import {abandonUnreachableLastLocation} from '@app/features/navigation/utils/ChannelRouteReachability';
import {
	AppStorageKey,
	LEGACY_SHARED_CONTENT_REVIEW_KEY,
	LegacySharedContentReviewState,
} from '@app/features/platform/state/AppStorageKeys';
import AppStorage, {getAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import {awaitHydration} from '@app/features/platform/utils/MobXPersistence';

const LOCATION_STORAGE_KEY = 'Location';

function reviewIsPending(): boolean {
	return AppStorage.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY) === LegacySharedContentReviewState.PENDING;
}

export async function reviewLegacySharedContent(): Promise<void> {
	if (!reviewIsPending() || GuildAvailability.unavailableGuilds.size > 0) {
		return;
	}
	const scope = getAppStorageScope();
	await Promise.all([awaitHydration(AppStorageKey.MESSAGING_DRAFTS), awaitHydration(LOCATION_STORAGE_KEY)]);
	if (getAppStorageScope() !== scope || !reviewIsPending() || GuildAvailability.unavailableGuilds.size > 0) {
		return;
	}
	for (const [channelId] of Drafts.getAllDrafts()) {
		if (Channels.getChannel(channelId) === undefined) {
			Drafts.deleteDraft(channelId);
		}
	}
	abandonUnreachableLastLocation();
	AppStorage.setItem(LEGACY_SHARED_CONTENT_REVIEW_KEY, LegacySharedContentReviewState.DONE);
}
