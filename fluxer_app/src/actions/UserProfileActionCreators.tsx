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

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ME} from '~/Constants';
import {UserProfileModal} from '~/components/modals/UserProfileModal';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {type Profile, ProfileRecord} from '~/records/ProfileRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserProfileMobileStore from '~/stores/UserProfileMobileStore';
import UserProfileStore from '~/stores/UserProfileStore';
import UserStore from '~/stores/UserStore';

const logger = new Logger('UserProfiles');

const pendingRequests: Map<string, Promise<ProfileRecord>> = new Map();

function buildKey(userId: string, guildId?: string): string {
	return `${userId}:${guildId ?? ME}`;
}

export const fetch = async (userId: string, guildId?: string, force = false): Promise<ProfileRecord> => {
	try {
		const key = buildKey(userId, guildId);

		if (!force) {
			const existingProfile = UserProfileStore.getProfile(userId, guildId);
			if (existingProfile) {
				logger.debug(`Using cached profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
				return existingProfile;
			}
			const existingRequest = pendingRequests.get(key);
			if (existingRequest) {
				logger.debug(`Reusing in-flight profile request for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
				return existingRequest;
			}
		} else {
			const existingRequest = pendingRequests.get(key);
			if (existingRequest) {
				logger.debug(
					`Force refresh requested but request already in-flight for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`,
				);
				return existingRequest;
			} else {
				logger.debug(`Force refreshing profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
			}
		}

		logger.debug(`Fetching profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
		const promise = (async () => {
			const response = await http.get<Profile>({
				url: Endpoints.USER_PROFILE(userId),
				query: {
					...(guildId ? {guild_id: guildId} : {}),
					with_mutual_friends: true,
				},
			});
			const profile = response.body;
			const profileRecord = new ProfileRecord(profile, guildId);
			UserStore.handleUserUpdate(profile.user);
			UserProfileStore.handleProfileCreate(profileRecord);
			logger.debug(`Fetched and cached profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
			return profileRecord;
		})();

		pendingRequests.set(key, promise);

		try {
			const res = await promise;
			pendingRequests.delete(key);
			return res;
		} catch (e) {
			pendingRequests.delete(key);
			throw e;
		}
	} catch (error) {
		logger.error(`Failed to fetch profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}:`, error);
		throw error;
	}
};

export const invalidate = (userId: string, guildId?: string): void => {
	const scope = guildId ? ` in guild ${guildId}` : '';
	logger.debug(`Invalidating cached profile for user ${userId}${scope}`);
	try {
		UserProfileStore.handleProfileInvalidate(userId, guildId);
		pendingRequests.delete(buildKey(userId, guildId));
	} catch (err) {
		logger.warn('Failed to invalidate cached profile:', err);
	}
};

export const clearCurrentUserProfiles = (): void => {
	logger.debug('Clearing cached profiles for current user');
	try {
		UserProfileStore.handleProfilesClear();
		const currentUserId = AuthenticationStore.currentUserId;
		if (currentUserId) {
			for (const key of Array.from(pendingRequests.keys())) {
				if (key.startsWith(`${currentUserId}:`)) {
					pendingRequests.delete(key);
				}
			}
		}
	} catch (err) {
		logger.warn('Failed to clear current user profiles:', err);
	}
};

export const openUserProfile = (userId: string, guildId?: string, autoFocusNote?: boolean): void => {
	if (MobileLayoutStore.enabled) {
		UserProfileMobileStore.open(userId, guildId, autoFocusNote);
	} else {
		ModalActionCreators.push(
			modal(() => <UserProfileModal userId={userId} guildId={guildId} autoFocusNote={autoFocusNote} />),
		);
	}
};
