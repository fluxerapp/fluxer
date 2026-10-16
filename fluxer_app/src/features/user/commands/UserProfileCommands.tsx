// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Initialization from '@app/features/app/state/Initialization';
import Authentication from '@app/features/auth/state/Authentication';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {UserProfileModal} from '@app/features/user/components/modals/UserProfileModal';
import {Profile, type ProfileWire} from '@app/features/user/models/Profile';
import UserProfile from '@app/features/user/state/UserProfile';
import UserProfileMobile from '@app/features/user/state/UserProfileMobile';
import Users from '@app/features/user/state/Users';
import {ME} from '@fluxer/constants/src/AppConstants';

const logger = new Logger('UserProfiles');
const pendingRequests: Map<string, Promise<Profile>> = new Map();

function buildKey(userId: string, guildId?: string): string {
	return `${userId}:${guildId ?? ME}`;
}

function profileScope(guildId?: string): string {
	return guildId ? ` in guild ${guildId}` : '';
}

function profileQuery(guildId?: string): Record<string, string | boolean> {
	return {
		...(guildId ? {guild_id: guildId} : {}),
		with_mutual_friends: true,
		with_mutual_guilds: true,
	};
}

function cachedProfileOrRequest(userId: string, guildId?: string): Profile | Promise<Profile> | null {
	const existingProfile = UserProfile.getProfile(userId, guildId);
	if (existingProfile) {
		logger.debug(`Using cached profile for user ${userId}${profileScope(guildId)}`);
		return existingProfile;
	}
	const existingRequest = pendingRequests.get(buildKey(userId, guildId));
	if (existingRequest) {
		logger.debug(`Reusing in-flight profile request for user ${userId}${profileScope(guildId)}`);
		return existingRequest;
	}
	return null;
}

async function requestProfile(userId: string, guildId?: string): Promise<ProfileWire> {
	const response = await http.get<ProfileWire>(Endpoints.USER_PROFILE(userId), {
		query: profileQuery(guildId),
	});
	return response.body;
}

function storeProfile(profile: ProfileWire, guildId?: string): Profile {
	Users.handleUserUpdate(profile.user);
	const profileRecord = new Profile(profile, guildId);
	UserProfile.handleProfileCreate(profileRecord);
	return profileRecord;
}

export async function fetch(userId: string, guildId?: string): Promise<Profile> {
	try {
		const key = buildKey(userId, guildId);
		const cached = cachedProfileOrRequest(userId, guildId);
		if (cached) return cached;
		logger.debug(`Fetching profile for user ${userId}${profileScope(guildId)}`);
		const promise = (async () => {
			const profileRecord = storeProfile(await requestProfile(userId, guildId), guildId);
			logger.debug(`Fetched and cached profile for user ${userId}${profileScope(guildId)}`);
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
		logger.error(`Failed to fetch profile for user ${userId}${profileScope(guildId)}:`, error);
		throw error;
	}
}

export function invalidate(userId: string, guildId?: string): void {
	const scope = guildId ? ` in guild ${guildId}` : '';
	logger.debug(`Invalidating cached profile for user ${userId}${scope}`);
	try {
		UserProfile.handleProfileInvalidate(userId, guildId);
		pendingRequests.delete(buildKey(userId, guildId));
	} catch (err) {
		logger.warn('Failed to invalidate cached profile:', err);
	}
}

export function clearCurrentUserProfiles(): void {
	logger.debug('Clearing cached profiles for current user');
	try {
		UserProfile.handleProfilesClear();
		const currentUserId = Authentication.currentUserId;
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
}

export function canOpenUserProfileSurface(): boolean {
	return Authentication.isAuthenticated && SessionManager.isConnected && Initialization.canNavigateToProtectedRoutes;
}

export function closeUserProfileSurfaces(): void {
	PopoutCommands.closeAll();
	ContextMenuCommands.close();
	UserProfileMobile.close();
	ModalCommands.popAllByType(UserProfileModal);
}

export function openUserProfile(userId: string, guildId?: string, autoFocusNote?: boolean): boolean {
	if (!canOpenUserProfileSurface()) {
		logger.debug(
			`Skipping profile open while protected routes are unavailable for user ${userId}${profileScope(guildId)}`,
		);
		return false;
	}
	if (MobileLayout.enabled) {
		UserProfileMobile.open(userId, guildId, autoFocusNote);
	} else {
		ModalCommands.push(
			modal(() => (
				<UserProfileModal
					userId={userId}
					guildId={guildId}
					autoFocusNote={autoFocusNote}
					data-flx="user.user-profile-commands.open-user-profile.user-profile-modal"
				/>
			)),
		);
	}
	return true;
}

export async function openLinkedUserProfile(
	userId: string,
	guildId?: string,
	autoFocusNote?: boolean,
): Promise<boolean> {
	if (!canOpenUserProfileSurface()) {
		logger.debug(`Skipping linked profile open before fetch for user ${userId}${profileScope(guildId)}`);
		return false;
	}
	try {
		await fetch(userId, guildId);
	} catch (error) {
		logger.error(`Failed to fetch linked profile for user ${userId}${guildId ? ` in guild ${guildId}` : ''}:`, error);
		return false;
	}
	return openUserProfile(userId, guildId, autoFocusNote);
}
