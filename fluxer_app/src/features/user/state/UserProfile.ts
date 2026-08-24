// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {Profile} from '@app/features/user/models/Profile';
import {ME} from '@fluxer/constants/src/AppConstants';
import {makeAutoObservable, runInAction} from 'mobx';

type ProfilesByGuildId = Record<string, Profile>;

const PROFILE_TIMEOUT_MS = 60000;

class UserProfile {
	private logger = new Logger('UserProfile');
	profiles: Record<string, ProfilesByGuildId> = {};
	profileTimeouts: Record<string, NodeJS.Timeout> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getProfile(userId: string, guildId?: string): Profile | null {
		return this.profiles[userId]?.[guildId ?? ME] ?? null;
	}

	handleGatewayReady(): void {
		Object.values(this.profileTimeouts).forEach(clearTimeout);
		this.profiles = {};
		this.profileTimeouts = {};
	}

	handleProfileInvalidate(userId: string, guildId?: string): void {
		const targetGuildId = guildId ?? ME;
		this.clearProfileTimeout(userId, targetGuildId);
		const userProfiles = this.profiles[userId];
		if (!userProfiles) return;
		const {[targetGuildId]: _, ...remainingGuildProfiles} = userProfiles;
		if (Object.keys(remainingGuildProfiles).length === 0) {
			const {[userId]: __, ...remainingProfiles} = this.profiles;
			this.profiles = remainingProfiles;
		} else {
			this.profiles = {
				...this.profiles,
				[userId]: remainingGuildProfiles,
			};
		}
	}

	handleProfileCreate(profile: Profile): void {
		if (!profile?.userId) {
			this.logger.warn('Attempted to set invalid profile:', profile);
			return;
		}
		const guildId = profile.guildId ?? ME;
		this.profiles = {
			...this.profiles,
			[profile.userId]: {
				...(this.profiles[profile.userId] ?? {}),
				[guildId]: profile,
			},
		};
		this.setProfileTimeout(profile.userId, guildId);
	}

	handleProfilesClear(): void {
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) {
			this.logger.warn('Attempted to clear profiles without valid user ID');
			return;
		}
		const currentUserTimeouts = Object.entries(this.profileTimeouts).filter(([key]) =>
			key.startsWith(`${currentUserId}:`),
		);
		for (const [_, timeout] of currentUserTimeouts) {
			clearTimeout(timeout);
		}
		const updatedTimeouts = Object.fromEntries(
			Object.entries(this.profileTimeouts).filter(([key]) => !key.startsWith(`${currentUserId}:`)),
		);
		const {[currentUserId]: _, ...remainingProfiles} = this.profiles;
		this.profiles = remainingProfiles;
		this.profileTimeouts = updatedTimeouts;
	}

	private createTimeoutKey(userId: string, guildId: string): string {
		return `${userId}:${guildId}`;
	}

	private clearProfileTimeout(userId: string, guildId: string): void {
		const timeoutKey = this.createTimeoutKey(userId, guildId);
		const existingTimeout = this.profileTimeouts[timeoutKey];
		if (existingTimeout) {
			clearTimeout(existingTimeout);
			const {[timeoutKey]: _, ...remainingTimeouts} = this.profileTimeouts;
			this.profileTimeouts = remainingTimeouts;
		}
	}

	private setProfileTimeout(userId: string, guildId: string): void {
		const timeoutKey = this.createTimeoutKey(userId, guildId);
		this.clearProfileTimeout(userId, guildId);
		const timeout = setTimeout(() => {
			runInAction(() => {
				const userProfiles = this.profiles[userId];
				if (!userProfiles) {
					const {[timeoutKey]: _, ...remainingTimeouts} = this.profileTimeouts;
					this.profileTimeouts = remainingTimeouts;
					return;
				}
				const {[guildId]: _, ...remainingGuildProfiles} = userProfiles;
				if (Object.keys(remainingGuildProfiles).length === 0) {
					const {[userId]: __, ...remainingProfiles} = this.profiles;
					this.profiles = remainingProfiles;
				} else {
					this.profiles = {
						...this.profiles,
						[userId]: remainingGuildProfiles,
					};
				}
				const {[timeoutKey]: ___, ...remainingTimeouts} = this.profileTimeouts;
				this.profileTimeouts = remainingTimeouts;
			});
		}, PROFILE_TIMEOUT_MS);
		this.profileTimeouts = {
			...this.profileTimeouts,
			[timeoutKey]: timeout,
		};
	}
}

export default new UserProfile();
