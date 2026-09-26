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
		this.clearAllProfiles();
	}

	handleProfileInvalidate(userId: string, guildId?: string): void {
		const targetGuildId = guildId ?? ME;
		this.clearProfileTimeout(userId, targetGuildId);
		this.removeProfile(userId, targetGuildId);
	}

	handleGuildMemberAdd(userId: string): void {
		this.clearUserProfiles(userId);
	}

	handleGuildMemberRemove(userId: string): void {
		this.clearUserProfiles(userId);
	}

	handleGuildCreate(): void {
		this.clearAllProfiles();
	}

	handleGuildDelete(unavailable?: boolean): void {
		if (unavailable) return;
		this.clearAllProfiles();
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

	private clearAllProfiles(): void {
		Object.values(this.profileTimeouts).forEach(clearTimeout);
		this.profiles = {};
		this.profileTimeouts = {};
	}

	private clearUserProfiles(userId: string): void {
		const userProfiles = this.profiles[userId];
		if (!userProfiles) return;
		for (const guildId of Object.keys(userProfiles)) {
			this.clearProfileTimeout(userId, guildId);
		}
		const {[userId]: _, ...remainingProfiles} = this.profiles;
		this.profiles = remainingProfiles;
	}

	private removeProfile(userId: string, guildId: string): void {
		const userProfiles = this.profiles[userId];
		if (!userProfiles) return;
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
				this.removeProfile(userId, guildId);
				const {[timeoutKey]: _, ...remainingTimeouts} = this.profileTimeouts;
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
