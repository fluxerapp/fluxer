// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {onLocaleChange} from '@app/features/i18n/utils/LocaleChangeListener';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
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
	private retained = new Map<string, {count: number; expired: boolean}>();

	constructor() {
		makeAutoObservable<this, 'retained'>(this, {retained: false}, {autoBind: true});
		onLocaleChange(() => this.clearAllProfiles());
	}

	retainProfile(userId: string, guildId?: string): () => void {
		const targetGuildId = guildId ?? ME;
		const key = this.createTimeoutKey(userId, targetGuildId);
		const lease = this.retained.get(key) ?? {count: 0, expired: false};
		lease.count += 1;
		this.retained.set(key, lease);
		return () => {
			if (this.retained.get(key) !== lease) return;
			lease.count -= 1;
			if (lease.count > 0) return;
			this.retained.delete(key);
			if (lease.expired) runInAction(() => this.removeProfile(userId, targetGuildId));
		};
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
		this.retained.clear();
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
		const retained = this.retained.get(timeoutKey);
		if (retained !== undefined) retained.expired = false;
		const timeout = setTimeout(() => {
			runInAction(() => {
				const lease = this.retained.get(timeoutKey);
				if (lease === undefined) this.removeProfile(userId, guildId);
				else lease.expired = true;
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

const userProfile = new UserProfile();

AccountScopedWork.registerCancellation(() => userProfile.handleGatewayReady());

export default userProfile;
