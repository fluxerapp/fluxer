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

import {makeAutoObservable, runInAction} from 'mobx';
import {ME} from '~/Constants';
import type {ProfileRecord} from '~/records/ProfileRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';

type ProfilesByGuildId = Record<string, ProfileRecord>;

const PROFILE_TIMEOUT_MS = 60_000;

class UserProfileStore {
	profiles: Record<string, ProfilesByGuildId> = {};
	profileTimeouts: Record<string, NodeJS.Timeout> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getProfile(userId: string, guildId?: string): ProfileRecord | null {
		return this.profiles[userId]?.[guildId ?? ME] ?? null;
	}

	handleConnectionOpen(): void {
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

	handleProfileCreate(profile: ProfileRecord): void {
		if (!profile?.userId) {
			console.warn('Attempted to set invalid profile:', profile);
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
		const currentUserId = AuthenticationStore.currentUserId;
		if (!currentUserId) {
			console.warn('Attempted to clear profiles without valid user ID');
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

export default new UserProfileStore();
