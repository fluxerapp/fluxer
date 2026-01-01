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

import {makeAutoObservable, reaction} from 'mobx';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {type StatusType, StatusTypes} from '~/Constants';
import AccountManager from '~/stores/AccountManager';
import UserSettingsStore from '~/stores/UserSettingsStore';

class StatusExpiryStore {
	activeUserId: string | null = null;
	private timer: ReturnType<typeof setTimeout> | null = null;

	constructor() {
		makeAutoObservable(this);

		reaction(
			() => AccountManager.currentUserId,
			(userId) => {
				this.handleActiveUserChange(userId);
			},
			{fireImmediately: true},
		);

		reaction(
			() => ({
				statusResetsAt: UserSettingsStore.statusResetsAt,
				statusResetsTo: UserSettingsStore.statusResetsTo,
				status: UserSettingsStore.status,
			}),
			() => {
				this.scheduleTimer();
			},
		);
	}

	get activeExpiresAt(): number | null {
		const resetsAt = UserSettingsStore.statusResetsAt;
		if (!resetsAt) {
			return null;
		}
		return new Date(resetsAt).getTime();
	}

	get activeFallbackStatus(): StatusType {
		const resetsTo = UserSettingsStore.statusResetsTo;
		if (!resetsTo) {
			return StatusTypes.ONLINE;
		}
		switch (resetsTo) {
			case 'online':
				return StatusTypes.ONLINE;
			case 'idle':
				return StatusTypes.IDLE;
			case 'dnd':
				return StatusTypes.DND;
			case 'invisible':
				return StatusTypes.INVISIBLE;
			default:
				return StatusTypes.ONLINE;
		}
	}

	setActiveStatusExpiry({
		status,
		durationMs,
		fallbackStatus = StatusTypes.ONLINE,
	}: {
		status: StatusType;
		durationMs: number | null;
		fallbackStatus?: StatusType;
	}): void {
		if (durationMs === null) {
			this.clearStatusExpiry();
			UserSettingsActionCreators.update({
				status,
				statusResetsAt: null,
				statusResetsTo: null,
			}).catch((error) => {
				console.error('Failed to update status:', error);
			});
			return;
		}

		const expiresAt = new Date(Date.now() + durationMs);

		UserSettingsActionCreators.update({
			status,
			statusResetsAt: expiresAt.toISOString(),
			statusResetsTo: fallbackStatus,
		}).catch((error) => {
			console.error('Failed to update status with expiry:', error);
		});
	}

	clearStatusExpiry(): void {
		if (this.timer) {
			clearTimeout(this.timer);
			this.timer = null;
		}
	}

	handleActiveUserChange(userId: string | null): void {
		this.activeUserId = userId;
		this.scheduleTimer();
	}

	private scheduleTimer(): void {
		if (this.timer) {
			clearTimeout(this.timer);
			this.timer = null;
		}

		const expiresAt = this.activeExpiresAt;
		if (!expiresAt) {
			return;
		}

		const delay = expiresAt - Date.now();
		if (delay <= 0) {
			void this.handleExpiry();
			return;
		}

		this.timer = setTimeout(() => {
			void this.handleExpiry();
		}, delay);
	}

	private async handleExpiry(): Promise<void> {
		const fallbackStatus = this.activeFallbackStatus;

		try {
			await UserSettingsActionCreators.update({
				status: fallbackStatus,
				statusResetsAt: null,
				statusResetsTo: null,
			});
		} catch (error) {
			console.error('Failed to revert status after expiry:', error);
		}
	}
}

export default new StatusExpiryStore();
